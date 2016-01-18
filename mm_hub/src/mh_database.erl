%%% -------------------------------------------------------------------
%%% Author  : anton
%%% Description :
%%%
%%% Created : 22 Jun 2008
%%% -------------------------------------------------------------------
-module(mh_database).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-define(SAVE_CHECK_TIMEOUT, 30000).

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0, 
         load_checks/0, load_check/1,
         load_check_ids/0, load_check_id/1, 
         load_notifications/0, load_notification/1,
         save_check_result/3, update_check_state/2,
         update_notification_state/2,
         get_destination/1, get_sms_credits/1, deduct_sms_credits/1,
         log_sms/5
         ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {facets}).

%% ====================================================================
%% External functions
%% ====================================================================

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

load_checks() -> gen_server:call(?MODULE, {load_checks}).

load_check(CheckId) -> gen_server:call(?MODULE, {load_check, CheckId}).

load_check_ids() -> gen_server:call(?MODULE, {load_check_ids}).

load_check_id(CheckId) -> gen_server:call(?MODULE, {load_check_id, CheckId}).

load_notifications() -> gen_server:call(?MODULE, {load_notifications}).

load_notification(CheckId) -> gen_server:call(?MODULE, {load_notification, CheckId}).

save_check_result(Check, Node, CheckResult) ->
    gen_server:call(?MODULE, {save_check_result, Check, Node, CheckResult}, infinity).

update_check_state(CheckId, NewState) ->
    gen_server:call(?MODULE, {update_check_state, CheckId, NewState}).

update_notification_state(NotificationId, NewState) ->
    gen_server:call(?MODULE, {update_notification_state, NotificationId, NewState}).

get_destination(DestinationId) ->
    gen_server:call(?MODULE, {get_destination, DestinationId}).

get_sms_credits(UserId) ->
    gen_server:call(?MODULE, {get_sms_credits, UserId}).

deduct_sms_credits(UserId) ->
    gen_server:call(?MODULE, {deduct_sms_credits, UserId}).

log_sms(UserId, Message, Phone, Reference, MessageId) ->
    gen_server:call(?MODULE, {log_sms, UserId, Message, Phone, Reference, MessageId}).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    connect(),
    prepare_statements(),
    Facets = load_facets(),
%%    io:format("facet: ~p~n", [Facets]),
    {ok, #state{facets=Facets}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call({load_checks}, _From, State) ->
    Reply = {ok, do_load_checks()},
    {reply, Reply, State};

handle_call({load_check, CheckId}, _From, State) ->
    Reply = {ok,do_load_check(CheckId)},
    {reply, Reply, State};

handle_call({load_check_ids}, _From, State) ->
    Reply = {ok, do_load_check_ids()},
    {reply, Reply, State};

handle_call({load_check_id, CheckId}, _From, State) ->
    Reply = {ok, do_load_check_id(CheckId)},
    {reply, Reply, State};

handle_call({load_notifications}, _From, State) ->
    Reply = {ok, do_load_notifications()},
    {reply, Reply, State};

handle_call({load_notification, CheckId}, _From, State) ->
    Reply = {ok, do_load_notification(CheckId)},
    {reply, Reply, State};

handle_call({update_check_state, CheckId, NewState}, _From, State) ->
    mysql:execute(p1, update_monitor_state, [encode_monitor_state(NewState), CheckId]),
    {reply, ok, State};

handle_call({update_notification_state, NotificationId, NewState}, _From, State) ->
    mysql:execute(p1, update_notification_state, [encode_notification_state(NewState), NotificationId]),
    {reply, ok, State};

handle_call({save_check_result, Check, Node, CheckResult}, _From, State) ->
	 CheckId = proplists:get_value(check_id, Check),
     Kind = proplists:get_value(kind, Check),
     NodeId = encode_node_id(Node),
     Checked = get_unix_timestamp(proplists:get_value(checked, CheckResult)),
     Facets = dict:fetch(Kind, State#state.facets),
     case proplists:get_value(status, CheckResult) of
        success ->
            save_check_success(CheckId, NodeId, Checked, Facets, CheckResult);
        failure ->
            Reason = encode_failure_reason(proplists:get_value(reason, CheckResult)),
            save_check_error(CheckId, NodeId, Checked, Facets, Reason);
        crash ->
            ok
     end,
    {reply, ok, State};

handle_call({get_destination, DestinationId}, _From, State) ->
    Destination = do_get_destination(DestinationId),
    Reply = case Destination of
                [H|_] -> 
                    {ok, H};
                [] -> 
                    {failed, notfound}
            end,
    {reply, Reply, State};

handle_call({get_sms_credits, UserId}, _From, State) ->
    {SmsPlan, SmsPurchased} = do_get_sms_credits(UserId),
    {reply, SmsPlan + SmsPurchased, State};

handle_call({deduct_sms_credits, UserId}, _From, State) ->
    do_deduct_sms_credits(UserId),
    {reply, ok, State};

handle_call({log_sms, UserId, Message, Phone, Reference, MessageId}, _From, State) ->
    mysql:execute(p1, insert_sms_log, [UserId, Message, Phone, Reference, MessageId, iso_8601_fmt(erlang:now())], ?SAVE_CHECK_TIMEOUT),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
connect() ->
    mysql:start_link(p1, "localhost", undefined, "monimi", "monimi", "monimi", fun(_, _, _, _) -> ok end),
    mysql:connect(p1, "localhost", undefined, "monimi", "monimi", "monimi", 'utf8', true).
    
prepare_statements() ->
    mysql:prepare(select_checks, <<"SELECT monitor_id, kind, config, beat FROM monitor WHERE state IN (0,1,2)">>),
    mysql:prepare(select_check, <<"SELECT kind, config, beat FROM monitor WHERE state IN (0,1,2) AND monitor_id = ?">>),
    mysql:prepare(select_check_ids, <<"SELECT monitor_id FROM monitor WHERE state IN (0,1,2)">>),
    mysql:prepare(select_check_id, <<"SELECT monitor_id FROM monitor WHERE state IN (0,1,2) AND monitor_id = ?">>),
    mysql:prepare(select_notifications,
    	<<"SELECT" 
       		" monitor.monitor_id,"
       		" alert_destination.alert_destination_id,"
       		" monitor.name,"
       		" alert_destination.destination_id,"
       		" alert_destination.alert_destination_state,"
       		" alert_destination.notify_down_on,"
			" alert_destination.notify_up,"
			" alert_destination.qp_start,"
			" alert_destination.qp_end"
		" FROM monitor" 
    		" INNER JOIN alert_destination ON alert_destination.monitor_id = monitor.monitor_id"
		" WHERE monitor.state IN (0,1,2) AND monitor.user_state = 0">>),
    mysql:prepare(select_notification,
        <<"SELECT" 
       			" alert_destination.alert_destination_id,"
       			" monitor.name,"
       			" alert_destination.destination_id,"
       		    " alert_destination.alert_destination_state,"
       			" alert_destination.notify_down_on,"
       			" alert_destination.notify_up,"
			    " alert_destination.qp_start,"
			    " alert_destination.qp_end"
			" FROM monitor"
    			" INNER JOIN alert_destination ON alert_destination.monitor_id = monitor.monitor_id"
			" WHERE monitor.state IN (0,1,2) AND monitor.user_state = 0 AND monitor.monitor_id = ?">>),
    mysql:prepare(select_destination,
		  <<"SELECT destination_type, user_id, address FROM destination WHERE destination_id = ?">>),    
    mysql:prepare(update_monitor_state,
		  <<"UPDATE monitor SET state = ? WHERE monitor_id = ?">>),
    mysql:prepare(update_notification_state,
		  <<"UPDATE alert_destination SET alert_destination_state = ? WHERE alert_destination_id = ?">>),

    mysql:prepare(insert_monitor_log,
		  <<"INSERT INTO monitor_log (monitor_id, node, ts, facet_id, value) VALUES(?,?,?,?,?)">>),
    mysql:prepare(insert_error_log,
		  <<"INSERT INTO error_log (monitor_id, node, ts, error) VALUES(?,?,?,?)">>),

    mysql:prepare(insert_sms_log,
		  <<"INSERT INTO sms (user_id, message, phone, reference, msg_id, time_stamp) VALUES(?,?,?,?,?,?)">>),
	mysql:prepare(select_sms_credits,
		<<"SELECT sms_credits_plan, sms_credits_purchased FROM user WHERE user_id = ?">>),
	mysql:prepare(deduct_sms_credits_plan,
		<<"UPDATE user SET sms_credits_plan = sms_credits_plan - 1 WHERE user_id = ?">>),
	mysql:prepare(deduct_sms_credits_purchased,
		<<"UPDATE user SET sms_credits_purchased = sms_credits_purchased - 1 WHERE user_id = ?">>).

load_facets() -> 
	{data, MysqlResult} = mysql:fetch(p1, "SELECT facet_id, kind, name FROM facet"),
	Facets = [{FacetId, decode_kind(Kind), decode_facet(Name)} || [FacetId, Kind, Name] <- mysql:get_result_rows(MysqlResult)],
    Dict1 = lists:foldl(fun({_FacetId, Kind, _Name}, Dict) ->
                                case dict:is_key(Kind, Dict) of
                                    true ->
                                        Dict;
                                    _ ->
                                        dict:store(Kind, [], Dict)
                                end
                        end, dict:new(), Facets),
    lists:foldl(fun({FacetId, Kind, Name}, Dict) ->
                        List = dict:fetch(Kind, Dict),
                        List1 = [{Name, FacetId} | List],
                        dict:store(Kind, List1, Dict)
                end, Dict1, Facets).
 
do_load_checks() ->
	{data, MysqlResult} = mysql:execute(p1, select_checks, []),
	[[{check_id, CheckId}, {kind, decode_kind(CheckKind)}, {beat, Beat}, {config, decode_config(decode_kind(CheckKind), Config)}] || [CheckId, CheckKind, Config, Beat] <- mysql:get_result_rows(MysqlResult)].

do_load_check(CheckId) ->
	{data, MysqlResult} = mysql:execute(p1, select_check, [CheckId]),
	[[{check_id, CheckId}, {kind, decode_kind(CheckKind)}, {beat, Beat}, {config, decode_config(decode_kind(CheckKind), Config)}] || [CheckKind, Config, Beat] <- mysql:get_result_rows(MysqlResult)].    
	
do_load_check_ids() ->
	{data, MysqlResult} = mysql:execute(p1, select_check_ids, []),
	[CheckId || [CheckId] <- mysql:get_result_rows(MysqlResult)].

do_load_check_id(CheckId) ->
	{data, MysqlResult} = mysql:execute(p1, select_check_id, [CheckId]),
	[Id || [Id] <- mysql:get_result_rows(MysqlResult)].

do_load_notifications() ->
	{data, MysqlResult} = mysql:execute(p1, select_notifications, []),
	[{CheckId, NotificationId, {CheckName, DestinationId, decode_notification_state(State), NotifyDownOn, decode_notify_up(NotifyUp), QpStart, QpEnd}} 
		|| [CheckId, NotificationId, CheckName, DestinationId, State, NotifyDownOn, NotifyUp, QpStart, QpEnd] <- mysql:get_result_rows(MysqlResult)].

do_load_notification(CheckId) ->
	{data, MysqlResult} = mysql:execute(p1, select_notification, [CheckId]),
	[{NotificationId, {CheckName, DestinationId, decode_notification_state(State), NotifyDownOn, decode_notify_up(NotifyUp), QpStart, QpEnd}} 
		|| [NotificationId, CheckName, DestinationId, State, NotifyDownOn, NotifyUp, QpStart, QpEnd] <- mysql:get_result_rows(MysqlResult)].

do_get_destination(DestinationId) ->
	{data, MysqlResult} = mysql:execute(p1, select_destination, [DestinationId]),
	[{decode_destination_type(DestinationType), UserId, Address} || [DestinationType, UserId, Address] <- mysql:get_result_rows(MysqlResult)].

do_get_sms_credits(UserId) ->
	{data, MysqlResult} = mysql:execute(p1, select_sms_credits, [UserId]),
	[[SmsPlan, SmsPurchased]] = mysql:get_result_rows(MysqlResult),
	{SmsPlan, SmsPurchased}.

do_deduct_sms_credits(UserId) ->
    {SmsPlan, _SmsPurchased} = do_get_sms_credits(UserId),
	case SmsPlan > 0 of
		true ->
			mysql:execute(p1, deduct_sms_credits_plan, [UserId]);
        false ->
			mysql:execute(p1, deduct_sms_credits_purchased, [UserId])
	end.

encode_failure_reason({invalid_http_code, Code}) ->
	"invalid_http_code_" ++ integer_to_list(Code);
encode_failure_reason({Reason, Subreason}) ->
	atom_to_list(Reason) ++ " " ++ atom_to_list(Subreason);
encode_failure_reason(Reason) ->
	Reason.

encode_node_id(Node) ->
    case Node of 
    	monitor1@localhost.monimi.net -> 1; % these three for dev setup
        monitor2@localhost.monimi.net -> 2;
        monitor3@localhost.monimi.net -> 3;
        monitor@monitor1.monimi.net -> 1;
        monitor@monitor2.monimi.net -> 2;
        monitor@monitor3.monimi.net -> 3
    end.

encode_monitor_state(State) ->
    case State of 
    	unknown -> 0;
    	up -> 1;
    	down -> 2;
        disabled -> 3;
        error -> 4
    end.

decode_notify_up(Encoded) ->
    case Encoded of 
    	0 -> false;
    	1 -> true
    end.

decode_destination_type(DestinationType) ->
    case DestinationType of 
    	0 -> sms;
    	1 -> email
    end.

encode_notification_state(NotificationState) ->
    case NotificationState of 
    	normal -> 0;
    	notified_down -> 1;
    	new -> 2
    end.

decode_notification_state(NotificationState) ->
    case NotificationState of 
    	0 -> normal;
    	1 -> notified_down;
    	2 -> new
    end.

decode_kind(Kind) ->
    case Kind of 
    	0 -> http_check;
    	1 -> https_check;
    	2 -> ping_check;
		3 -> dns_check
    end.

decode_facet(undefined) -> undefined;
decode_facet(FacetName) -> list_to_atom(binary_to_list(FacetName)).


decode_config(http_check, Config) ->
	[{url, binary_to_list(Config)}];

decode_config(https_check, Config) ->
	[{url, binary_to_list(Config)}];

decode_config(ping_check, Config) ->
	[{host, binary_to_list(Config)}];

decode_config(dns_check, Config) ->
	decode_dns_config(mochijson:decode(Config)).

decode_dns_config({array, Array}) ->
	[{list_to_atom(Name), Value} || {struct, [{Name, Value}]} <- Array].

iso_8601_fmt(Now) ->
    DateTime = calendar:now_to_universal_time(Now),
    {{Year,Month,Day},{Hour,Min,Sec}} = DateTime,
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
        [Year, Month, Day, Hour, Min, Sec]).

get_unix_timestamp(Now) ->
    calendar:datetime_to_gregorian_seconds( calendar:now_to_universal_time(Now) ) -
            calendar:datetime_to_gregorian_seconds( {{1970,1,1},{0,0,0}} ).

save_check_success(CheckId, NodeId, Checked, Facets, CheckResult) ->
    lists:foreach(fun({Name, FacetId}) ->
                          case Name of
                              undefined ->
                                  Value  = 1;
                              _ ->
                                  Value = proplists:get_value(Name, CheckResult)
                          end,
                          mysql:execute(p1, insert_monitor_log, [CheckId, NodeId, Checked, FacetId, Value], ?SAVE_CHECK_TIMEOUT)
                  end, Facets).

save_check_error(CheckId, NodeId, Checked, Facets, Reason) ->
    mysql:execute(p1, insert_error_log, [CheckId, NodeId, Checked, Reason], ?SAVE_CHECK_TIMEOUT),
    lists:foreach(fun({_Name, FacetId}) ->
                          mysql:execute(p1, insert_monitor_log, [CheckId, NodeId, Checked, FacetId, undefined], ?SAVE_CHECK_TIMEOUT)
                  end, Facets).
