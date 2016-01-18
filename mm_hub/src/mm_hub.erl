%%% -------------------------------------------------------------------
%%% Author  : anton
%%% Description :
%%%
%%% Created : 21 Jun 2008
%%% -------------------------------------------------------------------
-module(mm_hub).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start/0, start_link/0, incoming_im/3, nodes/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {monitor_nodes}).

%% ====================================================================
%% External functions
%% ====================================================================

%% @spec start() -> ok
%% @doc Start the xxx server.
start() ->
    ensure_started(crypto),
    ensure_started(inets),
    ensure_started(log4erl),
    ensure_started(clickatell),
    application:start(mm_hub, permanent).

start_link() -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

nodes() ->  
    gen_server:call(?MODULE, nodes).

incoming_im(User, Server, Message) ->
    mm_status:im(Message).

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
	log4erl:add_file_appender(file, {"log/", "hub", {size, 100000}, 10, "log", debug}),
   	log4erl:change_format(file, "%j %T [%L] %l%n"),
	{A, B, C} = erlang:now(),
	random:seed(A, B, C),
    mm_rate_limit:start_link(),
    mh_destination:start_link(),
    mh_notification_manager:start_link(),
    {ok, MonitorNodes} = application:get_env(mm_hub, monitor_nodes),
	mm_nodes:start_link(MonitorNodes),
    mh_check_manager:start_link(MonitorNodes),
    application:set_env(mm_hub, hub_started, erlang:now()),
    {ok, #state{monitor_nodes=MonitorNodes}}.

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
handle_call(nodes, _From, State) ->
    Reply = State#state.monitor_nodes,
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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

handle_info({_From, touch_check, CheckId}, State) ->
	log4erl:log(info, "Touch check ~p", [CheckId]),
    mh_notification_manager:touch(CheckId),
    mh_check_manager:touch(CheckId),
    {noreply, State};

handle_info({From, send_confirmation, Phone, Confirmation}, State) ->
	log4erl:log(info, "Sending confirmation code to ~p: ~p", [Phone, Confirmation]),
    %%SmsId = mh_sms:send_confirmation(Phone, Confirmation),
	SmsId = {ok, "1"},
    From ! SmsId,
    {noreply, State};

handle_info({_From, remind_password, Email, Password}, State) ->
	log4erl:log(info, "Remind password: ~p ~p", [Email, Password]),
    mh_email:remind_password(Email, Password),
    {noreply, State};

handle_info({_From, send_welcome, Email, Password}, State) ->
	log4erl:log(info, "Welcome new user: ~p ~p", [Email, Password]),
    mh_email:send_welcome(Email, Password),
    {noreply, State};

handle_info({From, sms_status, SmsId}, State) ->
	log4erl:log(info, "Sms status check ~p ~p", [From, SmsId]),
    Result = clickatell:status(SmsId),
    case Result of
        {error, _ } = Error ->
            From ! Error;
        Status ->
            From ! {ok, Status}
    end,
    {noreply, State};

handle_info({From, get_nameservers, Name}, State) ->
	log4erl:log(info, "Get nameservers for name ~p ~p", [From, Name]),
	NodesUp = [Node || Node <- State#state.monitor_nodes, mh_monitor:status(Node) == node_up],
	case length(NodesUp) of
		0 ->
			From ! {error, no_nodes_up};
		Len ->
			SelectedNode = lists:nth(random:uniform(Len), NodesUp),
			case rpc:call(SelectedNode, mm_monitor, get_nameservers, [Name]) of
				{ok, Name1, Nameservers} ->
					From ! {ok, {Name1, Nameservers}};
				{error, Reason} ->
					From ! {error, Reason};
				_ ->
					From ! {error, unable_to_lookup_nameservers}
			end
	end,
    {noreply, State};

handle_info(Info, State) ->
	log4erl:log(warn, "mm_hub, unexpected message: ~p", [Info]),
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

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
