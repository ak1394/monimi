%%% -------------------------------------------------------------------
%%% Author  : anton
%%% Description :
%%%
%%% Created : 24 Jun 2008
%%% -------------------------------------------------------------------
-module(mh_check_manager).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start_link/1, process_result/3, touch/1, get_check_fsm/1, checks_count/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {monitors, nodes, ids}).

%% ====================================================================
%% External functions
%% ====================================================================

start_link(Nodes) -> gen_server:start_link({local, ?MODULE}, ?MODULE, [Nodes], []).

process_result(Node, CheckId, CheckResult) ->  
    gen_server:call(?MODULE, {process_result, Node, CheckId, CheckResult}).

touch(CheckId) ->  
    gen_server:call(?MODULE, {touch, CheckId}).

checks_count() ->  
    gen_server:call(?MODULE, checks_count).

% for debugging purposes
get_check_fsm(CheckId) ->  
    gen_server:call(?MODULE, {get_check_fsm, CheckId}).

%% ====================================================================
%% Local functions
%% ====================================================================

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
init([Nodes]) ->
	{ok, Checks} = mh_database:load_checks(),
	MonitorsTable = ets:new(monitors, [named_table]),
	IdTable = ets:new(ids, [named_table]),
	create_monitors(Nodes, Checks, MonitorsTable, IdTable),
	ets:foldl(fun({Key, Ref}, Acc) ->
					  mm_monitor:run(Ref)
			  end, [], MonitorsTable),
    {ok, #state{monitors=MonitorsTable, ids=IdTable, nodes=Nodes}}.

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
%% handle_call({process_result, Node, CheckId, CheckResult}, _From, State) ->
%%     case dict:is_key(CheckId, State#state.checks) of
%%         true ->
%%     		CheckFsm = dict:fetch(CheckId, State#state.checks),
%%             mh_check_fsm:process_result(CheckFsm, CheckResult, Node);
%%         Else ->
%%             Else
%%     end,
%%     {reply, ok, State};

handle_call({touch, CheckId}, _From, State) ->
	IdTable = State#state.ids,    
    {ok, Result} = mh_database:load_check(CheckId),
%%     % check found in db
%%     case Result of
%%         [{CheckId, Check}] ->
%%             case dict:is_key(CheckId, Checks) of
%%                 % check is already loaded
%%                 true ->
%%                     NewChecks = restart_fsm(Checks, State#state.nodes, CheckId, Check),
%%                     Reply = restarted;
%%                 % new check
%%                 false ->
%%                     NewChecks = start_fsm(Checks, State#state.nodes, CheckId, Check),
%%                     Reply = added
%%             end;
%%         % check is not found in db    
%%         [] ->
%%     		case dict:is_key(CheckId, Checks) of
%%                 true ->
%% 					NewChecks = stop_fsm(Checks, State#state.nodes, CheckId),
%%             		Reply = removed;
%%                 false ->
%%                     NewChecks = Checks,
%% 					Reply = not_found
%%             end
%%     end,
    {reply, ok, State};

handle_call(checks_count, _From, State) ->
	Size = proplists:get_value(size, ets:info(State#state.ids)),
	{reply, Size, State};

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

create_monitors(Nodes, Checks, MonitorsTable, IdTable) ->
	lists:foreach(fun(Check) ->
						  Key = check_key(Check),
						  Id = proplists:get_value(check_id, Check),
						  case ets:lookup(MonitorsTable, Key) of
							  [] ->
								  {ok, Ref} = mm_monitor:start_link(Nodes),
								  mm_monitor:add_check(Ref, Check),
								  ets:insert(MonitorsTable, {Key, Ref}),
								  ets:insert(IdTable, {Id, Ref});
							  [{_Key, Ref}] ->
								  mm_monitor:add_check(Ref, Check),
								  ets:insert(IdTable, {Id, Ref})
						  end
				  end, Checks).

%% TODO can't use plain Config as a key, because of varying beats
check_key(Check) ->
	case proplists:get_value(kind, Check) of
		http_check ->
			{http_check, proplists:get_value(config, Check)};
		https_check ->
			{https_check, proplists:get_value(config, Check)};
		dns_check ->
			{dns_check, proplists:get_value(config, Check)};
		ping_check ->
			{ping_check, proplists:get_value(config, Check)};
		ssh_check ->
			{ssh_check, proplists:get_value(config, Check)}
	end.
		
%% start_fsm(Checks, Nodes, CheckId, Check) ->
%% 	publish_check(Nodes, CheckId),
%% 	% start fsm
%% 	{ok, Pid} = mh_check_fsm:start_link(CheckId, Check, Nodes),
%% 	% save check pid
%% 	dict:store(CheckId, Pid, Checks).
%% 
%% stop_fsm(Checks, Nodes, CheckId) ->
%% 	CheckFsm = dict:fetch(CheckId, Checks),
%% 	NewChecks = dict:erase(CheckId, Checks),
%% 	unlink(CheckFsm),
%% 	mh_check_fsm:stop(CheckFsm),
%% 	% remove check from all nodes
%% 	remove_check(Nodes, CheckId),
%%     NewChecks.
%% 
%% restart_fsm(Checks, Nodes, CheckId, Check) ->
%%     NewChecks = stop_fsm(Checks, Nodes, CheckId),
%%     start_fsm(NewChecks, Nodes, CheckId, Check).
%% 
%% publish_check(Nodes, CheckId) ->
%% 	lists:foreach(fun(Node) ->
%%                           mh_monitor:add_check(Node, CheckId)
%%                   end, Nodes),
%%     ok.
%% 
%% remove_check(Nodes, CheckId) ->
%% 	lists:foreach(fun(Node) ->
%%                           mh_monitor:remove_check(Node, CheckId)
%%                   end, Nodes),
%%     ok.
