%%% -------------------------------------------------------------------
%%% Author  : anton
%%% Description :
%%%
%%% Created : 22 Jun 2008
%%% -------------------------------------------------------------------
-module(mh_monitor).

-behaviour(gen_fsm).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define(CHECK_NODE_INTERVAL, 60000).
-define(POLL_RESULTS_INTERVAL, 1000).

%% --------------------------------------------------------------------
%% External exports
-export([start_link/1, add_check/2, check/2, remove_check/2, status/1]).

%% gen_fsm callbacks
-export([node_up/2, node_down/2]).

-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(state, {node}).

%% ====================================================================
%% External functions
%% ====================================================================

start_link(Node) ->
    gen_fsm:start_link({local, Node}, ?MODULE, [Node], []).

check(FsmRef, CheckId) ->
    gen_fsm:send_event(FsmRef, {check, CheckId}).

add_check(FsmRef, CheckId) ->
    gen_fsm:send_event(FsmRef, {add_check, CheckId}).

remove_check(FsmRef, CheckId) ->
    gen_fsm:send_event(FsmRef, {remove_check, CheckId}).

status(FsmRef) ->
  gen_fsm:sync_send_all_state_event(FsmRef, status).

%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}
%% --------------------------------------------------------------------
init([Node]) ->
    gen_fsm:start_timer(1000, check_node),
    {ok, node_down, #state{node=Node}}.    

%% --------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
node_down({timeout, _Ref, check_node}, StateData) ->
    Node = StateData#state.node,
    case net_adm:ping(Node) of
        pong ->
			log4erl:log(warn, "Node ~p is up", [Node]),
            %% initialize remote monitor node
			erlang:monitor_node(Node, true),
            {ok, Checks} = mh_database:load_checks(),
            rpc:call(Node, mm_monitor, remove_all, []),
            rpc:call(Node, mm_monitor, add_all, [Checks]),
            gen_fsm:start_timer(?POLL_RESULTS_INTERVAL, poll_results),
			{next_state, node_up, StateData};
        pang ->
			log4erl:log(warn, "Node ~p is down", [Node]),
    		gen_fsm:start_timer(?CHECK_NODE_INTERVAL, check_node),
			{next_state, node_down, StateData}
	end;

node_down(_Event, StateData) ->
    {next_state, node_down, StateData}.

node_up({add_check, CheckId}, StateData) ->
	{ok, [{_CheckId, Check}]} = mh_database:load_check(CheckId),
	Node = StateData#state.node,
    rpc:call(Node, mm_monitor, add, [CheckId, Check]),
    {next_state, node_up, StateData};

node_up({check, CheckId}, StateData) ->
	Node = StateData#state.node,
	rpc:cast(Node, mm_monitor, check, [CheckId]),
    {next_state, node_up, StateData};

node_up({remove_check, CheckId}, StateData) ->
	Node = StateData#state.node,
    rpc:call(Node, mm_monitor, remove, [CheckId]),
    {next_state, node_up, StateData};

node_up(nodedown, StateData) ->
    gen_fsm:start_timer(?CHECK_NODE_INTERVAL, check_node),
    {next_state, node_down, StateData};

node_up({timeout, _Ref, poll_results}, StateData) ->
	Node = StateData#state.node,
	Result = rpc:call(Node, mm_monitor, collect_results, []),
    case Result of
    	{badrpc, Reason} ->
			log4erl:log(info, "RPC call to node ~p has failed, reason: ~p", [Node, Reason]),
            {next_state, node_down, StateData};
        {ok, CheckResults} ->
            %% good result save to database and pass to mh_check_manager
    		lists:foreach(fun({CheckId, CheckResult}) ->
                          mh_database:save_check_result(Node, CheckId, CheckResult),
                          mh_check_manager:process_result(Node, CheckId, CheckResult)
                  end, CheckResults),
            gen_fsm:start_timer(?POLL_RESULTS_INTERVAL, poll_results),
            {next_state, node_up, StateData}
    end;

node_up(_Event, StateData) ->
    {next_state, node_up, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% --------------------------------------------------------------------
handle_sync_event(status, _From, StateName, StateData) ->
    Reply = StateName,
    {reply, Reply, StateName, StateData};

handle_sync_event(_Event, _From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_info(Info, StateName, StateData) ->
    case Info of
        {nodedown, Node} ->
			log4erl:log(info, "Node ~p is down", [Node]),
            gen_fsm:start_timer(?CHECK_NODE_INTERVAL, check_node),
			{next_state, node_down, StateData};            
        _Other -> % TODO notify about unexpected event
    		{next_state, StateName, StateData}
    end.

%% --------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% --------------------------------------------------------------------
terminate(_Reason, _StateName, _StatData) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% --------------------------------------------------------------------
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
