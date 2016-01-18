%%% -------------------------------------------------------------------
%%% Author  : anton
%%% Description :
%%%
%%% Created : 22 Jun 2008
%%% -------------------------------------------------------------------
-module(mm_nodes_worker).

-behaviour(gen_fsm).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define(CHECK_NODE_INTERVAL, 60000).
-define(POLL_RESULTS_INTERVAL, 1000).

%% --------------------------------------------------------------------
%% External exports
-export([start_link/1, execute/4, status/1]).

%% gen_fsm callbacks
-export([node_up/2, node_down/2]).

-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(state, {node}).

%% ====================================================================
%% External functions
%% ====================================================================

start_link(Node) ->
    gen_fsm:start_link(?MODULE, [Node], []).

execute(Ref, From, Kind, Args) ->
    gen_fsm:send_event(Ref, {execute, From, Kind, Args}).

status(Ref) ->
  gen_fsm:sync_send_all_state_event(Ref, status).

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
            gen_fsm:start_timer(?POLL_RESULTS_INTERVAL, poll_results),
			{next_state, node_up, StateData};
        pang ->
			log4erl:log(warn, "Node ~p is down", [Node]),
    		gen_fsm:start_timer(?CHECK_NODE_INTERVAL, check_node),
			{next_state, node_down, StateData}
	end;

node_down(Event, StateData) ->
	%% TODO reply with timeout or something
    {next_state, node_down, StateData}.

node_up({execute, From, Kind, Args}, StateData) ->
	Node = StateData#state.node,
	rpc:cast(Node, mm_monitor, execute, [From, Kind, Args]),
    {next_state, node_up, StateData};

node_up(nodedown, StateData) ->
    gen_fsm:start_timer(?CHECK_NODE_INTERVAL, check_node),
    {next_state, node_down, StateData};

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
