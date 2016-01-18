%%% -------------------------------------------------------------------
%%% Author  : anton
%%% Description :
%%%
%%% Created : 29 Jun 2008
%%% -------------------------------------------------------------------
-module(mh_check_fsm).

-behaviour(gen_fsm).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define(BEAT_TICK, 60000).
-define(CHECK_TIMEOUT, 60000).

%% --------------------------------------------------------------------
%% External exports
-export([start_link/3, process_result/3, stop/1]).

%% gen_fsm callbacks
-export([starting/2, up/2, verifying/2, degraded/2, down/2]).

-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(state, {check_id, beat, nodes, f_nodes, current_node_index}).

%% ====================================================================
%% External functions
%% ====================================================================

start_link(CheckId, Check, Nodes) ->
    Beat = proplists:get_value(beat, Check),
    gen_fsm:start_link(?MODULE, [CheckId, Beat, Nodes], []).

process_result(FsmRef, CheckResult, Node) ->
	case proplists:get_value(status, CheckResult) of
        success ->
    		gen_fsm:send_event(FsmRef, {success, Node});
        failure ->
     		gen_fsm:send_event(FsmRef, {failure, Node});
        crash ->
    		gen_fsm:send_event(FsmRef, {crash, Node})
    end.

stop(FsmRef) ->
	gen_fsm:send_all_state_event(FsmRef, stop). 

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
init([CheckId, Beat, Nodes]) ->
    mh_database:update_check_state(CheckId, unknown),
    mh_monitor:check(lists:nth(1, Nodes), CheckId),
    {ok, starting, #state{check_id=CheckId, 
                          current_node_index=1,
                          nodes=Nodes,
                          beat=Beat,
                          f_nodes=dict:new()}, ?CHECK_TIMEOUT}.    

%% --------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------

starting({success, _Node}, StateData) ->
    %% update database, notify node is up, set up timeout for the next check
    mh_notification_manager:notify(StateData#state.check_id, up),
    mh_database:update_check_state(StateData#state.check_id, up),
	{next_state, up, StateData, next_check_timeout(StateData)};

starting({failure, Node}, StateData) ->
    %% request out of schedule check at the next node and go to verifying state
    NewStateData = check_next(StateData),
	FNodes = dict:store(Node, now(), StateData#state.f_nodes),
    {next_state, verifying, NewStateData#state{f_nodes=FNodes}, ?CHECK_TIMEOUT};

starting(timeout, StateData) ->
    NewStateData = check_next(StateData),
	{next_state, starting, NewStateData, ?CHECK_TIMEOUT};

starting({crash, _Node}, StateData) ->
    %% stay in the same state retry check later
	{next_state, starting, StateData, next_check_timeout(StateData)};

starting(_Event, StateData) ->
	{next_state, starting, StateData}.

up({success, _Node}, StateData) ->
    %% notify up, schedule the next check
	mh_notification_manager:notify(StateData#state.check_id, up),
    {next_state, up, StateData, next_check_timeout(StateData)};

up({failure, Node}, StateData) ->
    %% request out of schedule check at the next node
    NewStateData = check_next(StateData),
	FNodes = dict:store(Node, now(), StateData#state.f_nodes),
    {next_state, verifying, NewStateData#state{f_nodes=FNodes}, ?CHECK_TIMEOUT};

up(timeout, StateData) ->
    %% request a check at the next node
    NewStateData = check_next(StateData),
    {next_state, up, NewStateData, ?CHECK_TIMEOUT};

up({crash, _Node}, StateData) ->
    %% stay in the same state retry check later
	{next_state, up, StateData, next_check_timeout(StateData)};

up(_Event, StateData) ->
	{next_state, up, StateData}.

verifying({success, Node}, StateData) ->
	mh_notification_manager:notify(StateData#state.check_id, up),
    %% recalc number of failure nodes and see if we've 0 failure nodes now
    %% note, because this is 'success' event number of failure nodes will always
    %% be less than number of all nodes
    NewFNodes = dict:erase(Node, StateData#state.f_nodes),
    case dict:size(NewFNodes) == 0 of
        true ->
            %% we must have checked all nodes and found that none of them are failing
            %% can schedule the next check on the normal schedule and go to 'up' state
            {next_state, up, StateData#state{f_nodes=NewFNodes}, next_check_timeout(StateData)};
        false ->
			%% can't go to 'up' state, still have some failure nodes left
			%% schedule the next on schedule check and go to 'degraded' state 
    		{next_state, degraded, StateData#state{f_nodes=NewFNodes}, next_check_timeout(StateData)}
    end;

verifying({failure, Node}, StateData) ->
    %% failure nodes and see if we've all the nodes failing now
    NewFNodes = dict:store(Node, now(), StateData#state.f_nodes),
    case dict:size(NewFNodes) == length(StateData#state.nodes) of
        true ->
            %% all nodes are failing: notify down,
            %% request the next check on a schedule and go to 'down' state
            mh_notification_manager:notify(StateData#state.check_id, down),
    		mh_database:update_check_state(StateData#state.check_id, down),
            {next_state, down, StateData#state{f_nodes=NewFNodes}, next_check_timeout(StateData)};
        false ->
            %% we've received a failure, but not all nodes have been checked yet
            %% request next out of schedule check, keep verifying
    		NewStateData = check_next(StateData),
            {next_state, verifying, NewStateData#state{f_nodes=NewFNodes}, ?CHECK_TIMEOUT}
    end;

verifying(timeout, StateData) ->
    %% should not normally occur
    %% can get here only because of 'crash' or missing check result
    NewStateData = check_next(StateData),
    {next_state, verifying, NewStateData, ?CHECK_TIMEOUT};

verifying({crash, _Node}, StateData) ->
    %% just time out for scheduled period and retry check again
	{next_state, verifying, StateData, next_check_timeout(StateData)};

verifying(_Event, StateData) ->
	{next_state, verifying, StateData}.

degraded({success, Node}, StateData) ->
	mh_notification_manager:notify(StateData#state.check_id, up),
    NewFNodes = dict:erase(Node, StateData#state.f_nodes),
    case dict:size(NewFNodes) == 0 of
        true ->
            %% all nodes are okay, go to 'up' state
            {next_state, up, StateData#state{f_nodes=NewFNodes}, next_check_timeout(StateData)};
        false ->
            %% stay in degraded
    		{next_state, degraded, StateData#state{f_nodes=NewFNodes}, next_check_timeout(StateData)}
    end;

degraded({failure, Node}, StateData) ->
    NewFNodes = dict:store(Node, now(), StateData#state.f_nodes),
    case dict:size(NewFNodes) == length(StateData#state.nodes) of
        true ->
            %% all nodes are failing: notify down,
            mh_notification_manager:notify(StateData#state.check_id, down),
    		mh_database:update_check_state(StateData#state.check_id, down),
            {next_state, down, StateData#state{f_nodes=NewFNodes}, next_check_timeout(StateData)};
        false ->
            %% not all nodes down yet, stay in degraded
            {next_state, degraded, StateData#state{f_nodes=NewFNodes}, next_check_timeout(StateData)}
	end;

degraded(timeout, StateData) ->
    NewStateData = check_next(StateData),
    {next_state, degraded, NewStateData, ?CHECK_TIMEOUT};

degraded({crash, _Node}, StateData) ->
    %% stay in the same state retry check later
	{next_state, degraded, StateData, next_check_timeout(StateData)};

degraded(_Event, StateData) ->
	{next_state, degraded, StateData}.

down({success, Node}, StateData) ->
	mh_notification_manager:notify(StateData#state.check_id, up),
    mh_database:update_check_state(StateData#state.check_id, up),
    NewFNodes = dict:erase(Node, StateData#state.f_nodes),
    case dict:size(NewFNodes) == 0 of
        true ->
            %% all nodes are okay, go to 'up' state
            {next_state, up, StateData#state{f_nodes=NewFNodes}, next_check_timeout(StateData)};
        false ->
            %% some nodes still down, go to degraded
    		{next_state, degraded, StateData#state{f_nodes=NewFNodes}, next_check_timeout(StateData)}
    end;

down({failure, _Node}, StateData) ->
    %% no change, all nodes are down
	mh_notification_manager:notify(StateData#state.check_id, down),
    {next_state, down, StateData, next_check_timeout(StateData)};

down(timeout, StateData) ->
    NewStateData = check_next(StateData),
    {next_state, down, NewStateData, ?CHECK_TIMEOUT};

down({crash, _Node}, StateData) ->
    %% stay in the same state retry check later
	{next_state, down, StateData, next_check_timeout(StateData)};

down(_Event, StateData) ->
	{next_state, down, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_event(stop, _StateName, StateData) ->
    {stop, shutdown, StateData};
                                          
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
handle_sync_event(_Event, _From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_info(_Info, StateName, StateData) ->
	{next_state, StateName, StateData}.

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
check_next(StateData) ->
    Nodes = StateData#state.nodes,
    CurrentIndex = StateData#state.current_node_index,
	NextIndex = case CurrentIndex < length(Nodes) of
                    true ->
                        CurrentIndex + 1;
                    false ->
                        1
                end,
    NextNode = lists:nth(NextIndex, Nodes),
    mh_monitor:check(NextNode, StateData#state.check_id),
    StateData#state{current_node_index=NextIndex}.

next_check_timeout(StateData) ->
    ?BEAT_TICK * StateData#state.beat.
	