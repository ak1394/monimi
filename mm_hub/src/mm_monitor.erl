%%% -------------------------------------------------------------------
%%% Author  : anton
%%% Description :
%%%
%%% Created : 24 Feb 2009
%%% -------------------------------------------------------------------
-module(mm_monitor).

-behaviour(gen_fsm).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define(BEAT_TICK, 60000).
-define(ESCALATED_CHECK_PAUSE, 5000).

%% --------------------------------------------------------------------
%% External exports
-export([start_link/1, add_check/2, run/1]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export([starting/2, starting/3, sleep/2]).

-record(sd, {nodes, node, checks, state, timer, pending}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link(Nodes) ->
    gen_fsm:start_link(?MODULE, [Nodes], []).

add_check(Ref, Check) ->
	gen_fsm:sync_send_event(Ref, {add_check, Check}).

run(Ref) ->
	gen_fsm:send_event(Ref, run).

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
init([Nodes]) ->
	{NextNode, State} = mm_multinode_check:new(Nodes),
    {ok, starting, #sd{nodes=Nodes, node=NextNode, checks=[], state=State, pending=[], timer=mm_monitor_timer:new()}}.

%% --------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
starting(run, StateData) ->
	%% TODO sleep random amount of time at start
	gen_fsm:start_timer(1000, wakeup),
	{next_state, sleep, StateData}.

sleep({timeout, _Ref, wakeup}, StateData) ->
	io:format("~p~n", ["wakeup"]),
	{ChecksToRun, NewTimer} = mm_monitor_timer:run(StateData#sd.timer),
	run_checks(StateData#sd.node, ChecksToRun),
    {next_state, expect_result, StateData#sd{timer=NewTimer, pending=ChecksToRun}}.

%% state_name(Event, StateData) ->
%%     {next_state, state_name, StateData}.

%% --------------------------------------------------------------------
%% Func: StateName/3
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% --------------------------------------------------------------------
starting({add_check, Check}, From, StateData) ->
	NewChecks = [Check | StateData#sd.checks],
	NewTimer = mm_monitor_timer:add_check(Check, proplists:get_value(beat, Check), StateData#sd.timer),
    {reply, ok, starting, StateData#sd{checks=NewChecks, timer=NewTimer}}.

%% state_name(Event, From, StateData) ->
%%     Reply = ok,
%%     {reply, Reply, state_name, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_event(Event, StateName, StateData) ->
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
handle_sync_event(Event, From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_info({result, Node, Result}, expect_result, StateData) ->
	io:format("got result ~p ~p~n", [Node, Result]),
	%% analyze result and see if any checks require escalated recheck
	{NewState, Directions} = case proplists:get_value(status, Result) of
								 success ->
									 mm_multinode_check:success(Node, StateData#sd.state);
								 failure ->
									 mm_multinode_check:failure(Node, StateData#sd.state)
							 end,
	%% notify of check result
	case proplists:get_value(notify, Directions) of
		up ->
			io:format("check is up~n"),
			ok;
		down ->
			io:format("check is down~n"),
			ok
	end,
	
	%% save check result
	save_results(Node, Result, StateData#sd.pending),
	
	case proplists:get_value(next_check, Directions) of
		{normal, NextNode} ->
    		gen_fsm:start_timer(?BEAT_TICK * mm_monitor_timer:tick(StateData#sd.timer), wakeup),
    		{next_state, sleep, StateData#sd{state=NewState, pending=[], node=NextNode}};
		{escalated, NextNode} ->
			timer:sleep(?ESCALATED_CHECK_PAUSE),
			run_checks(NextNode, StateData#sd.pending),
    		{next_state, expect_result, StateData#sd{state=NewState, node=undefined}}
	end;

handle_info(Info, StateName, StateData) ->
    {next_state, StateName, StateData}.


%% --------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% --------------------------------------------------------------------
terminate(Reason, StateName, StatData) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% --------------------------------------------------------------------
code_change(OldVsn, StateName, StateData, Extra) ->
    {ok, StateName, StateData}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
run_checks(Node, Checks) ->
	Kind = proplists:get_value(kind, hd(Checks)),
	Args = prepare_args(Kind, Checks),
	io:format("run_checks ~p ~p ~p ~p~n", [self(), Node, Kind, Args]),
	mm_nodes:execute(self(), Node, Kind, Args).

save_results(Node, Result, Checks) ->
	lists:foreach(fun(Check) ->
                          mh_database:save_check_result(Check, Node, Result)
				  end, Checks).

prepare_args(http_check, [Check | T]) ->
	Config = proplists:get_value(config, Check),
	[{url, proplists:get_value(url, Config)}];
	
prepare_args(https_check, [Check | T]) ->
	Config = proplists:get_value(config, Check),
	[{url, proplists:get_value(url, Config)}];
	
prepare_args(ping_check, [Check | T]) ->
	Config = proplists:get_value(config, Check),
	[{host, proplists:get_value(host, Config)}];
	
prepare_args(dns_check, [Check | T]) ->
	Config = proplists:get_value(config, Check),
	Name = proplists:get_value(name, Config),
	Nameserver = proplists:get_value(ns, Config),
	QType = proplists:get_value(qtype, Config),
	[{name, Name}, {nameserver, Nameserver}, {qtype, QType}];
	
prepare_args(ssh_check, Checks) ->
	Config = proplists:get_value(config, hd(Checks)),
	Hostname = proplists:get_value(hostname, Config),
	Username = proplists:get_value(username, Config),
	Password = proplists:get_value(password, Config),
	Subchecks = [proplists:get_value(subtype, proplists:get_value(config, Check)) || Check <- Checks],
	[{hostname, Hostname}, {username, Username}, {password, Password}, {subchecks, Subchecks}].

	