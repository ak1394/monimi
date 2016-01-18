-module(mm_multinode_check).

-export([new/1, success/2, failure/2, timeout/2]).

-record(sd, {nodes, checked_up, checked_down, index}).

new(Nodes) ->
	StateData = #sd{nodes=Nodes, checked_up=Nodes, checked_down=[], index=1},
	{StateData1, NextNode} = next(StateData),
	{NextNode, {starting, StateData1}}.
	
success(Node, {starting, StateData}) ->
	{NewStateData, NextNode} = checked_up(StateData, Node),
	{{up, NewStateData}, [{next_check, {normal, NextNode}}, {notify, up}]};

success(Node, {up, StateData}) ->
	{NewStateData, NextNode} = checked_up(StateData, Node),
	{{up, NewStateData}, [{next_check, {normal, NextNode}}, {notify, up}]};

%% a successfull check while verifying, go to degraded state
%% unless there are no failing nodes anymore
success(Node, {verifying, StateData}) ->
	{NewStateData, NextNode} = checked_up(StateData, Node),
	case no_checked_down_nodes(NewStateData) of
		true ->
			%% no failed nodes go to up state
			{{up, NewStateData}, [{next_check, {normal, NextNode}}, {notify, up}]};
		false ->
			%% change to degraded state
			{{degraded, NewStateData}, [{next_check, {normal, NextNode}}, {notify, up}]}
	end;

%% success in degraded state, change to up or stay in degraded
success(Node, {degraded, StateData}) ->
	{NewStateData, NextNode} = checked_up(StateData, Node),
	case no_checked_down_nodes(NewStateData) of
		true ->
			%% no failed nodes go to up state
			{{up, NewStateData}, [{next_check, {normal, NextNode}}, {notify, up}]};
		false ->
			%% stay in degraded
			{{degraded, NewStateData}, [{next_check, {normal, NextNode}}, {notify, up}]}
	end;

% success in 'down' state, go to degraded or up if no failing nodes
% notify about 'check is up' event
success(Node, {down, StateData}) ->
	{NewStateData, NextNode} = checked_up(StateData, Node),
	case no_checked_down_nodes(NewStateData) of
		true ->
			%% no failed nodes go to up state
			{{up, NewStateData}, [{next_check, {normal, NextNode}}, {notify, up}]};
		false ->
			%% stay in degraded
			{{degraded, NewStateData}, [{next_check, {normal, NextNode}}, {notify, up}]}
	end.

% failure in 'starting' state, stay in 'starting'
failure(Node, {starting, StateData}) ->
	{NewStateData, NextNode} = checked_down(StateData, Node),
	{{starting, NewStateData}, [{next_check, {normal, NextNode}}]};

% go to 'verifying'
failure(Node, {up, StateData}) ->
	{NewStateData, NextNode} = checked_down(StateData, Node),
	{{verifying, NewStateData}, [{next_check, {escalated, NextNode}}]};

% go to 'down' state or stay in 'verifying' 
failure(Node, {verifying, StateData}) ->
	{NewStateData, NextNode} = checked_down(StateData, Node),
	case no_checked_up_nodes(NewStateData) of
		true ->
			%% go to 'down' state and notify check is down
			{{down, NewStateData}, [{next_check, {normal, NextNode}}, {notify, down}]};
		false ->
			%% stay in 'verifying'
			{{verifying, NewStateData}, [{next_check, {escalated, NextNode}}]}
	end;

% go to 'down' or stay 'degraded'
failure(Node, {degraded, StateData}) ->
	{NewStateData, NextNode} = checked_down(StateData, Node),
	case no_checked_up_nodes(NewStateData) of
		true ->
			%% go to 'down' state and notify check is down
			{{down, NewStateData}, [{next_check, {normal, NextNode}}, {notify, down}]};
		false ->
			%% stay in 'degraded'
			{{degraded, NewStateData}, [{next_check, {normal, NextNode}}]}
	end;

% stay 'down'
failure(Node, {down, StateData}) ->
	{NewStateData, NextNode} = checked_down(StateData, Node),
	{{down, NewStateData}, [{next_check, {normal, NextNode}}, {notify, down}]}.

% timeout in some state stay in same state, request normal check
timeout(_Node, {State, StateData}) ->
	{NewStateData, NextNode} = next(StateData),
	{{State, NewStateData}, [{next_check, {normal, NextNode}}]}.
	
no_checked_down_nodes(StateData) ->
	length(StateData#sd.checked_down) == 0.

no_checked_up_nodes(StateData) ->
	length(StateData#sd.checked_up) == 0.

checked_up(StateData, Node) ->	
	case lists:member(Node, StateData#sd.checked_down) of
		true ->
			CheckedUp = [Node | StateData#sd.checked_up],
			CheckedDown = lists:delete(Node, StateData#sd.checked_down),
			next(StateData#sd{checked_up=CheckedUp, checked_down=CheckedDown});
		false ->
			next(StateData)
	end.

checked_down(StateData, Node) ->
	case lists:member(Node, StateData#sd.checked_up) of
		true ->
			CheckedUp = lists:delete(Node, StateData#sd.checked_up),
			CheckedDown = [Node | StateData#sd.checked_down],
			next(StateData#sd{checked_up=CheckedUp, checked_down=CheckedDown});
		false ->
			next(StateData)
	end.

next(StateData) when StateData#sd.index =< length(StateData#sd.nodes) ->
	Next = lists:nth(StateData#sd.index, StateData#sd.nodes),
	{StateData#sd{index=StateData#sd.index + 1}, Next};

next(StateData) ->
	Next = lists:nth(1, StateData#sd.nodes),
	{StateData#sd{index=2}, Next}.
