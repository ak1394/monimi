%% Author: anton
%% Created: 23 Feb 2009
%% Description: TODO: Add description to mm_monitor_timer
-module(mm_monitor_timer).

%%
%% Include files
%%

-record(state, {checks, tick}).

%%
%% Exported Functions
%%
-export([new/0, add_check/3, remove_check/2, run/1, tick/1, gcd/1]).

%%
%% API Functions
%%

new() ->
	#state{checks=[], tick=undefined}.

add_check(Check, Beat, State) ->
	NewChecks = [{Check, Beat, undefined, undefined} | State#state.checks],
	State#state{checks=NewChecks}.

remove_check(CheckToRemove, State) ->
	NewChecks = [{Check, Beat, undefined, undefined} || {Check, Beat, _, _} <- State#state.checks, Check =/= CheckToRemove],
	State#state{checks=NewChecks, tick=undefined}.
	
start(State) ->
	BeatList = [Beat || {_, Beat, _, _} <- State#state.checks],
	Tick = gcd(BeatList),
	NewChecks = [{Check, Beat, Beat div Tick, 0} || {Check, Beat, _, _} <- State#state.checks],
	State#state{checks=NewChecks, tick=Tick}.

run(State) when State#state.tick =/= undefined ->
	ChecksToRun = [Check || {Check, _Beat, 1, _Count} <- State#state.checks],
	NewChecks = lists:map(fun({Check, Beat, Left, Count}) ->
								  case Left == 1 of
									  true ->
										  {Check, Beat, Beat div State#state.tick, Count+1};
									  false ->
										  {Check, Beat, Left-1, Count}
								  end
						  end, State#state.checks),
	{ChecksToRun, State#state{checks=NewChecks}};

run(State) ->
	run(start(State)).

tick(State) -> 
	State#state.tick.

%%
%% Local Functions
%%

%% Find greatest common denominator
gcd(A, 0) -> A;
gcd(A, B) when A < B -> gcd(B, A);
gcd(A, B) -> gcd(B, A rem B).
gcd([H|T]) ->
    lists:foldl(fun gcd/2, H, T).
