%%% -------------------------------------------------------------------
%%% Author  : anton
%%% Description :
%%%
%%% Created : 13 Nov 2008
%%% -------------------------------------------------------------------
-module(mm_rate_limit).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0, throttle/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {keys}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link() -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% setup(Key, Period, Intensity) ->
%%     gen_server:call(?MODULE, {setup, Key, Period, Intensity}).    
%%     
%% throttle(Key) ->
%%     gen_server:call(?MODULE, {throttle, Key}).    

throttle(Key, Intensity, Period) ->
    gen_server:call(?MODULE, {throttle, Key, Intensity, Period}).    

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
    {ok, #state{keys=dict:new()}}.

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
%% handle_call({setup, Key, Period, Intensity}, _From, State) ->
%%     NewKeys = dict:store(Key, {Period, Intensity, []},  State#state.keys),
%%     {reply, ok,  State#state{keys=NewKeys}};
%% handle_call({throttle, Key}, _From, State) ->
%%     case dict:find(Key, State#state.keys) of
%%         {ok, {Period, Intensity, History}} ->
%%             Now = erlang:now(),
%%             NewHistory = add_entry([Now|History], Now, Period),
%%             NewState = State#state{keys=dict:store(Key, {Period, Intensity, NewHistory}, State#state.keys)},
%%             case length(NewHistory) =< Intensity of
%%                 true ->
%%                     %% no throttling, record event
%%                     {reply, false, NewState};
%%                 false ->
%%                     %% we decided to throttle, don't record new event
%%                     {reply, true, State}
%%             end;
%%         error ->
%%             {reply, error, State}
%%     end;

handle_call({throttle, Key, Intensity, Period}, _From, State) ->
    Now = erlang:now(),
    case dict:find(Key, State#state.keys) of
        {ok, History} ->
            NewHistory = add_entry([Now|History], Now, Period),
            NewState = State#state{keys=dict:store(Key, NewHistory, State#state.keys)},
            case length(NewHistory) =< Intensity of
                true ->
                    %% no throttling, record event
                    {reply, false, NewState};
                false ->
                    %% we decided to throttle, don't record new event
					log4erl:log(info, "Throttling ~p period ~p intensity ~p", [Key, Period, Intensity]),
                    {reply, true, State}
            end;
        error ->
			%% no throttling, record event
            NewHistory = add_entry([Now|[]], Now, Period),
			NewState = State#state{keys=dict:store(Key, NewHistory, State#state.keys)},
            {reply, false, NewState}
	end;

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

%%
%% modified code form supervisor.erl:
%%

add_entry([Entry|Entries], Now, Period) ->
    case inPeriod(Entry, Now, Period) of
    true ->
        [Entry|add_entry(Entries, Now, Period)];
    _ ->
        []
    end;
add_entry([], _, _) ->
    [].

inPeriod(Time, Now, Period) ->
    case difference(Time, Now) of
    T when T > Period ->
        false;
    _ ->
        true
    end.

%%
%% Time = {MegaSecs, Secs, MicroSecs} (NOTE: MicroSecs is ignored)
%% Calculate the time elapsed in seconds between two timestamps.
%% If MegaSecs is equal just subtract Secs.
%% Else calculate the Mega difference and add the Secs difference,
%% note that Secs difference can be negative, e.g.
%%      {827, 999999, 676} diff {828, 1, 653753} == > 2 secs.
%%

difference({TimeM, TimeS, _}, {CurM, CurS, _}) when CurM > TimeM ->
    ((CurM - TimeM) * 1000000) + (CurS - TimeS);
difference({_, TimeS, _}, {_, CurS, _}) ->
    CurS - TimeS.
