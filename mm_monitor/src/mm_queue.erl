-module(mm_queue).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define(MAX_RUNNING, 10).

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0, execute/3, completed/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {running, queued}).

%% ====================================================================
%% External functions
%% ====================================================================

start_link() -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

execute(From, Kind, Args) -> 
	io:format("execute0 ~p ~p ~p~n", [From, Kind, Args]),
	gen_server:cast(?MODULE, {execute, From, Kind, Args}).

completed() -> 
	gen_server:cast(?MODULE, completed).

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
	{ok, #state{running=0, queued=queue:new()}}.

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

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_cast(completed, State) ->
	Queued = State#state.queued,
	case not queue:is_empty(Queued) of
		true ->
            {value, {From, Kind, Args}, NewQueued} = queue:out(Queued),
			mm_runner:run(From, Kind, Args),
    		{noreply, State#state{queued=NewQueued}};
		false ->
			Running = State#state.running - 1,
    		{noreply, State#state{running=Running}}
	end;

handle_cast({execute, From, Kind, Args}, State) ->
	io:format("execute ~p ~p ~p~n", [From, Kind, Args]),
    case State#state.running < ?MAX_RUNNING of
        true ->
			{ok, Runner} = mm_runner:start_link(From, Kind, Args), 
			mm_runner:run(Runner),
			Running = State#state.running + 1,
    		{noreply, State#state{running=Running}};
        false ->
            NewQueued = queue:in({From, Kind, Args}, State#state.queued),
    		{noreply, State#state{queued=NewQueued}}
    end.

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
