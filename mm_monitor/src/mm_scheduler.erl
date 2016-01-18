%%% -------------------------------------------------------------------
%%% Author  : anton
%%% Description :
%%%
%%% Created : 17 Jun 2008
%%% -------------------------------------------------------------------
-module(mm_scheduler).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define(TIMEOUT, 5000).
-define(BEAT_TICK, 60000).
-define(MAX_RUNNING, 10).

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0, add/2, remove/1, check/1, completed/1, purge/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {checks, running, queued}).

%% ====================================================================
%% External functions
%% ====================================================================

start_link() -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add(CheckId, Check) -> 
	gen_server:call(?MODULE, {add, CheckId, Check}).

remove(CheckId) -> 
	gen_server:call(?MODULE, {remove, CheckId}).

purge() -> 
	gen_server:call(?MODULE, {purge}).

check(CheckId) -> 
	gen_server:call(?MODULE, {check, CheckId}).

completed(CheckId) -> 
	gen_server:cast(?MODULE, {completed, CheckId}).

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
	{ok, #state{checks=dict:new(), running=dict:new(), queued=queue:new()}}.

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
handle_call({add, CheckId, Check}, _From, State) ->
	NewChecks = dict:store(CheckId, Check, State#state.checks),
    {reply, ok, State#state{checks=NewChecks}};

handle_call({remove, CheckId}, _From, State) ->
    NewQueued = queue:filter(fun(QueuedCheckId) -> CheckId =/= QueuedCheckId end, State#state.queued),
    NewChecks = dict:erase(CheckId, State#state.checks),
    NewRunning = case dict:is_key(CheckId, State#state.running) of
                     true ->
                         Pid = dict:fetch(CheckId, State#state.running),
                         exit(Pid, kill),
                         dict:erase(CheckId, State#state.running);
                     false ->
                         State#state.running
                 end,
    {reply, ok, State#state{checks=NewChecks, queued=NewQueued, running=NewRunning}};

handle_call({purge}, _From, State) ->
    lists:foreach(fun({_CheckId, Pid}) ->
                          exit(Pid, kill)
                  end, dict:to_list(State#state.running)),
	{reply, ok, State#state{checks=dict:new(), running=dict:new(), queued=queue:new()}};    

handle_call({check, CheckId}, _From, State) ->
	gen_server:cast(?MODULE, {request_run, CheckId}),
    {reply, ok, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_cast({completed, CheckId}, State) ->
	NewRunning1 = dict:erase(CheckId, State#state.running),
	{NewQueued, NewRunning2} = run_if_queued(State#state.queued, NewRunning1, State#state.checks),
    {noreply, State#state{running=NewRunning2, queued=NewQueued}};

handle_cast({request_run, CheckId}, State) ->
    case dict:size(State#state.running) < ?MAX_RUNNING of
        true ->
			NewRunning = do_run(CheckId, State#state.checks, State#state.running),
            NewState = State#state{running=NewRunning};
        false ->
            NewQueued = queue:in(CheckId, State#state.queued),
            NewState = State#state{queued=NewQueued}
    end,
    {noreply, NewState};

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

do_run(CheckId, Checks, Running) ->
	case dict:is_key(CheckId, Checks) of
        true ->
            Check = dict:fetch(CheckId, Checks),
            {ok, Ref} = mm_runner:start(CheckId, Check),
            mm_runner:run(Ref),
            dict:store(CheckId, Ref, Running);
        false ->
            Running
    end.

run_if_queued(Queued, Running, Checks) ->
	case not queue:is_empty(Queued) of
        true ->
            {{value, CheckId}, NewQueued} = queue:out(Queued),
        	NewRunning = do_run(CheckId, Checks, Running),
            {NewQueued, NewRunning};
        false ->
        	{Queued, Running}
    end.
