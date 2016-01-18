%%% -------------------------------------------------------------------
%%% Author  : anton
%%% Description :
%%%
%%% Created : 24 Dec 2008
%%% -------------------------------------------------------------------
-module(mm_runner).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start_link/3, run/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {from, kind, args}).

%% ====================================================================
%% External functions
%% ====================================================================

start_link(From, Kind, Args) ->
    gen_server:start_link(?MODULE, [From, Kind, Args], []).

run(Ref) -> 
    gen_server:cast(Ref, run).

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
init([From, Kind, Args]) ->
    process_flag(trap_exit, true),
    {ok, #state{from=From, kind=Kind, args=Args}}.

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
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(run, State) ->
	Probe = case State#state.kind of
        http_check ->
            mm_probe_http;
        https_check ->
            mm_probe_https;
        ping_check ->
            mm_probe_ping;
        dns_check ->
            mm_probe_dns
    end,
    {ok, Ref} = Probe:start_link(State#state.args),
    Started = erlang:now(),
	try Probe:run(Ref) of
        CheckResult ->
    		State#state.from ! {result, node(), [{checked, Started} | CheckResult]},
    		Probe:stop(Ref)
	catch
		exit:Reason ->
            io:format("catch: ~p~n", [Reason]),
			State#state.from ! [{checked, Started}, {status, crash}, {reason, Reason}];
        Other:OtherReason ->
            io:format("catch other: ~p ~p~n", [Other, OtherReason]),
    		State#state.from ! [{checked, Started}, {status, crash}, {reason, OtherReason}]
	end,
    mm_queue:completed(),
    {stop, normal, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({'EXIT', _Pid, normal}, State) ->
    {noreply, State};

handle_info({'EXIT', _Pid, _Reason}, State) ->
    mm_queue:completed(),
    {stop, normal, State}.

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
