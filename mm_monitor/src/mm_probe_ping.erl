%%% -------------------------------------------------------------------
%%% Author  : anton
%%% Description :
%%%
%%% Created : 24 Dec 2008
%%% -------------------------------------------------------------------
-module(mm_probe_ping).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define(TIMEOUT, 15000).
-define(PING_COUNT, 4).

%% --------------------------------------------------------------------
%% External exports
-export([start_link/1, run/1, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {args}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link(Args) -> 
    gen_server:start_link(?MODULE, [Args], []).

run(ServerRef) -> 
    gen_server:call(ServerRef, run, infinity).

stop(ServerRef) -> 
    gen_server:call(ServerRef, stop).

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
init([Args]) ->
    {ok, #state{args=Args}}.

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
handle_call(run, _From, State) ->
	Host = proplists:get_value(host, State#state.args),
    Result = ping_probe(Host),
    {reply, Result, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(_Request, State) ->
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
ping_probe(Host) ->
    Started = erlang:now(),
	case inet:getaddr(Host, inet) of
    	{ok, HostAddr} ->
            DnsLookupTime = timer:now_diff(erlang:now(), Started) div 1000,
        	Pings = ping_addr(HostAddr),
            [{dns_lookup_time, DnsLookupTime}] ++ inspect_result(Pings);
        {error, _Reason} ->
			[{status, failure}, {reason, dns_lookup_failed}]            
    end.

ping_addr(HostAddr) ->
    ping_addr(HostAddr, [], ?PING_COUNT).

ping_addr(_HostAddr, Results, PingsLeft) when PingsLeft == 0 ->
	Results;

ping_addr(HostAddr, Results, PingsLeft) ->
    Result = ping_once(HostAddr),
    ping_addr(HostAddr, [Result | Results], PingsLeft -1).

ping_once(HostAddr) ->
    {ok, Response} = mm_ping:pingw(HostAddr, ?TIMEOUT), 
    case Response of
    	{ping_response, Info} ->
            proplists:get_value(roundtrip, hd(Info));
        {timeout, _Info} ->
            timeout
    end.

inspect_result(Pings) ->
    %% remove all timeouts
	PingsOk = lists:filter(fun(Ping) -> Ping =/= timeout end, Pings),
	PingsLength = length(Pings),
	PingsOkLength = length(PingsOk),
    case PingsOkLength > 0 of
        true ->
			PacketLoss = (100 - round(PingsOkLength / (PingsLength / 100))),
    		Min = lists:min(PingsOk),
    		Max = lists:max(PingsOk),
    		Median = lists:nth(round((PingsOkLength / 2)), lists:sort(PingsOk)),
			[{status, success}, {rtt_min, Min}, {rtt_max, Max}, {rtt_avg, Median}, {packet_loss, PacketLoss}];
        false ->
            [{status, failure}, {reason, timeout}]
    end.