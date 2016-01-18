%%%----------------------------------------------------------------------
%%% File    : ping.erl
%%% Author  : Taavi Talvik <taavi@uninet.ee>
%%% Purpose : Support for ICMP ping
%%% Id      : $Id: ping.erl,v 1.6 2001/10/15 13:45:03 taavi Exp $
%%% Created : 24 Aug 2001 by Taavi Talvik <taavi@uninet.ee>
%%%----------------------------------------------------------------------

-module(mm_ping).
-author('taavi@uninet.ee').

%%-compile(export_all).
%%-export([Function/Arity, ...]).

-behaviour(gen_server).

%% External exports
-export([start_link/0,
	 pingw/1,pingw/2,
	 ping/1,ping/2,
	 stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%
%% port - dirver port, seq - current seuence no#
%% running - ets table holding currently running pings
-record(state, {port,
		seq,
		running}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, mm_ping}, mm_ping, [], []).

% ping and wait for reply
pingw(Address) ->
    pingw(Address, 5000).
pingw(Address,Timeout) ->
    case gen_server:call(?MODULE, {do_ping,  Address, Timeout}) of
	{ok, PingRef} ->
	    waitloop();
	{error, Reason} ->
	    {error, Reason}
    end.

waitloop() ->
    receive
	{ok, {id,Seq}} ->
	    waitloop();
	{error, Reason} ->
	    {error, Reason};
	Response ->
	    {ok, Response}
    end.

% ping and answer with response message
% Probably we have to return some kind of Ping reference, so
% that calling process can somehow match responses with this reference
% PingRef = {id, Seq}
ping(Address) ->
    ping(Address, 5000).
ping(Address, Timeout) ->
    gen_server:call(?MODULE, {do_ping, Address, Timeout}).
    

stop()->
    gen_server:call(?MODULE,stop).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    Port = open_port({spawn,"c_src/ping"},[binary,{packet,2}]),
    Tabel = ets:new(running, [ordered_set]),
    {ok, #state{port = Port, seq=0, running=Tabel}}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------

% Send out icmp ping via C driver
% Actually we should do name -> IP lookup asynchronously,
% to not stall C driver. Other possibility is to require
% name resolution beforehand. Currently we just resolve name in Erlang
% before handling things over to C driver.
handle_call({do_ping, Address, Timeout}, From, State) ->
    case inet:getaddr(Address, inet) of
	{ok, {A1,A2,A3,A4}} ->
	    ResolvedAddress = 
		integer_to_list(A1) ++ "." ++
		integer_to_list(A2) ++ "." ++
		integer_to_list(A3) ++ "." ++
		integer_to_list(A4),
	    {ok, Tref} = timer:send_after(Timeout,self(),{timeout,State#state.seq}),
	    {Pid, _What_is_This} = From,
	    ets:insert(State#state.running,{State#state.seq,
					    Pid,Address,Timeout,Tref}),
	    Reply = { ok, {id, State#state.seq}},
	    do_ping(ResolvedAddress, Pid, State);
	{error, Reason} ->
	    Reply = {error, Reason}
    end,
    Seq = State#state.seq + 1,
    {reply, Reply, State#state{seq=Seq}};

handle_call(stop, From, State) ->
    io:format("stop:~n"),
    {stop, stop, State};
handle_call(Message, From, State) ->
    Reason = {error, "ping:handle_call called with unknown message", Message},
    {stop, Reason, State}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(Message, State) ->
    Reason = {error, "Empty ping:handle_cast called", Message},
    {stop, Reason, State}.

% Reply from pinger process
handle_info({Port,{data,Binary}}, State) when port(Port) ->
    Message = binary_to_term(Binary),
    case Message of
	{ping_response, ParamList} ->
	    {value, {id,Seq}} = lists:keysearch(id, 1, ParamList),
	    case ets:lookup(State#state.running,Seq) of
		[{PingId, Pid, Address, Timeout,Tref}] ->
		    ets:delete(State#state.running,Seq),
		    {ok, cancel} = timer:cancel(Tref),
		    Pid ! {ping_response, [ParamList]},
		    {noreply, State};
		_ ->
		    % This may happen, when response is received after timeout
		    % maybe we just should silently ignore this situation?
		    Reason = {error, "Ets lookup for ping ID failed"},
		    {stop, Reason, State}
	    end;
	Other ->
	    io:format("Received other message than ping_response: ~p~n",[Other]),
	    {noreply, State}
    end;

% Pinger process exited
handle_info({'EXIT',Port,Reason},State) when port(Port) ->
    io:format("Ping subprocess port ~p closed due to ~p~n}",[Port,Reason]),
    {stop,{'EXIT',Port,Reason}, State};


% Ping has timed out
handle_info({timeout,Seq}, State) ->
    case ets:lookup(State#state.running,Seq) of
	[{PingId, Pid, Address,Timeout,Tref}] ->
	    ets:delete(State#state.running, Seq),
	    Pid ! {timeout, {ping,[{address,Address},{timeout,Timeout},{id,Seq}]}},
	    {noreply,State};
	_ ->
	    Reason = {error, "Ets lookup for ping ID failed"},
	    {stop, Reason, State}
    end;

%
handle_info(Msg,State) ->
    io:format("Unknown message: ~p~n",[Msg]),
    {noreply,State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(Reason, State) ->
    ets:delete(State#state.running),
    ok.

%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%----------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
do_ping(Address, From, State) ->
    Id = State#state.seq,
    Message = term_to_binary({ping, [{address,Address}, 
				     {id,Id}]}),
    State#state.port ! {self(), {command, Message}}.

    
