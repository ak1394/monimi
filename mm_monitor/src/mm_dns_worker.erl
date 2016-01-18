%%% -------------------------------------------------------------------
%%% Author  : anton
%%% Description :
%%%
%%% Created : 1 Feb 2009
%%% -------------------------------------------------------------------
-module(mm_dns_worker).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include_lib("kernel/src/inet_dns.hrl").

-define(TIMEOUT, 30000).
-define(RETRY, 1).

-ifdef(DEBUG).
-define(dbg(Fmt, Args), io:format(Fmt, Args)).
-else.
-define(dbg(Fmd, Args), ok).
-endif.

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0, lookup/6, lookup_ns/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

%% ====================================================================
%% External functions
%% ====================================================================

start_link() ->
    gen_server:start_link(?MODULE, [], []).

lookup(Ref, From, Id, Name, Ns, Type) ->
    gen_server:cast(Ref, {lookup, From, Id, Name, Ns, Type}).

lookup_ns(Ref, From, Name) ->
    gen_server:cast(Ref, {lookup_ns, From, Name}).

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
    {ok, #state{}}.

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
handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({lookup, From, Id, Name, Ns, Type}, State) ->
    Result = res_getby_query(Name, [{Ns, 53}], Type, false, Id),
	gen_server:reply(From, Result),
    {stop, normal, State};

handle_cast({lookup_ns, From, Name}, State) ->
	Result = lookup_ns1(Name),
	gen_server:reply(From, Result),
    {stop, normal, State};

handle_cast(Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
res_getby_query(Name, Ns, Type, Timer, Id) ->
    {ok, Buffer} = res_mkquery(Name, in, Type, Id),
    case res_send(Id, Buffer, Ns,Timer) of
    	{ok, Rec} ->
        	case res_lookup_type(Name, Type, Rec#dns_rec.anlist) of
            	[] when Type == a ->
                    case res_lookup_type(Name, cname, Rec#dns_rec.anlist) of
                        [] -> 
                            {ok, a, []};
                        Result1 ->
                            {ok, cname, Result1}
                    end;
            	Result ->
                	{ok, Type, Result}
        	end;
    	Error -> Error
    end.
	
res_send(Id, Buffer, Ns, Timer) ->
    Retry = ?RETRY,
    Tm = ?TIMEOUT,
    res_send_udp2(Id, Buffer, Retry, Tm, Timer, Ns).

res_send_udp2(Id, Buffer, Retry, Tm, Timer, Ns) ->
    case inet_udp:open(0, [{active,false}]) of
    {ok,S} ->
        Res = res_send_udp(S, Id, Buffer, 0, Retry, Tm, Timer, Ns),
        inet_udp:close(S),
        Res;
    Error -> Error
    end.

res_send_udp(_S, _Id, _Buffer, N, N, _Tm, _Timer, _Ns) ->
    {error, timeout};
res_send_udp(S, Id, Buffer, I, N, Tm, Timer, Ns) ->
    Num = length(Ns),
    if Num =:= 0 ->
        {error, timeout};
       true ->
        case res_send_query_udp(S,Id,Buffer,I,Num,Tm,Timer,Ns,[]) of
        {noanswer, ErrNs} -> %% remove unreachable nameservers
            res_send_udp(S, Id, Buffer, I+1, N, Tm,Timer,Ns--ErrNs);
        Result ->
            Result
        end
    end.

res_send_query_udp(S, Id, Buffer, I, N, Tm, Timer, [{IP, Port}|Ns],ErrNs) ->
    Timeout = inet:timeout( (Tm * (1 bsl I)) div N, Timer),
    ?dbg("Try UDP server : ~p:~p (timeout=~w)\n", [IP, Port,Timeout]),
    inet_udp:connect(S, IP, Port),
    inet_udp:send(S, IP, Port, Buffer),
    case res_recv_reply_udp(S, IP, Port, Id, Timeout) of
    {ok, Rec} ->
        {ok, Rec};
    {error, nxdomain} ->
        {error, nxdomain};
    {error, qfmterror} ->
        {error, einval};
    {error, enetunreach} ->
        res_send_query_udp(S, Id, Buffer, I, N, Tm,Timer,
                   Ns, [{IP,Port}|ErrNs]);
    {error, econnrefused} ->
        res_send_query_udp(S, Id, Buffer, I, N, Tm, Timer,                                                                
                   Ns, [{IP,Port}|ErrNs]);
    {error, timeout} when Timeout =:= 0 ->
        {error, timeout};
    _Error -> res_send_query_udp(S, Id, Buffer, I, N, Tm, Timer,
                    Ns, ErrNs)
    end;
res_send_query_udp(_S, _Id, _Buffer, _I, _N, _Tm, _Timer, [], ErrNs) ->
    {noanswer, ErrNs}.


res_recv_reply_udp(S, IP, Port, Id, Timeout) ->
    case inet_udp:recv(S, 0, Timeout) of
    {ok, {IP, Port, Answer}} ->                                                                                             
        case decode_answer(Answer, Id) of                                                                                   
        {error, badid} ->
            res_recv_reply_udp(S, IP, Port, Id, Timeout);
        Reply -> Reply
        end;
    {ok, _} -> res_recv_reply_udp(S, IP, Port, Id, Timeout);
    Error ->
        ?dbg("Udp server error: ~p\n", [Error]),
        Error
    end.

res_mkquery(Dname, Class, Type, Id) ->
    Recurse = 0, %TODO option (recursive)
    Rec = #dns_rec { header = #dns_header { id = Id,
                       opcode = ?QUERY,
                       rd = Recurse,
                       rcode = ?NOERROR },
            qdlist = [ #dns_query {domain = Dname,
                       type = Type,
                       class = Class } ] },
    ?dbg("Query: ~p~n", [Rec]),
    {ok, Buffer} = inet_dns:encode(Rec),
    {ok, Buffer}.

decode_answer(Answer, Id) ->
    case inet_dns:decode(Answer) of
    {ok, Rec} ->
        ?dbg("Got reply: ~p~n", [Rec]),
        H = Rec#dns_rec.header,
        case H#dns_header.rcode of
        ?NOERROR ->
            if H#dns_header.id =/= Id ->
                {error, badid};                                                                                           
               length(Rec#dns_rec.qdlist) =/= 1 ->
                {error, noquery};
               true ->
                {ok, Rec}
            end;
        ?FORMERR  -> {error, qfmterror};
        ?SERVFAIL -> {error, servfail};
        ?NXDOMAIN -> {error, nxdomain};
        ?REFUSED  -> {error, refused};
        _ -> {error, unknown}
        end;
    Error ->
        ?dbg("Got reply: ~p~n", [Error]),
        Error
    end.

res_lookup_type(Domain,Type,RRs) ->
    [R#dns_rr.data || R <- RRs,
                      R#dns_rr.domain =:= Domain,
                      R#dns_rr.type =:= Type].

lookup_ns1(Name) ->
    lookup_ns1(Name, 3).

lookup_ns1(Name, 0) ->
  {error, nxdomain};

lookup_ns1(Name, Level) ->
	case inet_res:nslookup(Name, ?C_IN, ns) of
    	{ok, #dns_rec{anlist = [#dns_rr{type = ?S_NS} | _]}} = Answer ->
			read_ns_answer(Answer);
    	{ok, #dns_rec{anlist = [#dns_rr{type = ?S_CNAME} | _]}} = Answer ->
            [_ | Name1] = string:tokens(Name, "."),
            lookup_ns1(string:join(Name1, "."), Level - 1);
    	{ok, #dns_rec{anlist = []}} = Answer ->
            [_ | Name1] = string:tokens(Name, "."),
			case length(Name1) of
				1 ->
  					{error, nxdomain};
				_ ->
            		lookup_ns1(string:join(Name1, "."), Level - 1)
			end;
        {error, nxdomain} ->
            [_ | Name1] = string:tokens(Name, "."),
			case length(Name1) of
				1 ->
  					{error, nxdomain};
				_ ->
            		lookup_ns1(string:join(Name1, "."), Level - 1)
			end;
    	{error, Reason} -> 
        	{ error, Reason}
	end.

read_ns_answer({ok, #dns_rec{anlist = Answer}}) ->
    [#dns_rr{type = ?S_NS, domain = Domain} | _] = Answer,
    {ok, Domain, read_ns_list(Answer)}.

read_ns_list([#dns_rr{type=?S_NS, data = Nameserver } | T]) ->
    [Nameserver | read_ns_list(T)];
read_ns_list([]) -> [].



