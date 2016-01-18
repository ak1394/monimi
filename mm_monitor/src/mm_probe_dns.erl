%%% -------------------------------------------------------------------
%%% Author  : anton
%%% Description :
%%%
%%% Created : 02 Feb 2009
%%% -------------------------------------------------------------------
-module(mm_probe_dns).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("kernel/src/inet_dns.hrl").

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
	Name = proplists:get_value(name, State#state.args),
	Type = case proplists:get_value(qtype, State#state.args) of
			   undefined ->
				   a;
			   Other ->
				   Other
		   end,
	case proplists:get_value(nameserver, State#state.args) of
		undefined ->
			case mm_dns:lookup_ns(Name) of
				{ ok, _, [Nameserver | _] } ->
					Result = probe_dns(Name, Nameserver, Type),
    				{reply, Result, State};
				{error, Reason} ->
					Result = [{status, failure}, {reason, {unable_to_find_nameservers, Reason}}],
    				{reply, Result, State}
			end;
		Nameserver ->
			Result = probe_dns(Name, Nameserver, Type),
    		{reply, Result, State}
	end;

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

probe_dns(Name, Nameserver, Type) ->
    Started = erlang:now(),
    case lookup_records(Name, Nameserver, Type) of
		{ok, _Result} ->
        	ResponseTime = timer:now_diff(erlang:now(), Started) div 1000,
            [{status, success}, {time, ResponseTime}];
		{error, Reason} ->
			[{status, failure}, {reason, Reason}]
	end.

lookup_records(Hostname, Nameserver, Type) ->
    lookup_records(Hostname, Nameserver, Type, 3).
lookup_records(_Hostname, _Nameserver, _Type, 0) ->
    {error, too_many_lookups};
lookup_records(Hostname, Nameserver, Type, MaxLookups) ->
    case inet:getaddr(Nameserver, inet) of
        {ok, Addr} ->
            case mm_dns:lookup(Hostname, Addr, Type) of
                {ok, Type, []} ->
                    {error, no_records_found};
                {ok, Type, Result} ->
                    {ok, Result};
                {ok, cname, [Cname | _]} when Type == a ->
                    case mm_dns:lookup_ns(Cname) of
                        {ok, _, [Nameserver1 | _]} ->
                            lookup_records(Cname, Nameserver1, a, MaxLookups - 1);
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Error} ->
            {error, {unable_to_lookup_ns_address, Error}}
    end.

parse_config({array, Array}) ->
	[{list_to_atom(Name), Value} || {struct, [{Name, Value}]} <- Array];
parse_config(Config) ->
	parse_config(mochijson:decode(Config)).	
	