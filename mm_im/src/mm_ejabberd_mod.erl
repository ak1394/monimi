%%% -------------------------------------------------------------------
%%% Author  : anton
%%% Description :
%%%
%%% Created : 13 Dec 2008
%%% -------------------------------------------------------------------
-module(mm_ejabberd_mod).

-behaviour(gen_server).
-behavior(gen_mod).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start/2, stop/1, start_link/2, route/3, send/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {my_host}).

-include("ejabberd.hrl").
-include("jlib.hrl").

%% ====================================================================
%% External functions
%% ====================================================================
start(Host, Opts) ->
    ChildSpec = {?MODULE,
        {?MODULE, start_link, [Host, Opts]},
        temporary,
        1000,
        worker,
        [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).

start_link(Host, Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Host, Opts], []).

stop(Host) ->
    gen_server:call(?MODULE, stop),
    supervisor:terminate_child(ejabberd_sup, ?MODULE),
    supervisor:delete_child(ejabberd_sup, ?MODULE).

route(From, To, Packet) ->
 	gen_server:call(?MODULE, {route, From, To, Packet}).    

send(User, Server, Message) ->
	gen_server:call(?MODULE, {send, User, Server, Message}).    

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
init([Host, Opts]) ->
    ?DEBUG("ECHO_BOT: Starting echo_bot", []),
    % add a new virtual host / subdomain "echo".example.com
    MyHost = gen_mod:get_opt_host(Host, Opts, "im.@HOST@"),
    ejabberd_router:register_route(MyHost, {apply, ?MODULE, route}),
    {ok, #state{my_host=MyHost}}.

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
handle_call({route, From, To, {xmlelement, "presence", _, _} = Packet}, _From, State) ->
    case xml:get_tag_attr_s("type", Packet) of
        "subscribe" ->
            send_presence(To, From, "subscribe");
        "subscribed" ->
            send_presence(To, From, "subscribed"),
            send_presence(To, From, "");
        "unsubscribe" ->
            send_presence(To, From, "unsubscribed"),
            send_presence(To, From, "unsubscribe");
        "unsubscribed" ->
            send_presence(To, From, "unsubscribed");
        "" ->
            send_presence(To, From, "");
        "unavailable" ->
            ok;
        "probe" ->
            send_presence(To, From, "");
        _Other ->
            ?INFO_MSG("Other kind of presence~n~p", [Packet])
    end,
    {reply, ok, State};

handle_call({route, From, To, {xmlelement, "message", _, _} = Packet}, _From, State) ->
    case xml:get_subtag_cdata(Packet, "body") of
    	"" ->
        	ok;
    	Body ->
        	case xml:get_tag_attr_s("type", Packet) of
        	"error" ->
            	?ERROR_MSG("Received error message~n~p -> ~p~n~p", [From, To, Packet]);
        	_ ->
                Reply = rpc:call('hub@hub.monimi.net', mm_hub, incoming_im, ["", "", Body]),
                case Reply of
                	{badrpc, Reason} ->
                		send_message(To, From, "chat", "badrpc");
                    _ ->
                        send_message(To, From, "chat", Reply)
                end
        end
    end,
    {reply, ok, State};

handle_call({route, From, To, Packet}, _From, State) ->
    {reply, ok, State};

handle_call({send, User, Server, Message}, _From, State) ->
    To = jlib:make_jid(User, Server, ""),
    From = jlib:make_jid("hub", State#state.my_host, ""),
    send_message(From, To, "chat", Message),
    {reply, ok, State};

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


strip_bom([239,187,191|C]) -> C;
strip_bom(C) -> C.

send_presence(From, To, "") ->
    ejabberd_router:route(From, To, {xmlelement, "presence", [], []});

send_presence(From, To, TypeStr) ->
    ejabberd_router:route(From, To, {xmlelement, "presence", [{"type", TypeStr}], []}).

echo(From, To, Body) ->
    send_message(From, To, "chat", Body).

send_message(From, To, TypeStr, BodyStr) ->
    XmlBody = {xmlelement, "message",
           [{"type", TypeStr},
        {"from", jlib:jid_to_string(From)},
        {"to", jlib:jid_to_string(To)}],
           [{xmlelement, "body", [],
         [{xmlcdata, BodyStr}]}]},
    ejabberd_router:route(From, To, XmlBody).
