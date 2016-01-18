%%% -------------------------------------------------------------------
%%% Author  : anton
%%% Description :
%%%
%%% Created : 1 Jul 2008
%%% -------------------------------------------------------------------
-module(mh_sms).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-define(ALERT_INTENCITY, 6).
-define(ALERT_PERIOD, 3600).

-define(CONFIRMATION_INTENCITY, 60).
-define(CONFIRMATION_PERIOD, 6*3600).

-define(RETRY_ATTEMPTS, 3).
-define(RETRY_TIMEOUT, 30000).

-define(SMS_SEND_TIMEOUT, 10000).

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0, send_confirmation/2, send_alert/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {confirmation_code}).

%% ====================================================================
%% External functions
%% ====================================================================

start_link() -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

send_confirmation(Phone, Code) ->
    gen_server:call(?MODULE, {send_confirmation, Phone, Code}, ?SMS_SEND_TIMEOUT).    

send_alert(UserId, Phone, Message) when is_binary(Phone) ->
    gen_server:cast(?MODULE, {send_alert, UserId, binary_to_list(Phone), Message});    

send_alert(UserId, Phone, Message) ->
    gen_server:cast(?MODULE, {send_alert, UserId, Phone, Message}).    

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
	{ok, Compiled} = sgte:compile_file("priv/confirmation_code.tmpl"),    
    {ok, #state{confirmation_code=Compiled}}.

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
handle_call({send_confirmation, Phone, Code}, _From, State) ->
	Message = sgte:render_str(State#state.confirmation_code, [{code, Code}]),
    Reply = do_send_confirmation(Phone, Message),
    {reply, Reply, State};

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
handle_cast({send_alert, UserId, Phone, Message}, State) ->
    do_send_alert(UserId, Phone, Message),
	{noreply, State};    

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

% TODO shall log throttled messages?

do_send_alert(UserId, Phone, Message) ->
    case mm_rate_limit:throttle(Phone, ?ALERT_INTENCITY, ?ALERT_PERIOD) of
        false ->
        	send_message(Phone, Message, UserId, alert, ?RETRY_ATTEMPTS);
        Else ->
            Else
    end.

do_send_confirmation(Phone, Message) ->
    case mm_rate_limit:throttle(confirmation, ?CONFIRMATION_INTENCITY, ?CONFIRMATION_PERIOD) of
        false ->
        	send_message(Phone, Message, 0, confirmation, ?RETRY_ATTEMPTS);
        Else ->
            Else
    end.

send_message(Phone, _Message, _UserId, _Reference, 0) ->
	log4erl:log(warn, "Unable to send SMS message to ~p giving up", [Phone]);

send_message(Phone, Message, UserId, Reference, Attempts) ->
    case catch clickatell:send({Phone, "monimi.net", Message}) of
    	{'EXIT', Reason} ->
        	log4erl:log(warn, "Attempt to send SMS failed: ~p", [Reason]),
            timer:sleep(?RETRY_TIMEOUT),
            send_message(Phone, Message, UserId, Reference, Attempts - 1);
		{error, Reason} ->
        	log4erl:log(error, "Error sending SMS to ~p: ~p", [Phone, Reason]),
            {error, Reason};
		MessageId ->
			mh_database:log_sms(UserId, Message, Phone, Reference, MessageId),
            {ok, MessageId}
    end.