%%% -------------------------------------------------------------------
%%% Author  : anton
%%% Description :
%%%
%%% Created : 1 Jul 2008
%%% -------------------------------------------------------------------
-module(mh_email).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define(EMAIL_INTENCITY, 20).
-define(EMAIL_PERIOD, 3600).

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0, send/2, remind_password/2, send_welcome/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {password_reminder, welcome}).

%% ====================================================================
%% External functions
%% ====================================================================

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

send(Email, Message) ->
    gen_server:cast(?MODULE, {send, Email, Message}).    

remind_password(Email, Password) ->
    gen_server:cast(?MODULE, {remind_password, Email, Password}).    

send_welcome(Email, Password) ->
    gen_server:cast(?MODULE, {send_welcome, Email, Password}).    

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
	{ok, PasswordReminder} = sgte:compile_file("priv/password_reminder.tmpl"),    
	{ok, Welcome} = sgte:compile_file("priv/welcome_email.tmpl"),    
    {ok, #state{password_reminder=PasswordReminder, welcome=Welcome}}.

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
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({send, Email, Message}, State) ->
    case mm_rate_limit:throttle(Email, ?EMAIL_INTENCITY, ?EMAIL_PERIOD) of
        false ->
    		smtpc:sendmail(localhost, "monitor@monimi.net", binary_to_list(Email), Message);
        Else ->
            Else
    end,
    {noreply, State};

handle_cast({remind_password, Email, Password}, State) ->
    case mm_rate_limit:throttle(Email, ?EMAIL_INTENCITY, ?EMAIL_PERIOD) of
        false ->
    		Message = sgte:render_str(State#state.password_reminder, [{password, Password}, {email, Email}]),
    		smtpc:sendmail(localhost, "support@monimi.net", Email, Message);
        Else ->
            Else
    end,
    {noreply, State};

handle_cast({send_welcome, Email, Password}, State) ->
    case mm_rate_limit:throttle(Email, ?EMAIL_INTENCITY, ?EMAIL_PERIOD) of
        false ->
    		Message = sgte:render_str(State#state.welcome, [{password, Password}, {email, Email}]),
    		smtpc:sendmail(localhost, "support@monimi.net", Email, Message);
        Else ->
            Else
    end,
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

