%%% -------------------------------------------------------------------
%%% Author  : anton
%%% Description :
%%%
%%% Created : 30 Jun 2008
%%% -------------------------------------------------------------------
-module(mh_destination).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0, notify/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {templates}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link() -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

notify(Status, CheckName, DestinationId) ->
    gen_server:call(?MODULE, {notify, Status, CheckName, DestinationId}).    

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
	mh_sms:start_link(),
    mh_email:start_link(),
    Templates = compile_templates(
                 [{sms_up, "priv/sms_up.tmpl"},
                 {sms_down, "priv/sms_down.tmpl"},
				 {sms_up_lowcredit, "priv/sms_up_lowcredit.tmpl"},
                 {sms_down_lowcredit, "priv/sms_down_lowcredit.tmpl"},
				 {sms_up_lastcredit, "priv/sms_up_lastcredit.tmpl"},
                 {sms_down_lastcredit, "priv/sms_down_lastcredit.tmpl"},
                 {email_up, "priv/email_up.tmpl"},
                 {email_down, "priv/email_down.tmpl"}]),
    {ok, #state{templates=Templates}}.

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
handle_call({notify, Status, CheckName, DestinationId}, _From, State) ->
	{ok, Destination} = mh_database:get_destination(DestinationId),
    Templates = State#state.templates,
    case  Destination of
		{sms, UserId, Phone} ->
            Credits = mh_database:get_sms_credits(UserId),
            Params = [{check, CheckName}],
            case Credits of
                0 ->
					log4erl:log(info, "Can't send notification for check ~p, user ~p has no credit", [CheckName, UserId]),
                    ok;
                1 ->
                    Message = render_message(sms, credit_last, Status, Templates, Params),
            		mh_sms:send_alert(UserId, Phone, Message),
                	mh_database:deduct_sms_credits(UserId);
                2 ->
                    Message = render_message(sms, credit_low, Status, Templates, Params),
            		mh_sms:send_alert(UserId, Phone, Message),
                    mh_database:deduct_sms_credits(UserId);
                _ ->
                    Message = render_message(sms, credit_normal, Status, Templates, Params),
            		mh_sms:send_alert(UserId, Phone, Message),
                    mh_database:deduct_sms_credits(UserId)
                end;
        {email, _UserId, Email} ->
            Params = [{check, CheckName}, {email, Email}],
            Message = render_message(email, Status, Templates, Params),
            mh_email:send(Email, Message)
    end,
    {reply, ok, State};

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

compile_templates(Templates) ->
    Compiled = lists:map(fun({Name, FileName}) ->
                                 {ok, Compiled} = sgte:compile_file(FileName),
                                 {Name, Compiled}
                         end, Templates),
    dict:from_list(Compiled).
    
render_message(sms, credit_normal, up, Templates, Params) ->
    render_template(Templates, sms_up, Params);

render_message(sms, credit_normal, down, Templates, Params) ->
    render_template(Templates, sms_down, Params);

render_message(sms, credit_low, up, Templates, Params) ->
    render_template(Templates, sms_up_lowcredit, Params);

render_message(sms, credit_low, down, Templates, Params) ->
    render_template(Templates, sms_down_lowcredit, Params);

render_message(sms, credit_last, up, Templates, Params) ->
    render_template(Templates, sms_up_lastcredit, Params);

render_message(sms, credit_last, down, Templates, Params) ->
    render_template(Templates, sms_down_lastcredit, Params).

render_message(email, up, Templates, Params) ->
    render_template(Templates, email_up, Params);

render_message(email, down, Templates, Params) ->
    render_template(Templates, email_down, Params).

render_template(Templates, TemplateName, Params) ->
    PreparedParams = prepare_params(Params),
    Template = dict:fetch(TemplateName, Templates),
    sgte:render_str(Template, PreparedParams).

prepare_params(Params) ->
    lists:map(fun({Name, Value}) ->
                      {Name, convert_value(Value)}
              end, Params).

convert_value(Value) when is_binary(Value) ->
    binary_to_list(Value);

convert_value(Value) ->
    Value.
