%%% -------------------------------------------------------------------
%%% Author  : anton
%%% Description :
%%%
%%% Created : 24 Jun 2008
%%% -------------------------------------------------------------------
-module(mh_notification_manager).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0, notify/2, touch/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {notifications_by_check}).

%% ====================================================================
%% External functions
%% ====================================================================

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

notify(CheckId, Event) ->  
    gen_server:call(?MODULE, {notify, CheckId, Event}).

touch(CheckId) ->  
    gen_server:call(?MODULE, {touch, CheckId}).

%% ====================================================================
%% Local functions
%% ====================================================================

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
    {ok, AllNotifications} = mh_database:load_notifications(),
    NotificationsByCheck = lists:foldl(fun({CheckId, NotificationId, Params}, Dict) ->
                          State = mh_notification_fsm:init(NotificationId, Params),
                          dict:append(CheckId, {NotificationId, State}, Dict)
                end, dict:new(), AllNotifications), 
    {ok, #state{notifications_by_check=NotificationsByCheck}}.

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
handle_call({notify, CheckId, Event}, _From, State) ->
    Notifications = get_notifications(CheckId, State),
    NewNotifications = dispatch_event(Notifications, Event),
    NewState = put_notifications(CheckId, NewNotifications, State),
    {reply, ok, NewState};

handle_call({touch, CheckId}, _From, State) ->
    {ok, LoadedNotificationsData} = mh_database:load_notification(CheckId),
    Notifications = get_notifications(CheckId, State),
    UpdatedNotifications = lists:foldl(fun({NotificationId, Params}, Acc) ->
                                               case proplists:get_value(NotificationId, Notifications) of
                                                   undefined ->
                                                       log4erl:log(info, "Starting notification: ~p", [NotificationId]),
                                                       NewNotificationState = mh_notification_fsm:init(NotificationId, Params),
                                                       [{NotificationId, NewNotificationState} | Acc];
                                                   NotificationState ->
                                                       log4erl:log(info, "Updating notification: ~p", [NotificationId]),
                                                       NewNotificationState = mh_notification_fsm:update(NotificationState, Params),
                                                       [{NotificationId, NewNotificationState} | Acc]
                                                   end
                                               end, [], LoadedNotificationsData),
    NewState = put_notifications(CheckId, UpdatedNotifications, State),
    {reply, ok, NewState};

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

get_notifications(CheckId, State) ->
    case dict:find(CheckId, State#state.notifications_by_check) of
        {ok, Notifications} ->
            Notifications;
        error ->
            []
    end.


put_notifications(CheckId, [], State) ->
    State#state{notifications_by_check=
        dict:erase(CheckId, State#state.notifications_by_check)};

put_notifications(CheckId, Notifications, State) ->
    State#state{notifications_by_check=
        dict:store(CheckId, Notifications, State#state.notifications_by_check)}.


dispatch_event(Notifications, Event) ->
    lists:map(fun({NotificationId, NotificationState}) ->
                  {NotificationId, mh_notification_fsm:handle_event(NotificationState, Event)}
              end, Notifications).
