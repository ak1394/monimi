-module(mh_notification_fsm).

%%
%% Include files
%%

-record(state, {notification_id, destination_id, check_name, notify_down_on, notify_up, qp_start, qp_end, down_count, state}).

%%
%% Exported Functions
%%
-export([init/2, handle_event/2, get_id/1, update/2]).

%%
%% API Functions
%%

init(NotificationId, {CheckName, DestinationId, NotificationState, NotifyDownOn, NotifyUp, QpStart, QpEnd}) ->
    InitState = case NotificationState of
        normal -> normal;
        notified_down -> notified_down;
        new -> new
    end,
	#state{notification_id=NotificationId,
           destination_id=DestinationId,
           check_name = CheckName,
           notify_down_on=NotifyDownOn,
           notify_up=NotifyUp,
           qp_start=time_to_msec(QpStart),
           qp_end=time_to_msec(QpEnd),
           down_count=0,
           state=InitState}.

handle_event(StateData, Event) ->
    case StateData#state.state of
        normal -> normal(Event, StateData);
        notified_down -> notified_down(Event, StateData);
        new -> new(Event, StateData)
    end.

get_id(StateData) ->
    StateData#state.notification_id.

update(StateData, {_CheckName, _DestinationId, _NotificationState, NotifyDownOn, NotifyUp, QpStart, QpEnd}) ->
	StateData#state{notify_down_on=NotifyDownOn,
                    notify_up=NotifyUp,
                    qp_start=time_to_msec(QpStart),
                    qp_end=time_to_msec(QpEnd)}.                    

%%
%% Local Functions
%%

normal(up, StateData) ->
    StateData#state{down_count=0};

normal(down, StateData) when StateData#state.notify_down_on > 0 ->
    NewDownCount = StateData#state.down_count + 1,
    case NewDownCount >= StateData#state.notify_down_on of
        true ->
            case is_quiet_period(StateData) of
                true ->
                    %% in QP: don't notify just bump up down_count
					log4erl:log(info, "Notification ~p is down in and in QP, not sending message", [StateData#state.notification_id]),
                    StateData#state{down_count=NewDownCount};
                false ->
                    %% not in QP: notify, transition to notified_down
                    mh_database:update_notification_state(StateData#state.notification_id, notified_down),
                    mh_destination:notify(down, StateData#state.check_name, StateData#state.destination_id),
            		StateData#state{state=notified_down, down_count=NewDownCount}
            end;
        false ->
            StateData#state{down_count=NewDownCount}
    end;

normal(down, StateData) when StateData#state.notify_down_on == 0 -> 
    NewDownCount = StateData#state.down_count + 1,
    mh_database:update_notification_state(StateData#state.notification_id, notified_down),
    StateData#state{state=notified_down, down_count=NewDownCount}.

notified_down(up, StateData) when StateData#state.notify_up == true ->
    case is_quiet_period(StateData) of
        false ->
            %% not in QP: notify
            mh_destination:notify(up, StateData#state.check_name, StateData#state.destination_id);
        Else ->
            Else
    end,
    % transition to normal regardless of QP
    mh_database:update_notification_state(StateData#state.notification_id, normal),
    StateData#state{state=normal, down_count=0};

notified_down(up, StateData) -> 
    mh_database:update_notification_state(StateData#state.notification_id, normal),
    StateData#state{state=normal, down_count=0};

notified_down(down, StateData) -> 
    NewDownCount = StateData#state.down_count + 1,
    StateData#state{down_count=NewDownCount}.

%% go to 'normal' state after first 'up' event
new(up, StateData) -> 
    mh_database:update_notification_state(StateData#state.notification_id, normal),
    StateData#state{state=normal};

%% don't go to normal state (and don't send any notifications)
%% the check has never been up
new(down, StateData) -> StateData.

is_quiet_period(StateData) ->
    case (StateData#state.qp_start /= undefined) and (StateData#state.qp_end /= undefined) of
        true ->
    		{_Date, {CurrentHours, CurrentMinutes, CurrentSeconds}} = calendar:universal_time(),
    		CurrentMsec =  timer:hms(CurrentHours, CurrentMinutes, CurrentSeconds),
    		case StateData#state.qp_start < StateData#state.qp_end of
        		true ->
            		(StateData#state.qp_start =< CurrentMsec) and (StateData#state.qp_end >= CurrentMsec);
        		false ->
					(StateData#state.qp_start =< CurrentMsec) or (StateData#state.qp_end >= CurrentMsec)
    		end;
        false ->
            false
    end.

time_to_msec({time, {H, M, S}}) -> timer:hms(H, M, S);

time_to_msec(undefined) -> undefined.    
