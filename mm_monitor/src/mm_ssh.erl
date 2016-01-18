%%% -------------------------------------------------------------------
%%% Author  : anton
%%% Description :
%%%
%%% Created : 21 Dec 2008
%%% -------------------------------------------------------------------
-module(mm_ssh).

-behaviour(gen_fsm).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-define(TIMEOUT, 30000).
-define(BANNER_TIMEOUT, 2000).
-define(PROMPT_TIMEOUT, 1000).
-define(COMMAND_TIMEOUT, 5000).

%% --------------------------------------------------------------------
%% External exports
-export([start/0, connect/4, send/2, close/1]).

-export([file_exists/2]).

%% gen_fsm callbacks
-export([init/1, starting/3, reading_banner/2, guess_prompt1/2, guess_prompt2/2, guess_prompt3/2,
         ready/3, waiting/2,
     handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(state, {connection, prompt, prompt1, prompt2, prompt3, channel, replyto, received, command}).

%% ====================================================================
%% External functions
%% ====================================================================

start() ->
    gen_fsm:start(?MODULE, [], []).

connect(FsmRef, Host, User, Password) ->
	gen_fsm:sync_send_event(FsmRef, {connect, Host, User, Password}, ?TIMEOUT). 

send(FsmRef, Command) ->
	gen_fsm:sync_send_event(FsmRef, {send, Command}, ?TIMEOUT).

close(FsmRef) ->
	gen_fsm:send_all_state_event(FsmRef, close).

file_exists(FsmRef, Filename) ->
	{ok, Response} = send(FsmRef, "/bin/ls " ++ Filename),
    io:format("ex: ~p~n", [Response]),
    Filename == hd(Response).

%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}
%% --------------------------------------------------------------------
init([]) ->
    {ok, starting, #state{prompt1=[], prompt2=[], prompt3=[], received=[], command=[]}}.

%% --------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
reading_banner(timeout, StateData) ->
    send_cr(StateData),
	{next_state, guess_prompt1, StateData, ?PROMPT_TIMEOUT}.

guess_prompt1(timeout, StateData) ->
    case length(StateData#state.prompt1) of
        0 ->
            gen_fsm:reply(StateData#state.replyto, {error, failed_to_guess_prompt}),
        	{stop, normal, StateData};
		_ ->
			send_cr(StateData),
			{next_state, guess_prompt2, StateData, ?PROMPT_TIMEOUT}
    end.

guess_prompt2(timeout, StateData) ->
    case length(StateData#state.prompt2) of
        0 ->
            gen_fsm:reply(StateData#state.replyto, {error, failed_to_guess_prompt}),
        	{stop, normal, StateData};
		_ ->
			send_cr(StateData),
			{next_state, guess_prompt3, StateData, ?PROMPT_TIMEOUT}
    end.

guess_prompt3(timeout, StateData) ->
    case length(StateData#state.prompt3) of
        0 ->
            gen_fsm:reply(StateData#state.replyto, {error, failed_to_guess_prompt}),
        	{stop, normal, StateData};
		_ ->
			case guess_prompt(hd(StateData#state.prompt1), hd(StateData#state.prompt2), hd(StateData#state.prompt3)) of
                {ok, Prompt} ->
                    io:format("prompt ~p~n", [Prompt]),
                    gen_fsm:reply(StateData#state.replyto, ok),
					{next_state, ready, StateData#state{prompt1=[], prompt2=[], prompt3=[], prompt=Prompt}};
                {error, Reason} ->
                    gen_fsm:reply(StateData#state.replyto, {error, Reason}),
        			{stop, normal, StateData}
            end
    end.

waiting(timeout, StateData) ->
    {stop, timeout, StateData}.

%% --------------------------------------------------------------------
%% Func: StateName/3
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% --------------------------------------------------------------------
starting({connect, Host, User, Password}, From, StateData) ->
    case ssh:connect(Host, 22, [{silently_accept_hosts, true}, 
                                 {user_interaction, false}, 
                                 {user_dir, "/var/tmp/ssh"}, 
                                 {user, User}, 
                                 {password, Password},
                                 {timeout, ?TIMEOUT}]) of
		{ok, Connection} ->
    		{ok, Channel} = ssh_connection:session_channel(Connection, ?TIMEOUT),
			success = ssh_connection:open_pty(Connection, Channel, "dumb", 80, 24, [], ?TIMEOUT),
    		ok = ssh_connection:shell(Connection, Channel),
    		{next_state, reading_banner, StateData#state{connection=Connection, channel=Channel, replyto=From}};
		{error, Reason} ->
			{stop, normal, {error, Reason}, StateData}
	end.

ready({send, Command}, From, StateData) ->
    send_cmd(StateData, Command),
    {next_state, waiting, StateData#state{replyto=From, command=Command}}.

%% --------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_event(close, _StateName, StateData) ->
    {stop, normal, StateData};
    
handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% --------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, StateData) ->
    {reply, ok, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_info(Info, starting, StateData) ->
    io:format("starting info: ~p~n", [Info]),
    {next_state, starting, StateData};

handle_info(Info, reading_banner, StateData) ->
    {ssh_cm, _ConnectionRef, {data, _ChannelId, _Type, _Data}} = Info,
    %%io:format("banner: ~p ~p~n", [Type, Data]),
    {next_state, reading_banner, StateData, ?BANNER_TIMEOUT};
  
handle_info(Info, guess_prompt1, StateData) ->
    {ssh_cm, _ConnectionRef, {data, _ChannelId, _Type, Data}} = Info,
    io:format("guess prompt1: ~p~n", [Data]),
    {next_state, guess_prompt1, StateData#state{prompt1=[Data | StateData#state.prompt1]}, ?PROMPT_TIMEOUT};

handle_info(Info, guess_prompt2, StateData) ->
    {ssh_cm, _ConnectionRef, {data, _ChannelId, _Type, Data}} = Info,
    io:format("guess prompt2: ~p~n", [Data]),
    {next_state, guess_prompt2, StateData#state{prompt2=[Data | StateData#state.prompt2]}, ?PROMPT_TIMEOUT};

handle_info(Info, guess_prompt3, StateData) ->
    {ssh_cm, _ConnectionRef, {data, _ChannelId, _Type, Data}} = Info,
    io:format("guess prompt3: ~p~n", [Data]),
    {next_state, guess_prompt3, StateData#state{prompt3=[Data | StateData#state.prompt3]}, ?PROMPT_TIMEOUT};

handle_info(Info, ready, StateData) ->
    {ssh_cm, _ConnectionRef, {data, _ChannelId, _Type, Data}} = Info,
    io:format("ready: ~p~n", [Data]),
    {next_state, guess_prompt3, StateData#state{prompt3=[Data | StateData#state.prompt3]}, ?COMMAND_TIMEOUT};

handle_info(Info, waiting, StateData) ->
    {ssh_cm, _ConnectionRef, {data, _ChannelId, _Type, Data}} = Info,
    NewReceived = StateData#state.received ++ binary_to_list(Data),
	%%io:format("data: ~p~n", [NewReceived]),
	case string:str(NewReceived, StateData#state.prompt) of
		0 ->
			% no prompt wait some more
    		{next_state, waiting, StateData#state{received=NewReceived}, ?COMMAND_TIMEOUT};
		Pos ->
            {ok, CmdData} = strip_data(NewReceived, Pos, StateData#state.command),
			Lines = string:tokens(CmdData, "\r\n"),
            gen_fsm:reply(StateData#state.replyto, {ok, Lines}),
    		{next_state, ready, StateData#state{received=[], command=[]}}
    end;

handle_info(Info, StateName, StateData) ->
    io:format("unknown info: ~p~n", [Info]),
    {next_state, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% --------------------------------------------------------------------
terminate(_Reason, _StateName, StateData) ->
    ssh:close(StateData#state.connection),
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% --------------------------------------------------------------------
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

send_cmd(StateData, Cmd) ->
	ssh_connection:send(StateData#state.connection,  StateData#state.channel, Cmd ++ "\n").

send_cr(StateData) ->
	ssh_connection:send(StateData#state.connection,  StateData#state.channel, "\n").

strip_data(Received, Position, Command) ->
    CommandCRNL = Command ++ "\r\n",
    Received1 = string:substr(Received, 1, Position-1),
    case string:str(Received1, CommandCRNL) of
        0 ->
            {error, cant_strip_command};
        1 ->
             Received2 = string:substr(Received1, length(CommandCRNL)+1),
			{ok, Received2};
        _ ->
        	{error, unexpected_command_position}
    end.

guess_prompt(Prompt1, Prompt2, Prompt3) when Prompt1 == Prompt2, Prompt1 == Prompt3 ->
    {ok, binary_to_list(Prompt1)};

guess_prompt(_Prompt1, _Prompt2, _Prompt3) ->
    {error, propmpts_does_not_match}.
