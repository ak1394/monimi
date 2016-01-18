%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(mm_monitor).
-author('author <author@example.com>').
-export([start/0, stop/0, execute/3, get_nameservers/1]).

%% @spec start() -> ok
%% @doc Start the xxx server.
start() ->
    inet_db:set_timeout(8000),
    application:set_env(kernel, gethost_poolsize, 10),
    ensure_started(crypto),
    ensure_started(ssl),
    ensure_started(ssh),
    application:start(mm_monitor, permanent).

%% @spec stop() -> ok
%% @doc Stop the xxx server.
stop() ->
    Res = application:stop(mm_monitor),
    application:stop(ssh),
    application:stop(ssl),
    application:stop(crypto),
    Res.

execute(From, Kind, Args) ->
    	mm_queue:execute(From, Kind, Args).

get_nameservers(Name) ->
    	mm_dns:lookup_ns(Name).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.