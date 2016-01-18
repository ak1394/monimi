%% Author: anton
%% Created: 23 Dec 2008
%% Description: TODO: Add description to mm_parse
-module(mm_ssh_parse).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([meminfo/1, beancounters/1, loadavg/1]).

%%
%% API Functions
%%

meminfo(Lines) ->
	lists:foldl(fun(Line, Proplist) ->
                        Tokens = string:tokens(Line, " :"), 
                      	case meminfo_parse(Tokens) of
                            unknown ->
                            	Proplist;
                            Result ->
                            	[Result | Proplist]
                        end
                end, [], Lines).

beancounters([First | Rest]) ->
    Version = [{vz_version, bc_version(First)}],
	lists:foldl(fun(Line, Proplist) ->
                        Tokens = string:tokens(Line, " :"), 
                      	case bc_parse(Tokens) of
                            skip ->
                            	Proplist;
                            Result ->
                            	[Result | Proplist]
                        end
                end, Version, Rest).

loadavg([Line | _]) ->
	[One, Five, Ten, NumRunning, NumTotal, LastId] = string:tokens(Line, " /"),
	[{one, round(list_to_float(One) * 1000)},
	 {five, round(list_to_float(Five) * 1000)}, 
	 {ten, round(list_to_float(Ten) * 1000)}, 
	 {num_running, NumRunning}, {num_total, NumTotal}, {last_id, LastId}].

%%
%% Local Functions
%%

meminfo_parse(["MemTotal", Value, _]) -> {mem_total, list_to_integer(Value)};
meminfo_parse(["MemFree", Value, _]) -> {mem_free, list_to_integer(Value)};
meminfo_parse(["Buffers", Value, _]) -> {buffers, list_to_integer(Value)};
meminfo_parse(["Cached", Value, _]) -> {cached, list_to_integer(Value)};
meminfo_parse(["SwapTotal", Value, _]) -> {swap_total, list_to_integer(Value)};
meminfo_parse(["SwapFree", Value, _]) -> {swap_free, list_to_integer(Value)};
meminfo_parse(_) -> unknown.

bc_version(Line) ->
	case string:tokens(Line, " :") of
		["Version", Version] ->
            Version;
        _ ->
            unknown
    end.

bc_parse([_Uid, "kmemsize" | Rest]) -> {kmemsize, bc_values(Rest)};
bc_parse(["lockedpages" | Rest]) -> {lockedpages, bc_values(Rest)};
bc_parse(["privvmpages" | Rest]) -> {privvmpages, bc_values(Rest)};
bc_parse(["shmpages" | Rest]) -> {shmpages, bc_values(Rest)};
bc_parse(["numproc" | Rest]) -> {numproc, bc_values(Rest)};
bc_parse(["physpages" | Rest]) -> {physpages, bc_values(Rest)};
bc_parse(["vmguarpages" | Rest]) -> {vmguarpages, bc_values(Rest)};
bc_parse(["oomguarpages" | Rest]) -> {oomguarpages, bc_values(Rest)};
bc_parse(["numtcpsock" | Rest]) -> {numtcpsock, bc_values(Rest)};
bc_parse(["numflock" | Rest]) -> {numflock, bc_values(Rest)};
bc_parse(["numpty" | Rest]) -> {numpty, bc_values(Rest)};
bc_parse(["numsiginfo" | Rest]) -> {numsiginfo, bc_values(Rest)};
bc_parse(["tcpsndbuf" | Rest]) -> {tcpsndbuf, bc_values(Rest)};
bc_parse(["tcprcvbuf" | Rest]) -> {tcprcvbuf, bc_values(Rest)};
bc_parse(["othersockbuf" | Rest]) -> {othersockbuf, bc_values(Rest)};
bc_parse(["dgramrcvbuf" | Rest]) -> {dgramrcvbuf, bc_values(Rest)};
bc_parse(["numothersock" | Rest]) -> {numothersock, bc_values(Rest)};
bc_parse(["dcachesize" | Rest]) -> {dcachesize, bc_values(Rest)};
bc_parse(["numfile" | Rest]) -> {numfile, bc_values(Rest)};
bc_parse(["numiptent" | Rest]) -> {numiptent, bc_values(Rest)};
bc_parse(["dummy" | _Rest]) -> skip;
bc_parse(_) -> skip.

bc_values([Held, Maxheld, Barrier, Limit, Failcnt]) ->
    [{held, Held}, {maxheld, Maxheld}, {barrier, Barrier}, {limit, Limit}, {failcnt, Failcnt}].