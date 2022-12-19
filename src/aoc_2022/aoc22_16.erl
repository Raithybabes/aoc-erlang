-module(aoc22_16).

-export([answer/0]).

process_dat(Line) ->
    [Valve, FlowRate, TunnelsList] = dat:extract(
        Line, "Valve (\\w\\w) has flow rate=(\\d+); tunnels? leads? to valves? (.*)$"
    ),
    Tunnels = [Flat || [Flat] <- dat:extract_global(TunnelsList, "(\\w\\w),? ?")],
    {Valve, list_to_integer(FlowRate), Tunnels}.

add_vertices([], Graph) ->
    Graph;
add_vertices([{V, _, _} | T], Graph) ->
    digraph:add_vertex(Graph, V),
    add_vertices(T, Graph).

add_edges([], Graph) ->
    Graph;
add_edges([{V, _, Tunnels} | T], Graph) ->
    lists:foreach(
        fun(V2) ->
            digraph:add_edge(Graph, V, V2),
            digraph:add_edge(Graph, V2, V)
        end,
        Tunnels
    ),
    add_edges(T, Graph).

build_graph(Dat) -> add_edges(Dat, add_vertices(Dat, digraph:new())).

do(Graph, Distances, Valve, VWF, AllowedTime) -> do(Graph, Distances, Valve, VWF, VWF, 0, 0, AllowedTime).

do(_Graph, _Distances, _Valve, [], _VWF, Total, MaxPressure, _Mins) ->
    max(Total, MaxPressure);
% do(_Graph, _Distances, _Valve, ValvesToCheck, _VWF2, Total, MaxPressure, _Mins) when length(ValvesToCheck) < 3 ->
%     max(Total, MaxPressure);
do(_Graph, _Distances, _Valve, _, _VWF, Total, MaxPressure, Mins) when Mins =:= 0 ->
    max(Total, MaxPressure);
do(_Graph, _Distances, _Valve, _, _VWF, _Total, MaxPressure, Mins) when Mins < 0 -> MaxPressure;
do(Graph, Distances, Valve, [{TV, TF} = V2 | VWF], VWF2, Total, MaxPressure, Mins) ->
    % - 1 (don't include starting node) + 1 (extra minute to turn the valve on)
    NewMins = Mins - maps:get({Valve, TV}, Distances) - 1,
    NewTotal = Total + NewMins * TF,
    NewMaxPressure = do(
        Graph, Distances, TV, VWF2 -- [V2], VWF2 -- [V2], NewTotal, MaxPressure, NewMins
    ),
    do(Graph, Distances, Valve, VWF, VWF2, Total, NewMaxPressure, Mins).

do2(Perms, Graph, Distances, Valve, VWF, AllowedTime) -> do2(Perms, Graph, Distances, Valve, VWF, AllowedTime, 0).
do2([], _Graph, _Distances, _Valve, _VWF, _AllowedTime, Best) -> Best;
do2([Perm|Rest], Graph, Distances, Valve, VWF, AllowedTime, Best) ->
    Yours = Perm,
    Eleps = VWF -- Yours,
    % io:format("Checking permutation with ~p remaining (you/ele check ~p/~p) ... ", [length(Rest), length(Yours), length(Eleps)]),
    YourFlow = do(Graph, Distances, Valve, Yours, AllowedTime),
    ElepFlow = do(Graph, Distances, Valve, Eleps, AllowedTime),
    Total = YourFlow + ElepFlow,
    % io:format("~p / ~p~n", [Total, Best]),
    do2(Rest, Graph, Distances, Valve, VWF, AllowedTime, max(Best, Total)).

% sort_flows({_, F1}, {_, F2}) -> F1 >= F2.

perms(List) -> perms(List, [], []).
perms([], _, Perms) ->
    Perms;
perms([H | T], Taken, Perms) ->
    NewTaken = [H | Taken],
    NewPerms = perms(T, [H | Taken], Perms),
    perms(T, Taken, [NewTaken | NewPerms]).

answer() ->
    Dat = aoc22:data("16", fun process_dat/1),
    % ValvesLookup = maps:from_list([{V, Valve} || {V, _, _} = Valve <- Dat]),
    Graph = build_graph(Dat),
    Valves = [{V, F} || {V, F, _} <- Dat],
    ValvesWithFlow = lists:filter(fun({_, FlowRate}) -> FlowRate > 0 end, Valves),
    DistanceLookup = maps:from_list([
        {{VA, VB}, length(digraph:get_short_path(Graph, VA, VB)) - 1}
     || {VA, _} <- Valves, {VB, _} <- Valves
    ]),

    Start1 = erlang:system_time(),
    Part1 = do(Graph, DistanceLookup, "AA", ValvesWithFlow, 30), % 1701
    Stop1 = erlang:system_time(),
    io:format("Completed in ~p secs~n", [floor((Stop1 - Start1) / 100000000) / 10]),
    io:format("Part 1 = ~p~n~n", [Part1]),

    Perms0 = perms(ValvesWithFlow),
    Perms = lists:filter(fun (P) -> (length(P) >= 8) and (length(P) =< 10) end, Perms0), % trim some unlikely distributions to speed things up (risky)
    io:format("Checking ~p permutations of you / elephant Valve targeting", [length(Perms)]),
    Start2 = erlang:system_time(),
    Part2 = do2(Perms, Graph, DistanceLookup, "AA", ValvesWithFlow, 26), % 2455
    Stop2 = erlang:system_time(),
    io:format("Completed in ~p secs~n", [floor((Stop2 - Start2) / 100000000) / 10]),
    io:format("Part 2 = ~p~n~n", [Part2]).
