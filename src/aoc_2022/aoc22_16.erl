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

do(Graph, Distances, Valve, VWF) -> do(Graph, Distances, Valve, VWF, VWF, 0, 0, 30).

do(_Graph, _Distances, _Valve, [], _VWF, Total, MaxPressure, _Mins) -> max(Total, MaxPressure);
do(_Graph, _Distances, _Valve, _, _VWF, Total, MaxPressure, Mins) when Mins =:= 0 -> max(Total, MaxPressure);
do(_Graph, _Distances, _Valve, _, _VWF, _Total, MaxPressure, Mins) when Mins < 0 -> MaxPressure;
do(Graph, Distances, Valve, [{TV, TF} = V2|VWF], VWF2, Total, MaxPressure, Mins) ->
    NewMins = Mins - maps:get({Valve, TV}, Distances) - 1, % - 1 (don't include starting node) + 1 (extra minute to turn the valve on)
    NewTotal = Total + NewMins * TF,
    NewMaxPressure = do(Graph, Distances, TV, VWF2 -- [V2], VWF2 -- [V2], NewTotal, MaxPressure, NewMins),
    do(Graph, Distances, Valve, VWF, VWF2, Total, NewMaxPressure, Mins)
    .

sort_flows({_, F1}, {_, F2}) -> F1 >= F2.

answer() ->
    Dat = aoc22:data("16", fun process_dat/1),
    % ValvesLookup = maps:from_list([{V, Valve} || {V, _, _} = Valve <- Dat]),
    Graph = build_graph(Dat),
    Valves = [{V, F} || {V, F, _} <- Dat],
    ValvesWithFlow = lists:sort(fun sort_flows/2, lists:filter(fun({_, FlowRate}) -> FlowRate > 0 end, Valves)),
    DistanceLookup = maps:from_list([{{VA, VB}, length(digraph:get_short_path(Graph, VA, VB)) - 1} || {VA, _} <- Valves, {VB, _} <- Valves]),
    Start = erlang:system_time(),
    Part1 = do(Graph, DistanceLookup, "AA", ValvesWithFlow), % 1701
    Stop = erlang:system_time(),
    io:format("Completed in ~p secs~n", [floor((Stop - Start) / 100000000) / 10]),
    Part1
.