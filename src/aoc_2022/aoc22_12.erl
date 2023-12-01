-module(aoc22_12).

-include("../rwm_lib/macros.hrl").

-export([answer/0]).

height('S') ->
    height(a);
height('E') ->
    height(z);
height(A) ->
    length(
        lists:takewhile(fun(V) -> V =/= A end, [
            a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z
        ])
    ) + 1.

process_dat(Line) -> [list_to_atom(V) || V <- dat:match_global(Line, "\\w")].

confirm_edge({X1, Y1}, {X2, Y2}, Grid) ->
    V1 = grid:get(X1, Y1, Grid),
    V2 = grid:get(X2, Y2, Grid),
    V2 =< (V1 + 1).

shortest_path(Graph, Start, End) ->
    ShortestPath = digraph:get_short_path(Graph, Start, End),
    ?CASE(ShortestPath =:= false, null, length(ShortestPath) - 1).

shortest_amongst_paths(Graph, Starts, End) -> shortest_amongst_paths(Graph, Starts, End, null).
shortest_amongst_paths(_Graph, [], _End, Shortest) ->
    Shortest;
shortest_amongst_paths(Graph, [Start | Rest], End, Shortest) ->
    NewShortest = shortest(Shortest, shortest_path(Graph, Start, End)),
    shortest_amongst_paths(Graph, Rest, End, NewShortest).

shortest(null, B) -> B;
shortest(A, null) -> A;
shortest(A, B) when A =< B -> A;
shortest(_A, B) -> B.

answer() ->
    Dat = aoc22:data("12", fun process_dat/1),
    AtomGrid = grid:from_list2d(Dat),
    [Start] = grid:filter(fun({_X, _Y, V, _G}) -> V =:= 'S' end, AtomGrid),
    [End] = grid:filter(fun({_X, _Y, V, _G}) -> V =:= 'E' end, AtomGrid),
    HeightGrid = grid:process(fun({X, Y, V, G}) -> grid:set(X, Y, height(V), G) end, AtomGrid),
    Graph = grid:to_graph(fun confirm_edge/3, HeightGrid),
    LowPoints = grid:filter(fun({_X, _Y, V, _G}) -> V =:= 1 end, HeightGrid),
    {shortest_path(Graph, Start, End), shortest_amongst_paths(Graph, LowPoints, End)}.
