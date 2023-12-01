-module(aoc22_08).

-include("../rwm_lib/macros.hrl").

-export([answer/0]).

check_if_visible({X, Y, V, G}) ->
    N2E = grid:neighbours_to_edges(X, Y, G),
    VisibleByEdge = [lists:all(fun({_NX, _NY, NV}) -> NV < V end, NList) || NList <- N2E],
    Visible = lists:any(fun(Visible) -> Visible =:= true end, VisibleByEdge),
    Visible.

tree_heights(Trees) -> [V || {_X, _Y, V} <- Trees].

count_visible_trees(HeightCutoff, Trees) ->
    count_visible_trees(running, HeightCutoff, tree_heights(Trees), 0).
count_visible_trees(_, _, [], Count) ->
    Count;
count_visible_trees(finished, _, _, Count) ->
    Count;
count_visible_trees(running, HeightCutoff, [Tree | Rest], Count) ->
    count_visible_trees(
        ?CASE(Tree >= HeightCutoff, finished, running), HeightCutoff, Rest, Count + 1
    ).

product([H | T]) -> lists:foldl(fun(A, B) -> A * B end, H, T).

scenic_score({X, Y, V, G}) ->
    N2E = grid:neighbours_to_edges(X, Y, G),
    NumVisibleInEachDirection = [count_visible_trees(V, NList) || NList <- N2E],
    Score = product(NumVisibleInEachDirection),
    Score.

process_dat(Line) -> dat:to_digit_list(Line).

answer() ->
    Dat = aoc22:data("08", fun process_dat/1),
    Grid = grid:from_list2d(Dat),
    VisibilityResults = grid:scan(fun check_if_visible/1, Grid),
    NumberOfTreesVisibleFromEdges = length([V || V <- VisibilityResults, V =:= true]),
    ScenicScores = grid:scan(fun scenic_score/1, Grid),
    MostScenicScore = lists:max(ScenicScores),
    {NumberOfTreesVisibleFromEdges, MostScenicScore}.
