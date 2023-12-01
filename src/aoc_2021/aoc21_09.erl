-module(aoc21_09).

-include("../rwm_lib/macros.hrl").

-export([answer/0]).

-define(else, true).

test_data() ->
    [
        "2199943210",
        "3987894921",
        "9856789892",
        "8767896789",
        "9899965678"
    ].

process_dat(V) -> [list_to_integer(X) || X <- dat:match_global(V, "\\d")].

filter_low_point(X, Y, V, C) ->
    Neighbours = [NV || {_, _, NV} <- canvas2d:neighbours_adjacent(X, Y, C)],
    IsLowPoint = lists:all(fun(NV) -> V < NV end, Neighbours),
    ?CASE(IsLowPoint, {X, Y, V}, []).

expand_basin(StartX, StartY, Canvas) ->
    fun Recurse_expand({X, Y}, Basin) ->
        IsAlreadyInBasin = lists:member({X, Y}, Basin),
        case IsAlreadyInBasin of
            true ->
                Basin;
            false ->
                BasinBuddies = [
                    {NX, NY}
                 || {NX, NY, NV} <- canvas2d:neighbours_adjacent(X, Y, Canvas), NV < 9
                ],
                NewBasin = lists:foldl(
                    Recurse_expand,
                    [{X, Y} | Basin],
                    BasinBuddies
                ),
                NewBasin
        end
    end(
        {StartX, StartY}, []
    ).

answer() ->
    Dat = aoc21:data("09", fun process_dat/1),
    % Dat = [process_dat(V) || V <- test_data()],
    Map = canvas2d:from_list2d(Dat),
    LowPoints = lists:flatten(canvas2d:process(fun filter_low_point/4, Map)),

    RiskLevel = lists:sum([V + 1 || {_X, _Y, V} <- LowPoints]),

    Basins = [expand_basin(X, Y, Map) || {X, Y, _} <- LowPoints],
    BasinsSize = [length(B) || B <- Basins],
    {TopThree, _} = lists:split(3, lists:reverse(lists:sort(BasinsSize))),
    TopThreeProduct = lists:foldl(fun(A, B) -> A * B end, 1, TopThree),

    {RiskLevel, TopThreeProduct}.
