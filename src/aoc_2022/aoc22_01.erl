-module(aoc22_01).

-export([answer/0]).

subtotals(Nums) -> subtotals(Nums, 0, []).
subtotals([], 0, Subtotals) -> Subtotals;
subtotals([], Sub, Subtotals) -> [Sub | Subtotals];
subtotals([<<>> | T], Sub, Subtotals) -> subtotals(T, 0, [Sub | Subtotals]);
subtotals([H | T], Sub, Subtotals) -> subtotals(T, Sub + H, Subtotals).

process_dat(<<>>) -> <<>>;
process_dat(B) -> binary_to_integer(B).

answer() ->
    Dat = aoc22:data("01", fun process_dat/1),
    ElfCalories = lists:reverse(lists:sort(subtotals(Dat))),
    io:format("~p Elves~n", [length(ElfCalories)]),
    MaxCalories = lists:nth(1, ElfCalories),
    {TopThreeCalories, _} = lists:split(3, ElfCalories),
    {MaxCalories, lists:sum(TopThreeCalories)}.
