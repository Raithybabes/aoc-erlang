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
    ElfCalories = lists:sort(subtotals(Dat)),
    io:format("~p Elves~n", [length(ElfCalories)]),
    TopThreeCalories = rwm:last(3, ElfCalories),
    MaxCalories = rwm:last(TopThreeCalories),
    {MaxCalories, lists:sum(TopThreeCalories)}.
