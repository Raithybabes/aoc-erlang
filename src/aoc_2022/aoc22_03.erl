-module(aoc22_03).

-export([answer/0]).

group(L) -> group(L, []).
group([], Groups) -> Groups;
group([G1, G2, G3 | T], Groups) -> group(T, [[G1, G2, G3] | Groups]).

process_dat(X) -> binary_to_list(X).

priority(A) -> length(lists:takewhile(fun (V) -> V =/= hd(A) end, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")) + 1.

sum_priorities(L) -> lists:sum([priority(A) || A <- L]).

answer() ->
    Dat = aoc22:data("03", fun process_dat/1),
    Compartments = [lists:split(floor(length(Rucksack) / 2), Rucksack) || Rucksack <- Dat],
    RucksackIntersects = [rwm:intersect(List1, List2) || {List1, List2} <- Compartments],
    GroupIntersects = [rwm:intersect(Group) || Group <- group(Dat)],
    {sum_priorities(RucksackIntersects), sum_priorities(GroupIntersects)}.
