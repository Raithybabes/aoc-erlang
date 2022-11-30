-module(aoc21_06).

-export([answer/0]).

age_a_day([Breeders, D1, D2, D3, D4, D5, D6, D7, D8]) ->
    [D1, D2, D3, D4, D5, D6, D7 + Breeders, D8, Breeders].

age_n_days(0, Fish) -> Fish;
age_n_days(N, Fish) -> age_n_days(N - 1, age_a_day(Fish)).

answer() ->
    Dat = aoc21:data("06", fun dat:to_integer_list/1),
    StartingPopulation = lists:map(
        fun(Age) -> length([X || X <- Dat, X =:= Age]) end, lists:seq(0, 8)
    ),
    {
        lists:sum(age_n_days(80, StartingPopulation)),
        lists:sum(age_n_days(256, StartingPopulation))
    }.
