-module(aoc22_18).

-export([answer/0]).

process_dat(Line) -> dat:to_integer_list(Line).

answer() ->
    Dat = aoc22:data("18", fun process_dat/1),
    {-1, -1}.
