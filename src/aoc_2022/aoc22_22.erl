-module(aoc22_22).

-export([answer/0]).

process_dat(Line) -> dat:to_integer_list(Line).

answer() ->
    Dat = aoc22:data("22", fun process_dat/1),
    {-1, -1}.
