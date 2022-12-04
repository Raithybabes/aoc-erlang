-module(aoc22_17).

-export([answer/0]).

process_dat(Line) -> dat:to_integer_list(Line).

answer() ->
    Dat = aoc22:data("17", fun process_dat/1),
    {-1, -1}.
