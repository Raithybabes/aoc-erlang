-module(aoc23).

-export([data/1, data/2]).
-export([run/0]).

data(Num) -> data(Num, fun dat:nop/1).

data(Num, Processor) -> dat:map_data("../src/dat_2023/dat_" ++ Num, Processor).

expected() ->
    [
        {aoc23_01, {54951,55218}}
    ].

run() -> aoc_runner:expect(expected()).
