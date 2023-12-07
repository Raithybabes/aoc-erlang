-module(aoc23).

-export([data/1, data/2]).
-export([run/0]).

data(Num) -> data(Num, fun dat:nop/1).

data(Num, Processor) -> dat:map_data("../src/dat_2023/dat_" ++ Num, Processor).

expected() ->
    [
        {aoc23_01, {54951, 55218}},
        {aoc23_02, {2727, 56580}},
        {aoc23_03, {556057, 82824352}},
        {aoc23_04, {32001, 5037841}},
        {aoc23_05, {621354867, -1}},
        {aoc23_06, {170000, 20537782}}
    ].

run() -> aoc_runner:expect(expected()).
