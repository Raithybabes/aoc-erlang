-module(aoc22).

-export([data/1, data/2]).
-export([run/0]).

data(Num) -> data(Num, fun dat:nop/1).

data(Num, Processor) -> dat:map_data("../src/dat_2022/dat_" ++ Num, Processor).

expected() ->
    [
        {aoc22_01, {68787, 198041}},
        {aoc22_02, {15422, 15442}},
        {aoc22_03, {8105, 2363}},
        {aoc22_04, {605, 914}},
        {aoc22_05, {"QNNTGTPFN", "GGNPJBTTR"}},
        {aoc22_06, {1912, 2122}},
        {aoc22_07, {1501149, 10096985}},
        {aoc22_08, {1849, 201600}},
        {aoc22_09, {5878, 2405}},
        {aoc22_10, {13180, "EZFCHJAB"}},
        {aoc22_11, {50172, 11614682178}},
        {aoc22_12, {440, 439}},
        {aoc22_13, {-1, -1}},
        {aoc22_14, {-1, -1}},
        {aoc22_15, {-1, -1}},
        {aoc22_16, {-1, -1}},
        {aoc22_17, {-1, -1}},
        {aoc22_18, {-1, -1}},
        {aoc22_19, {-1, -1}},
        {aoc22_20, {-1, -1}},
        {aoc22_21, {-1, -1}},
        {aoc22_22, {-1, -1}},
        {aoc22_23, {-1, -1}},
        {aoc22_24, {-1, -1}},
        {aoc22_25, {-1, -1}}
    ].

run() -> aoc_runner:expect(expected()).
