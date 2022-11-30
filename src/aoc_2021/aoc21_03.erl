-module(aoc21_03).

-export([answer/0]).

calculate_power_consumption(Dat) ->
    Counts = bits:count(Dat),
    Gamma = bits:most_occurring(Counts),
    Epsilon = bits:least_occurring(Counts),
    bits:value(Gamma) * bits:value(Epsilon).

calculate_life_support_rating(Dat) ->
    Oxygen = find_final_using(fun bits:most_occurring/1, Dat),
    Carbon = find_final_using(fun bits:least_occurring/1, Dat),
    bits:value(Oxygen) * bits:value(Carbon).

find_final_using(Algo, Dat) ->
    find_final_using(Algo, Dat, 1).
find_final_using(_, [Final], _) ->
    Final;
find_final_using(Algo, Dat, Idx) ->
    Counts = bits:count(Dat),
    Target = Algo(Counts),
    find_final_using(
        Algo,
        lists:filter(fun(Bits) -> lists:nth(Idx, Bits) == lists:nth(Idx, Target) end, Dat),
        Idx + 1
    ).

answer() ->
    Dat = aoc21:data("03", fun dat:to_bits/1),
    {calculate_power_consumption(Dat), calculate_life_support_rating(Dat)}.
