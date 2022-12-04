-module(aoc22_04).

-export([answer/0]).

process_dat(Line) ->
    [Start1, End1, Start2, End2] = dat:to_integer_list(Line),
    {{Start1, End1}, {Start2, End2}}.

overlaps({{A1, A2}, {B1, B2}}) -> (A1 =< B2) and (A2 >= B1).

contains({{A1, A2}, {B1, B2}}) -> ((B1 >= A1) and (B2 =< A2)) or ((A1 >= B1) and (A2 =< B2)).

answer() ->
    Dat = aoc22:data("04", fun process_dat/1),
    NumContained = length(lists:filter(fun contains/1, Dat)),
    NumOverlapping = length(lists:filter(fun overlaps/1, Dat)),
    {NumContained, NumOverlapping}.
