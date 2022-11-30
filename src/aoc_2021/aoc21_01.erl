-module(aoc21_01).

-export([answer/0]).

count_increases([Initial | Rest]) -> count_increases(Rest, Initial, 0).

count_increases([], _, Count) ->
    Count;
count_increases([Val | Rest], Prev, Count) ->
    count_increases(
        Rest,
        Val,
        Count +
            case Val > Prev of
                true -> 1;
                false -> 0
            end
    ).

count_increases_sliding_window([A, B, C | Rest]) ->
    count_increases_sliding_window([B, C | Rest], A + B + C, 0).

count_increases_sliding_window([_, _], _, Count) ->
    Count;
count_increases_sliding_window([A, B, C | Rest], Prev, Count) ->
    Val = A + B + C,
    count_increases_sliding_window(
        [B, C | Rest],
        Val,
        Count +
            case Val > Prev of
                true -> 1;
                false -> 0
            end
    ).

answer() ->
    Dat = aoc21:data("01", fun dat:to_integer/1),
    {count_increases(Dat), count_increases_sliding_window(Dat)}.
