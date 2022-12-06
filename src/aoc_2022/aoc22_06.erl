-module(aoc22_06).

-export([answer/0]).

find_uniq_seq(List, Length) -> find_uniq_seq(List, Length, [], 0).
find_uniq_seq([H | T], Length, Buffer, Pos) when length(Buffer) < Length ->
    find_uniq_seq(T, Length, Buffer ++ [H], Pos + 1);
find_uniq_seq([H | T], Length, [_ | TBuffer] = Buffer, Pos) ->
    Unique = lists:uniq(Buffer) =:= Buffer,
    if
        Unique -> Pos;
        true -> find_uniq_seq(T, Length, TBuffer ++ [H], Pos + 1)
    end.

process_dat(Line) -> binary_to_list(Line).

answer() ->
    Dat = aoc22:data("06", fun process_dat/1),
    {find_uniq_seq(Dat, 4), find_uniq_seq(Dat, 14)}.
