-module(aoc22_20).

-export([answer/0, new_pos/3]).

find_in_list(V, L) -> length(lists:takewhile(fun(V2) -> V =/= V2 end, L)) + 1.

new_pos(Old, Offset, Range) -> ((Old - 1 + Offset + (10000000000 * Range)) rem Range) + 1.

mix(A) -> mix(A, A).
mix([], Mixing) ->
    Mixing;
mix([{_Idx, Val} = H | T], Mixing) ->
    Pos = find_in_list(H, Mixing),
    Mixing1 = Mixing -- [H],
    NewPos = new_pos(Pos, Val, length(Mixing1)),
    % io:format("Found ~p at ~p => ~p~n", [H, Pos, NewPos]),
    {L, R} = lists:split(NewPos - 1, Mixing1),
    Mixing2 = L ++ [H] ++ R,
    mix(T, Mixing2).

process_dat(Line) -> binary_to_integer(Line).

result(Dat, Mixed) ->
    Range = length(Dat),
    IdxZero = {find_in_list(0, Dat), 0},
    ZeroPos = find_in_list(IdxZero, Mixed),
    {_, V1} = lists:nth(new_pos(ZeroPos, 1000, Range), Mixed),
    {_, V2} = lists:nth(new_pos(ZeroPos, 2000, Range), Mixed),
    {_, V3} = lists:nth(new_pos(ZeroPos, 3000, Range), Mixed),
    V1 + V2 + V3.

part_1(Dat) ->
    Indexed = lists:enumerate(Dat),
    Mixed = mix(Indexed),
    result(Dat, Mixed).

mix_multi(A, Num) -> mix_multi(A, A, Num).
mix_multi(_Orig, Iter, 0) -> Iter;
mix_multi(Orig, Iter, Num) -> mix_multi(Orig, mix(Orig, Iter), Num - 1).

part_2(Dat) ->
    Multiplied = [V * 811589153 || V <- Dat],
    Indexed = lists:enumerate(Multiplied),
    Mixed = mix_multi(Indexed, 10),
    result(Dat, Mixed).

answer() ->
    Dat = aoc22:data("20", fun process_dat/1),
    {part_1(Dat), part_2(Dat)}.
