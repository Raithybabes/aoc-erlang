-module(aoc23_01).

-export([answer/0]).

value_from_extracted_digits_mapper(FunExtractor) ->
    fun(Input) ->
        Extracted = FunExtractor(Input),
        First = lists:nth(1, Extracted),
        Last = lists:nth(length(Extracted), Extracted),
        list_to_integer([First, Last])
    end.

extract1(Input) -> extract1(Input, []).
%
extract1([], Out) -> Out;
extract1([H | T], Out) when (H >= $0) and (H =< $9) -> extract1(T, Out ++ [H]);
extract1([_ | T], Out) -> extract1(T, Out).

% retain last letter of buffer because twone = 21, sevenine = 79, eightwo = 82, etc.!!!
extract2(Input) -> extract2(Input, [], []).
%
extract2([], Out, _) -> Out;
extract2([H = $e | T], Out, Buf = [$n, $o | _]) -> extract2(T, Out ++ [$1], [H | Buf]);
extract2([H = $o | T], Out, Buf = [$w, $t | _]) -> extract2(T, Out ++ [$2], [H | Buf]);
extract2([H = $e | T], Out, Buf = [$e, $r, $h, $t | _]) -> extract2(T, Out ++ [$3], [H | Buf]);
extract2([H = $r | T], Out, Buf = [$u, $o, $f | _]) -> extract2(T, Out ++ [$4], [H | Buf]);
extract2([H = $e | T], Out, Buf = [$v, $i, $f | _]) -> extract2(T, Out ++ [$5], [H | Buf]);
extract2([H = $x | T], Out, Buf = [$i, $s | _]) -> extract2(T, Out ++ [$6], [H | Buf]);
extract2([H = $n | T], Out, Buf = [$e, $v, $e, $s | _]) -> extract2(T, Out ++ [$7], [H | Buf]);
extract2([H = $t | T], Out, Buf = [$h, $g, $i, $e | _]) -> extract2(T, Out ++ [$8], [H | Buf]);
extract2([H = $e | T], Out, Buf = [$n, $i, $n | _]) -> extract2(T, Out ++ [$9], [H | Buf]);
extract2([H | T], Out, _) when (H >= $0) and (H =< $9) -> extract2(T, Out ++ [H], []);
extract2([H | T], Out, Buf) -> extract2(T, Out, [H | Buf]).

process_dat(X) -> binary_to_list(X).

answer() ->
    Dat = aoc23:data("01", fun process_dat/1),
    % 142
    Part1 = lists:sum(
        lists:map(value_from_extracted_digits_mapper(fun extract1/1), Dat)
    ),
    % 281
    Part2 = lists:sum(
        lists:map(value_from_extracted_digits_mapper(fun extract2/1), Dat)
    ),
    {Part1, Part2}.
