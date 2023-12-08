-module(aoc23_07).

-export([answer/0]).
-export([compare_hands/2, contest_hands/2]).

% I'm not proud of this. Lots of duplication. But it remains quite declarative.

prepend_tuple(V, T) ->
    list_to_tuple([V | tuple_to_list(T)]).

add_ordinal_to_list_of_tuples(Tuples) ->
    add_ordinal_to_list_of_tuples(Tuples, 1, []).
add_ordinal_to_list_of_tuples([], _, Result) ->
    Result;
add_ordinal_to_list_of_tuples([Tuple | Rest], Ordinal, Result) ->
    TupleWithOrdinal = prepend_tuple(Ordinal, Tuple),
    add_ordinal_to_list_of_tuples(Rest, Ordinal + 1, [TupleWithOrdinal | Result]).

card_value1($A) -> 14;
card_value1($K) -> 13;
card_value1($Q) -> 12;
card_value1($J) -> 11;
card_value1($T) -> 10;
card_value1(X) when (X >= $1) and (X =< $9) -> X - $0;
card_value1(_) -> error(bad_card_input).

hand1(Cards) -> [card_value1(Card) || Card <- Cards].

shand(Cards) -> lists:sort(Cards).

% score for a shand (sorted hand)
shand_type([A, A, A, A, A]) -> 7;
shand_type([A, A, A, A, _]) -> 6;
shand_type([_, A, A, A, A]) -> 6;
shand_type([A, A, A, B, B]) -> 5;
shand_type([A, A, B, B, B]) -> 5;
shand_type([A, A, A, _, _]) -> 4;
shand_type([_, A, A, A, _]) -> 4;
shand_type([_, _, A, A, A]) -> 4;
shand_type([A, A, B, B, _]) -> 3;
shand_type([A, A, _, B, B]) -> 3;
shand_type([_, A, A, B, B]) -> 3;
shand_type([A, A, _, _, _]) -> 2;
shand_type([_, A, A, _, _]) -> 2;
shand_type([_, _, A, A, _]) -> 2;
shand_type([_, _, _, A, A]) -> 2;
shand_type([_A, _B, _C, _D, _E]) -> 1.

card_value_with_jokers($A) -> 14;
card_value_with_jokers($K) -> 13;
card_value_with_jokers($Q) -> 12;
card_value_with_jokers($J) -> 1;
card_value_with_jokers($T) -> 10;
card_value_with_jokers(X) when (X >= $1) and (X =< $9) -> X - $0;
card_value_with_jokers(_) -> error(bad_card_input).

hand2(Cards) -> [card_value_with_jokers(Card) || Card <- Cards].

shand_type_with_jokers([A, A, A, A, A], 0) -> 7;
shand_type_with_jokers([A, A, A, A, _], 0) -> 6;
shand_type_with_jokers([_, A, A, A, A], 0) -> 6;
shand_type_with_jokers([A, A, A, B, B], 0) -> 5;
shand_type_with_jokers([A, A, B, B, B], 0) -> 5;
shand_type_with_jokers([A, A, A, _, _], 0) -> 4;
shand_type_with_jokers([_, A, A, A, _], 0) -> 4;
shand_type_with_jokers([_, _, A, A, A], 0) -> 4;
shand_type_with_jokers([A, A, B, B, _], 0) -> 3;
shand_type_with_jokers([A, A, _, B, B], 0) -> 3;
shand_type_with_jokers([_, A, A, B, B], 0) -> 3;
shand_type_with_jokers([A, A, _, _, _], 0) -> 2;
shand_type_with_jokers([_, A, A, _, _], 0) -> 2;
shand_type_with_jokers([_, _, A, A, _], 0) -> 2;
shand_type_with_jokers([_, _, _, A, A], 0) -> 2;
shand_type_with_jokers([_, _, _, _, _], 0) -> 1;
%
shand_type_with_jokers([A, A, A, A], 1) -> 7;
shand_type_with_jokers([A, A, A, _], 1) -> 6;
shand_type_with_jokers([_, A, A, A], 1) -> 6;
shand_type_with_jokers([A, A, B, B], 1) -> 5;
shand_type_with_jokers([A, A, _, _], 1) -> 4;
shand_type_with_jokers([_, A, A, _], 1) -> 4;
shand_type_with_jokers([_, _, A, A], 1) -> 4;
% Joker will not upgrade a single pair hand to 2 pairs - will go to 3 of a kind instead
shand_type_with_jokers([_, _, _, _], 1) -> 2;
%
shand_type_with_jokers([A, A, A], 2) -> 7;
shand_type_with_jokers([A, A, _], 2) -> 6;
shand_type_with_jokers([_, A, A], 2) -> 6;
shand_type_with_jokers([_, _, _], 2) -> 4;
%
shand_type_with_jokers([A, A], 3) -> 7;
shand_type_with_jokers([_, _], 3) -> 6;
%
shand_type_with_jokers([_], 4) -> 7;
%
shand_type_with_jokers([], 5) -> 7.

shand_type2(H) ->
    HandWithoutJokers = H -- "JJJJJ",
    NumberOfJokers = length(H) - length(HandWithoutJokers),
    shand_type_with_jokers(HandWithoutJokers, NumberOfJokers).

% compare two UNsorted hands
compare_hands([S1, S2, S3, S4, S5], [S1, S2, S3, S4, S5]) -> error(matching_hands);
compare_hands([S1, S2, S3, S4, A], [S1, S2, S3, S4, B]) -> (A =< B);
compare_hands([S1, S2, S3, A, _], [S1, S2, S3, B, _]) -> (A =< B);
compare_hands([S1, S2, A, _, _], [S1, S2, B, _, _]) -> (A =< B);
compare_hands([S1, A, _, _, _], [S1, B, _, _, _]) -> (A =< B);
compare_hands([A, _, _, _, _], [B, _, _, _, _]) -> (A =< B).

contest_hands({H1, _}, {H2, _}) ->
    contest_hands(H1, H2, shand_type(shand(H1)), shand_type(shand(H2))).
contest_hands(H1, H2, T, T) -> compare_hands(hand1(H1), hand1(H2));
contest_hands(_, _, T1, T2) -> T1 =< T2.

contest_hands2({H1, _}, {H2, _}) ->
    contest_hands2(H1, H2, shand_type2(shand(H1)), shand_type2(shand(H2))).
contest_hands2(H1, H2, T, T) -> compare_hands(hand2(H1), hand2(H2));
contest_hands2(_, _, T1, T2) -> T1 =< T2.

part_1(Cards) ->
    Sorted = lists:sort(fun contest_hands/2, Cards),
    Indexed = add_ordinal_to_list_of_tuples(Sorted),
    Part1 = lists:sum([Rank * Bid || {Rank, _, Bid} <- Indexed]),
    Part1.

part_2(Cards) -> 
    Sorted = lists:sort(fun contest_hands2/2, Cards),
    Indexed = add_ordinal_to_list_of_tuples(Sorted),
    Part2 = lists:sum([Rank * Bid || {Rank, _, Bid} <- Indexed]),
    Part2.

process_dat(X) ->
    [Hand, Bid] = dat:extract(X, "^(.+) (.+)$"),
    {Hand, list_to_integer(Bid)}.

answer() ->
    Cards = aoc23:data("07", fun process_dat/1),
    Part1 = part_1(Cards),
    Part2 = part_2(Cards),
    {Part1, Part2}.
