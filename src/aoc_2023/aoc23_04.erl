-module(aoc23_04).

-export([answer/0]).

card_score({card, _, _, _, 0}) -> 0;
card_score({card, _, _, _, NumMatches}) -> trunc(math:pow(2, NumMatches - 1)).

part_1(Cards) ->
    Scores = lists:map(fun card_score/1, Cards),
    lists:sum(Scores).

% There will be multiple parallel carry-overs.
% Take the head of each (total = BroughtForward), and continue to carry forward the tails
apply_carry(CarryOvers) -> apply_carry(CarryOvers, 0, []).
%
apply_carry([], BroughtForward, CarryForward) ->
    {BroughtForward, CarryForward};
apply_carry([[] | Rest], BroughtForward, CarryForward) ->
    apply_carry(Rest, BroughtForward, CarryForward);
apply_carry([[Keep | Carry] | Rest], BroughtForward, CarryForward) ->
    apply_carry(Rest, BroughtForward + Keep, [Carry | CarryForward]).

check_card(Cards) -> check_card(Cards, [], 0).
%
check_card([], _, Count) ->
    Count;
check_card([NumberMatched | Pile], CarryOvers, Count) ->
    {BroughtForward, CarryForward} = apply_carry(CarryOvers),
    NumberOfThisCard = 1 + BroughtForward,
    NewCarryForward = lists:duplicate(NumberMatched, NumberOfThisCard),
    check_card(Pile, [NewCarryForward | CarryForward], Count + NumberOfThisCard).

part_2(Cards) ->
    CardMatches = [Matches || {card, _, _, _, Matches} <- Cards],
    check_card(CardMatches).

process_dat(X) -> binary_to_list(X).

process_card(Line) ->
    [Raw1, Raw2, Raw3] = dat:extract(Line, "^Card\\s+(\\d+):([\\d ]+) \\| ([\\d+ ]+)$"),
    CardId = list_to_integer(Raw1),
    WinningNumbers = dat:to_integer_list(Raw2),
    YourNumbers = dat:to_integer_list(Raw3),
    NumberMatched = length(YourNumbers) - length(YourNumbers -- WinningNumbers),
    {card, CardId, WinningNumbers, YourNumbers, NumberMatched}.

process_cards(Dat) -> [process_card(Line) || Line <- Dat].

answer() ->
    Dat = aoc23:data("04", fun process_dat/1),
    Cards = process_cards(Dat),
    Part1 = part_1(Cards),
    Part2 = part_2(Cards),
    {Part1, Part2}.
