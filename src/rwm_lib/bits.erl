-module(bits).

-export([
    parse/1,
    value/1,
    invert/1,
    string/1,
    count/1,
    most_occurring/1,
    least_occurring/1
]).

%
parse(BitsString) ->
    lists:map(fun(Bit) -> list_to_integer([Bit]) end, BitsString).

%
value(Bits) ->
    value(Bits, 0).
value([], Total) ->
    trunc(Total);
value([Bit | Rest], Total) ->
    BitValue =
        case Bit of
            0 -> 0;
            1 -> math:pow(2, length(Rest))
        end,
    value(Rest, Total + BitValue).

%
invert(Bits) -> lists:map(fun(B) -> 1 - B end, Bits).

%
string(Bits) -> lists:flatten(lists:map(fun(B) -> integer_to_list(B) end, Bits)).

%
count(BitsData) ->
    BitWidth = length(lists:nth(1, BitsData)),
    count(BitsData, BitWidth, dat:duplicate(BitWidth, {0, 0})).
count([], _, BitCounts) ->
    BitCounts;
count([Bits | Rest], BitWidth, BitCounts) ->
    NewBitCounts = lists:map(
        fun(Idx) ->
            Bit = lists:nth(Idx, Bits),
            {ZeroCount, OneCount} = lists:nth(Idx, BitCounts),
            {ZeroCount + (1 - Bit), OneCount + Bit}
        end,
        lists:seq(1, BitWidth)
    ),
    count(Rest, BitWidth, NewBitCounts).

%
most_occurring(BitCounts) ->
    lists:map(
        fun({ZeroCount, OneCount}) ->
            case OneCount >= ZeroCount of
                true -> 1;
                false -> 0
            end
        end,
        BitCounts
    ).

least_occurring(BitCounts) ->
    invert(most_occurring(BitCounts)).
