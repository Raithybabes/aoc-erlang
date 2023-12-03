-module(aoc23_03).

-export([answer/0]).

% Data input safe assumptions:
% All lines contain at least one number
% All lines are 140 characters long
% There are 140 lines
% The symbols are: -@*/&#%+=$
% The part numbers only have 1, 2 or 3 digits
% No parts are associated with more than 1 gear (nice!)

% I will use Row,Col co-ordinate system (Y, X)

-define(MIN, 1).
-define(MAX, 140).
-define(SYMBOLS, [$-, $@, $*, $/, $&, $#, $%, $+, $=, $$]).

is_symbol(X) -> lists:member(X, ?SYMBOLS).

location_is_symbol(Y, X, _) when (X < ?MIN) or (X > ?MAX) or (Y < ?MIN) or (Y > ?MAX) -> false;
location_is_symbol(Y, X, Map) -> is_symbol(lists:nth(X, lists:nth(Y, Map))).

location_is_gear(Y, X, _) when (X < ?MIN) or (X > ?MAX) or (Y < ?MIN) or (Y > ?MAX) -> [];
location_is_gear(Y, X, Map) ->
    case lists:nth(X, lists:nth(Y, Map)) == $* of
        true -> {gear, Y, X};
        false -> []
    end.

check_around_position(Fun, Y, X, Map) ->
    [
        Fun(Y - 1, X - 1, Map),
        Fun(Y - 1, X, Map),
        Fun(Y - 1, X + 1, Map),
        Fun(Y, X - 1, Map),
        Fun(Y, X + 1, Map),
        Fun(Y + 1, X - 1, Map),
        Fun(Y + 1, X, Map),
        Fun(Y + 1, X + 1, Map)
    ].

augment_parts(FunAugmentor, Map, Matches) -> augment_parts(FunAugmentor, Map, Matches, []).
%
augment_parts(_, _, [], Result) ->
    Result;
augment_parts(FunAugmentor, Map, [Part | T], Result) ->
    augment_parts(FunAugmentor, Map, T, Result ++ [FunAugmentor(Map, Part)]).

augment_is_part_number(Map, Part) -> augment_is_part_number(Map, Part, 0, []).
%
augment_is_part_number(_, {part, Row, Col, Len, Val}, Idx, Result) when Idx == Len ->
    {part_with_flag, Row, Col, Len, Val, lists:member(true, Result)};
augment_is_part_number(Map, Part = {part, Row, Col, _Len, _Val}, Idx, Result) ->
    augment_is_part_number(
        Map,
        Part,
        Idx + 1,
        Result ++ check_around_position(fun location_is_symbol/3, Row, Col + Idx, Map)
    ).

augment_gears(Map, Part) -> augment_gears(Map, Part, 0, []).
%
augment_gears(_, {part, Row, Col, Len, Val}, Idx, Result) when Idx == Len ->
    {part_with_gears, Row, Col, Len, Val, lists:uniq(lists:flatten(Result))};
augment_gears(Map, Part = {part, Row, Col, _Len, _Val}, Idx, Result) ->
    augment_gears(
        Map,
        Part,
        Idx + 1,
        Result ++ check_around_position(fun location_is_gear/3, Row, Col + Idx, Map)
    ).

add_index(Lines) ->
    add_index(Lines, 1, []).
add_index([], _, Out) -> Out;
add_index([Line | T], Idx, Out) -> add_index(T, Idx + 1, Out ++ [[{Idx, X, L} || {X, L} <- Line]]).

extract_number(Map, {Row, Col, Len}) ->
    list_to_integer(lists:sublist(lists:nth(Row, Map), Col, Len)).

find_parts(Map) ->
    RawMatches = [re:run(Line, "\\d+", [global]) || Line <- Map],
    Matches = [lists:flatten(X) || {match, X} <- RawMatches],
    IndexedMatches = add_index(Matches),
    AllMatches = lists:flatten(IndexedMatches),
    CorrectedX = [{Y, X + 1, L} || {Y, X, L} <- AllMatches],
    Parts = [{part, Y, X, L, extract_number(Map, Match)} || Match = {Y, X, L} <- CorrectedX],
    Parts.

part_1(Map, Parts) ->
    AugmentFlag = augment_parts(fun augment_is_part_number/2, Map, Parts),
    PartNumbers = [Val || {part_with_flag, _Row, _Col, _Len, Val, true} <- AugmentFlag],
    Part1 = lists:sum(PartNumbers),
    Part1.

part_2(Map, Parts) ->
    AugmentGears = augment_parts(fun augment_gears/2, Map, Parts),
    % Reminder: spike showed that no Part has more than a single Gear
    ValGears = [{Val, Gear} || {part_with_gears, _Row, _Col, _Len, Val, [Gear]} <- AugmentGears],
    PotentialGears = lists:uniq([Gear || {_Val, Gear} <- ValGears]),
    GearVals = [
        {CheckGear,
            % filter ValGears to those which match the Gear, and map to just the Val
            lists:map(
                fun(ValGear) -> element(1, ValGear) end,
                lists:filter(fun({_Val, Gear}) -> Gear == CheckGear end, ValGears)
            )}
     || CheckGear <- PotentialGears
    ],
    GearRatios = [A * B || {_, [A, B]} <- GearVals],
    Part2 = lists:sum(GearRatios),
    Part2.

process_dat(X) -> binary_to_list(X).

answer() ->
    Dat = aoc23:data("03", fun process_dat/1),
    Map = Dat,
    Parts = find_parts(Map),
    Part1 = part_1(Map, Parts),
    Part2 = part_2(Map, Parts),
    {Part1, Part2}.
