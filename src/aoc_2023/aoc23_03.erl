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

% Generic - extract these to lib

% e.g. "123 45 6" -> [{0, 3}, {4, 2}, {7, 1}]
rx_find_numbers(String) ->
    {match, Matches} = re:run(String, "\\d+", [global]),
    lists:flatten(Matches).

prepend_tuple(V, T) ->
    list_to_tuple([V | tuple_to_list(T)]).

% e.g. [[{a}, {b}], [{c}, {d}]] -> [[{1, a}, {1, b}], [{2, c}, {2, d}]]
add_ordinal_to_lists_of_tuples(LinesOfTuples) ->
    add_ordinal_to_lists_of_tuples(LinesOfTuples, 1, []).
add_ordinal_to_lists_of_tuples([], _, Result) ->
    Result;
add_ordinal_to_lists_of_tuples([LineOfTuples | Rest], Ordinal, Result) ->
    TuplesWithOrdinal = [prepend_tuple(Ordinal, T) || T <- LineOfTuples],
    add_ordinal_to_lists_of_tuples(Rest, Ordinal + 1, [TuplesWithOrdinal | Result]).

% --

is_symbol(X) -> lists:member(X, ?SYMBOLS).

location_is_symbol(Y, X, _) when (X < ?MIN) or (X > ?MAX) or (Y < ?MIN) or (Y > ?MAX) -> false;
location_is_symbol(Y, X, Grid) -> is_symbol(lists:nth(X, lists:nth(Y, Grid))).

location_is_gear(Y, X, _) when (X < ?MIN) or (X > ?MAX) or (Y < ?MIN) or (Y > ?MAX) -> [];
location_is_gear(Y, X, Grid) ->
    case lists:nth(X, lists:nth(Y, Grid)) == $* of
        true -> {gear, Y, X};
        false -> []
    end.

check_around_position(Fun, Y, X, Grid) ->
    [
        Fun(Y - 1, X - 1, Grid),
        Fun(Y - 1, X, Grid),
        Fun(Y - 1, X + 1, Grid),
        Fun(Y, X - 1, Grid),
        Fun(Y, X + 1, Grid),
        Fun(Y + 1, X - 1, Grid),
        Fun(Y + 1, X, Grid),
        Fun(Y + 1, X + 1, Grid)
    ].

augment_parts(FunAugmentor, Grid, Matches) -> augment_parts(FunAugmentor, Grid, Matches, []).
%
augment_parts(_, _, [], Result) ->
    Result;
augment_parts(FunAugmentor, Grid, [Part | T], Result) ->
    augment_parts(FunAugmentor, Grid, T, Result ++ [FunAugmentor(Grid, Part)]).

augment_is_part_number(Grid, Part) -> augment_is_part_number(Grid, Part, 0, []).
%
augment_is_part_number(_, {part, Row, Col, Len, Val}, Idx, Result) when Idx == Len ->
    {part_with_flag, Row, Col, Len, Val, lists:member(true, Result)};
augment_is_part_number(Grid, Part = {part, Row, Col, _Len, _Val}, Idx, Result) ->
    augment_is_part_number(
        Grid,
        Part,
        Idx + 1,
        Result ++ check_around_position(fun location_is_symbol/3, Row, Col + Idx, Grid)
    ).

augment_gears(Grid, Part) -> augment_gears(Grid, Part, 0, []).
%
augment_gears(_, {part, Row, Col, Len, Val}, Idx, Result) when Idx == Len ->
    {part_with_gears, Row, Col, Len, Val, lists:uniq(lists:flatten(Result))};
augment_gears(Grid, Part = {part, Row, Col, _Len, _Val}, Idx, Result) ->
    augment_gears(
        Grid,
        Part,
        Idx + 1,
        Result ++ check_around_position(fun location_is_gear/3, Row, Col + Idx, Grid)
    ).

extract_number_from_grid(Grid, {Row, Col, Len}) ->
    list_to_integer(lists:sublist(lists:nth(Row, Grid), Col, Len)).

process_parts__correct_col_to_one_based({Row, Col, Len}) -> {Row, Col + 1, Len}.

process_parts__make_part(Grid, RCL = {Row, Col, Len}) ->
    {part, Row, Col, Len, extract_number_from_grid(Grid, RCL)}.

process_parts(Grid) ->
    LinesOfNumberMatches = lists:map(fun rx_find_numbers/1, Grid),
    RowAdded = add_ordinal_to_lists_of_tuples(LinesOfNumberMatches),
    Flattened = lists:flatten(RowAdded),
    ColCorrected = lists:map(fun process_parts__correct_col_to_one_based/1, Flattened),
    Parts = [process_parts__make_part(Grid, RCL) || RCL <- ColCorrected],
    Parts.

part_1(Grid, Parts) ->
    AugmentFlag = augment_parts(fun augment_is_part_number/2, Grid, Parts),
    PartNumbers = [Val || {part_with_flag, _Row, _Col, _Len, Val, true} <- AugmentFlag],
    Part1 = lists:sum(PartNumbers),
    Part1.

part_2(Grid, Parts) ->
    AugmentGears = augment_parts(fun augment_gears/2, Grid, Parts),
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
    GearRatios = [A * B || {_Gear, [A, B]} <- GearVals],
    Part2 = lists:sum(GearRatios),
    Part2.

process_dat(X) -> binary_to_list(X).

answer() ->
    Dat = aoc23:data("03", fun process_dat/1),
    Grid = Dat,
    Parts = process_parts(Grid),
    Part1 = part_1(Grid, Parts),
    Part2 = part_2(Grid, Parts),
    {Part1, Part2}.
