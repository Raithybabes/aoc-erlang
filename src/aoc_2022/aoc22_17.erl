-module(aoc22_17).

-include("../rwm_lib/macros.hrl").

-export([answer/0]).

-define(air, ' ').
-define(block, '#').
-define(oob, oob).
-define(width, 7).
-define(max_x, 6).

% ####

% .#.
% ###
% .#.

% ..#
% ..#
% ###

% #
% #
% #
% #

% ##
% ##

shape(1) -> [{0, 0}, {1, 0}, {2, 0}, {3, 0}];
shape(2) -> [{1, 0}, {0, 1}, {1, 1}, {2, 1}, {1, 2}];
shape(3) -> [{0, 0}, {1, 0}, {2, 0}, {2, 1}, {2, 2}];
shape(4) -> [{0, 0}, {0, 1}, {0, 2}, {0, 3}];
shape(5) -> [{0, 0}, {1, 0}, {0, 1}, {1, 1}].

blast_direction('<') -> {-1, 0};
blast_direction('>') -> {1, 0}.

drop_direction() -> {0, -1}.

well_new() ->
    array:from_list([array:new({default, ?air}) || _I <- lists:seq(1, ?width)], {default, ?oob}).

well_get(X, _Y, _Well) when (X < 0) or (X > ?max_x) -> ?oob;
well_get(X, Y, Well) -> array:get(Y, array:get(X, Well)).

well_height(Well) ->
    Columns = array:to_list(Well),
    lists:max([array:size(A) || A <- Columns]).

well_set(X, Y, Well) ->
    New_Column = array:set(Y, ?block, array:get(X, Well)),
    array:set(X, New_Column, Well).

check_shape_direction(_Well, _ShapeIdx, {_X, 0}, {_Xoff, -1}) ->
    false;
check_shape_direction(Well, ShapeIdx, {X, Y}, {Xoff, Yoff}) ->
    ShapeOffset = [{X + SX + Xoff, Y + SY + Yoff} || {SX, SY} <- shape(ShapeIdx)],
    % io:format("Checking if shape ~p can move from ~p,~p by ~p,~p~n", [ShapeIdx, X, Y, Xoff, Yoff]),
    Checks = [well_get(WX, WY, Well) || {WX, WY} <- ShapeOffset],
    lists:all(fun(V) -> V =:= ?air end, Checks).

set_shape(Well, ShapeIdx, {X, Y}) ->
    ToSet = [{X + SX, Y + SY} || {SX, SY} <- shape(ShapeIdx)],
    Well1 = lists:foldl(fun({WX, WY}, Acc) -> well_set(WX, WY, Acc) end, Well, ToSet),
    Well1.

effective_offset(Well, ShapeIdx, {X, Y}, TestOffset) ->
    IsClear = check_shape_direction(Well, ShapeIdx, {X, Y}, TestOffset),
    ?CASE(IsClear, TestOffset, {0, 0}).

inc_shape(V) when V =:= 5 -> 1;
inc_shape(V) -> V + 1.

cycle_through_jets(Jets, Idx) when Idx >= length(Jets) -> 1;
cycle_through_jets(_Jets, Idx) -> Idx + 1.

is_zero(V) when V =:= 0 -> true;
is_zero(_) -> false.

play(Funtil, Well, Jets) -> play_shape(Funtil, Well, 0, {0, 0}, Jets, 1, {0, 0}).

play_shape(Funtil, Well, 0, {_, _}, Jets, JetIdx, Stats) ->
    % start the first shape
    play_shape(Funtil, Well, 1, {2, well_height(Well) + 3}, Jets, JetIdx, Stats);
play_shape(Funtil, Well, ShapeIdx, {X, Y}, Jets, JetIdx, Stats) ->
    % across - updates X
    io:format("Checking jet ~p (~p)~n", [JetIdx, lists:nth(JetIdx, Jets)]),
    {Xmoved, 0} = effective_offset(
        Well, ShapeIdx, {X, Y}, blast_direction(lists:nth(JetIdx, Jets))
    ),
    X1 = X + Xmoved,

    % down - updates Y / IsLanded
    {0, Ymoved} = _DropOffset = effective_offset(Well, ShapeIdx, {X1, Y}, drop_direction()),
    Y1 = Y + Ymoved,

    % check if landed
    IsLanded = is_zero(Ymoved),

    {CountShapes, CountIterations} = Stats,

    io:format("prev Stats / IsLanded = ~p ~p~n", [Stats, IsLanded]),

    case IsLanded of
        true ->
            % stats
            Stats1 = {CountShapes + 1, CountIterations + 1},
            Well1 = set_shape(Well, ShapeIdx, {X1, Y1}),
            IsFinished = Funtil(Stats1, well_height(Well1)),
            case IsFinished of
                true ->
                    % end
                    {Well1, Stats1};
                false ->
                    % next shape
                    play_shape(
                        Funtil,
                        Well1,
                        inc_shape(ShapeIdx),
                        {2, well_height(Well1) + 3},
                        Jets,
                        cycle_through_jets(Jets, JetIdx),
                        Stats1
                    )
            end;
        false ->
            % continue playing
            Stats1 = {CountShapes, CountIterations + 1},
            play_shape(
                Funtil, Well, ShapeIdx, {X1, Y1}, Jets, cycle_through_jets(Jets, JetIdx), Stats1
            )
    end.

process_dat(Line) ->
    Chars = dat:match_global(Line, "."),
    [list_to_atom(C) || C <- Chars].

show_well(Well, From) ->
    Cols = [array:to_list(Col) || Col <- array:to_list(Well)],
    [lists:sublist(C, From, 16) || C <- Cols].

% I have 10091 jet impulses
% I have 5 shapes
% I have 50,455 iterations until the pattern repeats

answer() ->
    Dat = aoc22:data("17", fun process_dat/1),
    Jets = Dat,
    % io:format("Jets~n~p~n~n", [Jets]),
    % {FinalWell, FinalStats} = play(fun({CountShapes}, Height) -> CountShapes =:= 2022 end, well_new(), Jets), % well height = 3202
    {FinalWell, FinalStats} = play(
        % fun({CountShapes, CountIterations}, Height) -> (CountIterations > 10103) end, well_new(), Jets
        fun({CountShapes, _CountIterations}, _Height) -> CountShapes =:= 1010 end,
        well_new(),
        Jets
        % fun({CountShapes, CountIterations}, Height) -> (CountShapes rem 5 =:= 0) and (CountIterations rem 10091 =:= 0) end, well_new(), Jets
    ),
    WellHeight = well_height(FinalWell),
    io:format("Well Bottom~n~p~n~n", [show_well(FinalWell, 1)]),
    io:format("Well Top~n~p~n~n", [show_well(FinalWell, max(1, WellHeight - 15))]),
    io:format("Well height ~p~n~nStats ~p~n~n", [WellHeight, FinalStats]).
