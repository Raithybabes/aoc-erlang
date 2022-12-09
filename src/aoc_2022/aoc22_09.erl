-module(aoc22_09).

-export([answer/0]).

process_dat(Line) -> 
    [Dir, Num] = dat:extract(Line, "(\\w) (\\d)"),
    {list_to_atom(Dir), list_to_integer(Num)}.

apply_move('U', {X, Y}) -> {X, Y + 1};
apply_move('D', {X, Y}) -> {X, Y - 1};
apply_move('L', {X, Y}) -> {X - 1, Y};
apply_move('R', {X, Y}) -> {X + 1, Y}.

apply_move_times_cb(_, XY, _, Acc, 0) -> {XY, Acc};
apply_move_times_cb(Dir, XY, F, Acc, Times) -> 
    XY1 = apply_move(Dir, XY),
    apply_move_times_cb(Dir, XY1, F, F(XY1, Acc), Times - 1).

apply_step_cb({Dir, Num}, XY, F, Acc) -> apply_move_times_cb(Dir, XY, F, Acc, Num).

apply_steps_cb([], Pos, _, Acc) -> {Pos, Acc};
apply_steps_cb([Step|Rest], Pos, F, Acc) -> 
    {XY1, Acc1} = apply_step_cb(Step, Pos, F, Acc),
    apply_steps_cb(Rest, XY1, F, Acc1).

cb_determine_bounds({X, Y}, {MinX, MaxX, MinY, MaxY}) ->
    {min(MinX, X), max(MaxX, X), min(MinY, Y), max(MaxY, Y)}.

cb_follow(Head, {{TX, TY} = Tail, Grid}) ->
    Grid1 = grid:set(TX, TY, 1, Grid),
    {TX1, TY1} = tail_follows_head(Head, Tail),
    Grid2 = grid:set(TX1, TY1, 1, Grid1),
    {{TX1, TY1}, Grid2}.

tail_follows_head({HX, HY}, {TX, TY}) when (abs(HX - TX) > 1) or (abs(HY - TY) > 1) ->
    {TX + towards(TX, HX), TY + towards(TY, HY)};
tail_follows_head(_Head, Tail) -> Tail.

towards(From, To) when To > From -> 1;
towards(From, To) when To < From -> -1;
towards(_, _) -> 0.

total_grid({_X, _Y, V, Grid, Acc}) -> {Grid, Acc + V}.

answer() ->
    Dat = aoc22:data("09", fun process_dat/1),
    {_, {MinX0, _MaxX0, MinY0, _MaxY0}} = apply_steps_cb(Dat, {0,0}, fun cb_determine_bounds/2, {0, 0, 0, 0}),
    OffsetStart = {-MinX0, -MinY0},
    {_, {_MinX1, MaxX1, _MinY1, MaxY1} = Bounds} = apply_steps_cb(Dat, OffsetStart, fun cb_determine_bounds/2, {0, 0, 0, 0}),
    io:format("Bounds ~p~nOffset ~p~n", [Bounds, OffsetStart]),
    Grid = grid:new(MaxX1 + 1, MaxY1 + 1),
    {Pos, {Tail, Grid1}} = apply_steps_cb(Dat, OffsetStart, fun cb_follow/2, {OffsetStart, Grid}),
    io:format("Pos ~p~nTail ~p~n", [Pos, Tail]),
    {_Grid1, Part1} = grid:process_ext(fun total_grid/1, Grid1, 0),
    % grid:to_list2d(_Grid1)
    Part1
    .
