-module(grid).

-export([new/2, new/3, from_list2d/1, get/3, set/4]).
-export([all/1, scan/2, process/2, process/3, process_ext/3, process_ext/4]).
-export([process_ext_times/4, process_ext_times/5, process_ext_until/4, process_ext_until/5]).
-export([adjacent_positions/3, adjacent_neighbours/3]).
-export([adjacent_and_diagonal_positions/3, adjacent_and_diagonal_neighbours/3]).
-export([diamond_positions/3, diamond_neighbours/3]).
-export([to_list2d/1]).

-define(else, true).
-define(neighbours_4, [{0, -1}, {-1, 0}, {1, 0}, {0, 1}]).
-define(neighbours_8, [{-1, -1}, {0, -1}, {1, -1}, {-1, 0}, {1, 0}, {-1, 1}, {0, 1}, {1, 1}]).
-define(neighbours_12, [{-1, -1}, {0, -1}, {1, -1}, {-1, 0}, {1, 0}, {-1, 1}, {0, 1}, {1, 1}, {0, -2}, {-2, 0}, {2, 0}, {0, 2}]).

new(W, H) ->
    new(W, H, 0).

new(W, H, Default) ->
    Row = array:new(W, {default, Default}),
    G = array:new(H, {default, Row}),
    {W, H, G}.

from_list2d(L) ->
    H = length(L),
    W = length(lists:nth(1, L)),
    Rows = [array:from_list(X) || X <- L],
    G = array:from_list(Rows),
    {W, H, G}.

get(X, Y, {_W, _H, G}) ->
    Row = array:get(Y, G),
    V = array:get(X, Row),
    V.

get(Positions, G) ->
    [{X, Y, get(X, Y, G)} || {X, Y} <- Positions].

set(X, Y, V, {W, H, G}) ->
    Row = array:get(Y, G),
    NewRow = array:set(X, V, Row),
    NewG = array:set(Y, NewRow, G),
    {W, H, NewG}.

all({W, H, _G}) ->
    [{X, Y} || X <- lists:seq(0, W - 1), Y <- lists:seq(0, H - 1)].

scan(F, Grid) ->
    [F({X, Y, get(X, Y, Grid), Grid}) || {X, Y} <- all(Grid)].

process(F, Grid) ->
    process(F, Grid, all(Grid)).
process(_, Grid, []) ->
    Grid;
process(F, Grid, [{X, Y} | T]) ->
    NewGrid = F({X, Y, get(X, Y, Grid), Grid}),
    process(F, NewGrid, T).

process_ext(F, Grid, Ext) ->
    process_ext(F, Grid, Ext, all(Grid)).
process_ext(_, Grid, Ext, []) ->
    {Grid, Ext};
process_ext(F, Grid, Ext, [{X, Y} | T]) ->
    {NewGrid, NewExt} = F({X, Y, get(X, Y, Grid), Grid, Ext}),
    process_ext(F, NewGrid, NewExt, T).

process_pass_thru(Whatever) -> Whatever.

process_ext_times(F, Grid, Ext, Times) ->
    process_ext_times(F, fun process_pass_thru/1, Grid, Ext, Times).
process_ext_times(_, _, Grid, Ext, 0) ->
    {Grid, Ext};
process_ext_times(F, FPostProcess, Grid, Ext, Times) ->
    Processed1 = process_ext(F, Grid, Ext),
    {NewGrid2, NewExt2} = FPostProcess(Processed1),
    process_ext_times(F, FPostProcess, NewGrid2, NewExt2, Times - 1).

process_ext_until(F, FUntil, Grid, Ext) -> process_ext_until(F, fun process_pass_thru/1, FUntil, Grid, Ext).
process_ext_until(F, FPostProcess, FUntil, Grid, Ext) ->
    Processed1 = process_ext(F, Grid, Ext),
    Processed2 = FPostProcess(Processed1),
    {NewGrid3, NewExt3, IsFinished} = FUntil(Processed2),
    if
        IsFinished -> {NewGrid3, NewExt3};
        ?else -> process_ext_until(F, FPostProcess, FUntil, NewGrid3, NewExt3)
    end.

check_bounds(X, Y, {W, H, _}) when (X >= 0) and (X < W) and (Y >= 0) and (Y < H) -> {X, Y};
check_bounds(_, _, _) -> [].
check_offsets_in_bounds(X, Y, C, Offsets) ->
    lists:flatten([
        check_bounds(X + Xoff, Y + Yoff, C)
     || {Xoff, Yoff} <- Offsets
    ]).

adjacent_positions(X, Y, Grid) ->
    check_offsets_in_bounds(X, Y, Grid, ?neighbours_4).

adjacent_neighbours(X, Y, Grid) ->
    get(adjacent_positions(X, Y, Grid), Grid).

adjacent_and_diagonal_positions(X, Y, Grid) ->
    check_offsets_in_bounds(X, Y, Grid, ?neighbours_8).

adjacent_and_diagonal_neighbours(X, Y, Grid) ->
    get(adjacent_and_diagonal_positions(X, Y, Grid), Grid).

diamond_positions(X, Y, Grid) ->
    check_offsets_in_bounds(X, Y, Grid, ?neighbours_12).

diamond_neighbours(X, Y, Grid) ->
    get(diamond_neighbours(X, Y, Grid), Grid).

to_list2d({_W, _H, G}) ->
    List = array:to_list(G),
    Rows = [array:to_list(Row) || Row <- List],
    Rows.
