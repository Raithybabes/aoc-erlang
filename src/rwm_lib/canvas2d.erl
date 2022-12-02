-module(canvas2d).

-export([new/1, from_list2d/1, get/3, set/4, to_list/1]).
-export([process/2, neighbours_adjacent/3, neighbours_adjacent_and_diagonal/3]).

-define(coord, {X, Y}).
-define(canvas2d, {Width, Height, Array}).
-define(neighbours_4, [{0, -1}, {-1, 0}, {1, 0}, {0, 1}]).
-define(neighbours_8, [{-1, -1}, {0, -1}, {1, -1}, {-1, 0}, {1, 0}, {-1, 1}, {0, 1}, {1, 1}]).

from_list2d(List2d) ->
    {length(lists:nth(1, List2d)), length(List2d), array:from_list(lists:concat(List2d))}.

new({Width, Height}) -> {Width, Height, array:new(Width * Height, {default, 0})}.

get(X, Y, {W, _, A} = _Canvas) -> array:get(index(X, Y, W), A).

get(Positions, Canvas) -> [{X, Y, get(X, Y, Canvas)} || {X, Y} <- Positions].

set(X, Y, Value, {W, H, A} = _Canvas) ->
    {W, H, array:set(index(X, Y, W), Value, A)}.

index(X, Y, Width) -> Y * Width + X.

to_list({_, _, A} = _Canvas) -> array:to_list(A).

process(F, {W, H, _} = Canvas) ->
    [F(X, Y, get(X, Y, Canvas), Canvas) || X <- lists:seq(0, W - 1), Y <- lists:seq(0, H - 1)].

check_bounds(X, Y, {W, H, _}) when (X >= 0) and (X < W) and (Y >= 0) and (Y < H) -> {X, Y};
check_bounds(_, _, _) -> [].
check_offsets_in_bounds(X, Y, C, Offsets) ->
    lists:flatten([
        check_bounds(X + Xoff, Y + Yoff, C)
     || {Xoff, Yoff} <- Offsets
    ]).

neighbours_adjacent(X, Y, Canvas) ->
    Neighbours = check_offsets_in_bounds(X, Y, Canvas, ?neighbours_4),
    get(Neighbours, Canvas).

neighbours_adjacent_and_diagonal(X, Y, Canvas) ->
    Neighbours = check_offsets_in_bounds(X, Y, Canvas, ?neighbours_8),
    get(Neighbours, Canvas).
