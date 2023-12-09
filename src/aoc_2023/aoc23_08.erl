-module(aoc23_08).

-export([answer/0]).

part_1({Dirs, Nodes}) ->
    Part1 = nav(Nodes, "AAA", Dirs),
    Part1.

part_2({Dirs, Nodes}) ->
    Starts = [N || {N = [_, _, $A], _, _} <- Nodes],
    LoopPoints = [find_loop_point(Nodes, Start, Dirs) || Start <- Starts],
    [Start | Rest] = LoopPoints,
    Part2 = lists:foldl(fun(X, A) -> least_common_multiple(A, X) end, Start, Rest),
    Part2.

least_common_multiple(A, B) -> least_common_multiple(A, B, A).
least_common_multiple(_, B, Acc) when Acc rem B == 0 -> Acc;
least_common_multiple(A, B, Acc) -> least_common_multiple(A, B, Acc + A).

process_dat([Dirs, _ | T]) ->
    {Dirs, [process_dat2(L) || L <- T]}.
process_dat2(X) ->
    [Node, L, R] = dat:extract(X, "(\\w{3}).+(\\w{3}).+(\\w{3})"),
    {Node, L, R}.

nav(Nodes, Start, Dirs) -> nav(Nodes, Start, Dirs, Dirs, 0).
%
nav(_, "ZZZ", _DirsOrig, [], Count) ->
    Count;
nav(Nodes, Current, DirsOrig, [], Count) ->
    nav(Nodes, Current, DirsOrig, DirsOrig, Count);
nav(Nodes, Current, DirsOrig, [Dir | T], Count) ->
    {_, L, R} = proplists:lookup(Current, Nodes),
    Nav = #{$L => L, $R => R},
    nav(Nodes, maps:get(Dir, Nav), DirsOrig, T, Count + 1).

% There are 5 **A start nodes. Find how many steps each takes before a fresh iteration of the directions starts on a **Z node.
% Then the Least Common Multiple of those counts is the point where the patterns intersect.

find_loop_point(Nodes, Start, Dirs) ->
    find_loop_point(Nodes, Start, Start, Dirs, Dirs, 0).
%
find_loop_point(_, [_, _, $Z], _, DirsOrig, DirsOrig, Count) when Count > 0 ->
    Count;
find_loop_point(Nodes, Current, Start, DirsOrig, [], Count) ->
    find_loop_point(Nodes, Current, Start, DirsOrig, DirsOrig, Count);
find_loop_point(Nodes, Current, Start, DirsOrig, [Dir | T], Count) ->
    {_, L, R} = proplists:lookup(Current, Nodes),
    Nav = #{$L => L, $R => R},
    find_loop_point(Nodes, maps:get(Dir, Nav), Start, DirsOrig, T, Count + 1).

answer() ->
    Raw = aoc23:data("08", fun binary_to_list/1),
    Dat = process_dat(Raw),
    Part1 = part_1(Dat),
    Part2 = part_2(Dat),
    {Part1, Part2}.
