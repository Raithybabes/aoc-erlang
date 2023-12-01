-module(aoc22_15).

-include("../rwm_lib/macros.hrl").

-export([answer/0]).

process_dat(Line) ->
    [Sx, Sy, Bx, By] = dat:to_integer_list(Line),
    {{Sx, Sy}, {Bx, By}}.

x_range(Y, Sensors) ->
    Ranges = coverage(Y, Sensors),
    Min = lists:min([X || {X, _} <- Ranges]),
    Max = lists:max([X || {_, X} <- Ranges]),
    {Min, Max}.

coverage(Y, Sensors) ->
    Ranges = [{MinX, MaxX} || {coverage, MinX, MaxX} <- [cov_range(Y, S) || S <- Sensors]],
    lists:sort(Ranges).

cov_range(Y, Sensor) ->
    {Sx, Sy, D} = Sensor,
    Attenuation = abs(Y - Sy),
    Strength = D - Attenuation,
    {?CASE(Strength =< 0, no_coverage, coverage), Sx - Strength, Sx + Strength}.

find_gap_y(Y, MaxY, _, _, _) when Y > MaxY -> no_gap;
find_gap_y(Y, MaxY, MinX, MaxX, Sensors) ->
    Ranges = coverage(Y, Sensors),
    % io:format("~p look for gap amongst ranges ~p~n~n", [Y, Ranges]),
    GapX = find_gap_x(MinX, MaxX, Ranges),
    case GapX =:= no_gap of
        true -> find_gap_y(Y + 1, MaxY, MinX, MaxX, Sensors);
        false -> {GapX, Y}
    end.

find_gap_x(X, MaxX, _) when X > MaxX -> no_gap;
find_gap_x(_, _, []) ->
    no_gap;
find_gap_x(X, MaxX, [{Rmin, Rmax} | Rest]) ->
    case X < Rmin of
        true -> X;
        false -> find_gap_x(max(Rmax + 1, X), MaxX, Rest)
    end.

answer() ->
    Dat = aoc22:data("15", fun process_dat/1),
    Sensors = [{Sx, Sy, abs(Bx - Sx) + abs(By - Sy)} || {{Sx, Sy}, {Bx, By}} <- Dat],
    % Beacons = [B || {_, B} <- Dat],
    {Min, Max} = x_range(2000000, Sensors),
    Part1 = Max - Min,
    {GapX, Gapy} = find_gap_y(0, 4000000, 0, 4000000, Sensors),
    Part2 = GapX * 4000000 + Gapy,
    {Part1, Part2}.
