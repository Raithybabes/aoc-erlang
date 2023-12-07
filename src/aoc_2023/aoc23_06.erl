-module(aoc23_06).

-export([answer/0]).

part_1() ->
    R1 = length([R || R <- [T * (35 - T) || T <- lists:seq(1, 35)], R > 213]),
    R2 = length([R || R <- [T * (69 - T) || T <- lists:seq(1, 69)], R > 1168]),
    R3 = length([R || R <- [T * (68 - T) || T <- lists:seq(1, 68)], R > 1086]),
    R4 = length([R || R <- [T * (87 - T) || T <- lists:seq(1, 87)], R > 1248]),
    Part1 = R1 * R2 * R3 * R4,
    Part1.

% This takes a while to run on Windows but just a few seconds on Mac
% It's definitely not good enough and should be optimised...
% The optimum value is in the middle.
% The other values are symmetrical to either side.
% First level optimisation would be to start in middle, and move away from center until no longer achieving the target.
% Next level would be binary chop to find the 'distance' from the center.
% Note that even start values have an odd number of results (single mid point),
% whereas odd start numbers have an equal number of results (pair of mid points)
part_2() ->
    Part2 = length([
        R
     || R <- [T * (35696887 - T) || T <- lists:seq(1, 35696887)], R > 213116810861248
    ]),
    Part2.

answer() ->
    % Dat = aoc23:data("06", fun process_dat/1),
    Part1 = part_1(),
    Part2 = part_2(),
    {Part1, Part2}.
