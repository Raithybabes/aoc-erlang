-module(aoc21_02).

-export([answer/0]).

move_sub_simple(Commands) ->
    Position = 0,
    Depth = 0,
    move_sub_simple(Commands, {Position, Depth}).

move_sub_simple([], Loc) -> Loc;
move_sub_simple([Command | Rest], Loc) -> move_sub_simple(Rest, apply_command_simple(Command, Loc)).

apply_command_simple({forward, Amt}, {Position, Depth}) -> {Position + Amt, Depth};
apply_command_simple({up, Amt}, {Position, Depth}) -> {Position, Depth - Amt};
apply_command_simple({down, Amt}, {Position, Depth}) -> {Position, Depth + Amt}.

move_sub_aim(Commands) ->
    Position = 0,
    Depth = 0,
    Aim = 0,
    move_sub_aim(Commands, {Position, Depth, Aim}).

move_sub_aim([], Loc) -> Loc;
move_sub_aim([Command | Rest], Loc) -> move_sub_aim(Rest, apply_command_aim(Command, Loc)).

apply_command_aim({forward, Amt}, {Position, Depth, Aim}) ->
    {Position + Amt, Depth + Aim * Amt, Aim};
apply_command_aim({up, Amt}, {Position, Depth, Aim}) ->
    {Position, Depth, Aim - Amt};
apply_command_aim({down, Amt}, {Position, Depth, Aim}) ->
    {Position, Depth, Aim + Amt}.

process_dat(V) ->
    [Command, Amount] = dat:extract(V, "(\\w+) (\\d+)"),
    {list_to_atom(Command), list_to_integer(Amount)}.

answer() ->
    Dat = aoc21:data("02", fun process_dat/1),
    {Position1, Depth1} = move_sub_simple(Dat),
    {Position2, Depth2, _Aim} = move_sub_aim(Dat),
    {Position1 * Depth1, Position2 * Depth2}.
