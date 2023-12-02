-module(aoc23_02).

-export([answer/0]).

extract_game_id(Game) ->
    list_to_integer(dat:extract1(Game, "Game (\\d+):")).

extract_max_of_colour(Game, Colour) ->
    lists:max([list_to_integer(X) || X <- dat:extract1_global(Game, "(\\d+) " ++ atom_to_list(Colour))]).

process_game_input(List) -> process_game_input(List, []).
%
process_game_input([], Out) ->
    Out;
process_game_input([Game | T], Out) ->
    Id = extract_game_id(Game),
    R = extract_max_of_colour(Game, red),
    G = extract_max_of_colour(Game, green),
    B = extract_max_of_colour(Game, blue),
    process_game_input(T, [{Id, R, G, B} | Out]).

process_dat(X) -> binary_to_list(X).

answer() ->
    Dat = aoc23:data("02", fun process_dat/1),
    Games = process_game_input(Dat),
    Part1 = lists:sum([Id || {Id, R, G, B} <- Games, R =< 12, G =< 13, B =< 14]),
    Part2 = lists:sum([R * G * B || {_, R, G, B} <- Games]),
    {Part1, Part2}.
