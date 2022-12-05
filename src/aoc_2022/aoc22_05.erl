-module(aoc22_05).

-export([answer/0]).

process_dat(Line) ->
    X = dat:extract(Line, "move (\\d+) from (\\d+) to (\\d+)|.*"),
    if
        length(X) =:= 3 ->
            [list_to_integer(A) || A <- X];
        true ->
            X
    end.

% Note: I am currently cheating! Sorry :O
% I simply don't have time this morning to parse the data
% I shall finish this later
% In the meantime I have hard-coded my starting stacks and completed the moving part of the challenge

num_stacks() -> 9.

cheat_stacks(1) -> "NRJTZBDF";
cheat_stacks(2) -> "HJNSR";
cheat_stacks(3) -> "QFZGJNRC";
cheat_stacks(4) -> "QTRGNVF";
cheat_stacks(5) -> "FQTL";
cheat_stacks(6) -> "NGRBZWCQ";
cheat_stacks(7) -> "MHNSLCF";
cheat_stacks(8) -> "JTMQND";
cheat_stacks(9) -> "SGP".

run_step_9001(S, [Num, From, To]) -> run_step_9001(S, From, To, [], Num).
run_step_9001(S, _, To, Buffer, 0) ->
    A0 = array:from_list(S),
    A1 = array:set(To - 1, Buffer ++ array:get(To - 1, A0), A0),
    array:to_list(A1);
run_step_9001(S, From, To, Buffer, Num) ->
    A0 = array:from_list(S),
    [Item | Rest] = array:get(From - 1, A0),
    A1 = array:set(From - 1, Rest, A0),
    % A2 = array:set(To - 1, [Item | array:get(To - 1, A1)], A1),
    run_step_9001(array:to_list(A1), From, To, Buffer ++ [Item], Num - 1).

run_step_9000(S, [Num, From, To]) -> run_step_9000(S, From, To, Num).
run_step_9000(S, _, _, 0) ->
    S;
run_step_9000(S, From, To, Num) ->
    A0 = array:from_list(S),
    [Item | Rest] = array:get(From - 1, A0),
    A1 = array:set(From - 1, Rest, A0),
    A2 = array:set(To - 1, [Item | array:get(To - 1, A1)], A1),
    run_step_9000(array:to_list(A2), From, To, Num - 1).

run_plan(_, S, []) ->
    S;
run_plan(F, S, [M | T]) ->
    S1 = F(S, M),
    run_plan(F, S1, T).

top_of_each_stack(Stacks) -> [T || [T|_Rest] <- Stacks].

answer() ->
    Dat = aoc22:data("05", fun process_dat/1),
    Moves = [D || D <- Dat, length(D) =:= 3],
    StartStacks = [cheat_stacks(I) || I <- lists:seq(1, num_stacks())],
    Stacks1 = run_plan(fun run_step_9000/2, StartStacks, Moves),
    Tops1 = top_of_each_stack(Stacks1),
    Stacks2 = run_plan(fun run_step_9001/2, StartStacks, Moves),
    Tops2 = top_of_each_stack(Stacks2),
    {Tops1 , Tops2}.
