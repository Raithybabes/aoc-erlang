-module(aoc22_05).

-export([answer/0]).

split(List) ->
    split(List, [], []).
split([], Buffer, Sections) ->
    Sections ++ [Buffer];
split([H | T], Buffer, Sections) when H =:= <<>> ->
    split(T, [], Sections ++ [Buffer]);
split([H | T], Buffer, Sections) ->
    split(T, Buffer ++ [H], Sections).

extract_data(Dat) ->
    [StackData, MoveData] = split(Dat),
    {process_stack_data(StackData), process_move_data(MoveData)}.

build_stacks([], Stacks) -> Stacks;
build_stacks([Row | T], Stacks) -> build_stacks(T, add_row_of_crates(Row, Stacks)).

add_row_of_crates(Row, Stacks) -> add_row_of_crates(Row, Stacks, []).
add_row_of_crates([], [], Added) ->
    Added;
add_row_of_crates([Crate | TCrate], [], Added) ->
    % add empty stacks as necessary
    add_row_of_crates([Crate | TCrate], [[]], Added);
add_row_of_crates([Crate | TCrate], [Stack | TStack], Added) ->
    add_row_of_crates(TCrate, TStack, Added ++ [add_crate(Crate, Stack)]).

add_crate(Crate, Stack) when [Crate] =:= " " -> Stack;
add_crate(Crate, Stack) -> Stack ++ [Crate].

process_stack_data(Dat) ->
    % Get the Row data (extract the Crates)
    RowData = lists:sublist(Dat, length(Dat) - 1),
    ExtractCrates = fun(X) -> lists:flatten(dat:extract_global(X, ".(.). ?")) end,
    Rows = [ExtractCrates(X) || X <- RowData],
    % Build the stacks
    build_stacks(Rows, []).

process_move_data(Dat) ->
    [dat:to_integer_list(X) || X <- Dat].

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

top_of_each_stack(Stacks) -> [T || [T | _Rest] <- Stacks].

answer() ->
    Dat = aoc22:data("05", fun dat:nop/1),
    {Stacks, Moves} = extract_data(Dat),
    Stacks1 = run_plan(fun run_step_9000/2, Stacks, Moves),
    Tops1 = top_of_each_stack(Stacks1),
    Stacks2 = run_plan(fun run_step_9001/2, Stacks, Moves),
    Tops2 = top_of_each_stack(Stacks2),
    {Tops1, Tops2}.
