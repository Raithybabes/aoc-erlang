-module(aoc22_11).

-export([answer/0]).

-record(monkey, {items = [], fn_op, div_test, true_target, false_target, inspect_count = 0}).

% -define(magic_divisor, 96577). % 23 * 19 * 13 * 17
-define(magic_divisor, 9699690). % 2 * 3 * 5 * 7 * 11 * 13 * 17 * 19

test_monkeys() ->
    array:from_list([
        #monkey{
            items = [79, 98],
            fn_op = fun(V) -> V * 19 end,
            div_test = 23,
            true_target = 2,
            false_target = 3
        },
        #monkey{
            items = [54, 65, 75, 74],
            fn_op = fun(V) -> V + 6 end,
            div_test = 19,
            true_target = 2,
            false_target = 0
        },
        #monkey{
            items = [79, 60, 97],
            fn_op = fun(V) -> V * V end,
            div_test = 13,
            true_target = 1,
            false_target = 3
        },
        #monkey{
            items = [74],
            fn_op = fun(V) -> V + 3 end,
            div_test = 17,
            true_target = 0,
            false_target = 1
        }
    ]).

cheat_hardcoded_monkeys() ->
    array:from_list([
        #monkey{
            items = [54, 61, 97, 63, 74],
            fn_op = fun(V) -> V * 7 end,
            div_test = 17,
            true_target = 5,
            false_target = 3
        },
        #monkey{
            items = [61, 70, 97, 64, 99, 83, 52, 87],
            fn_op = fun(V) -> V + 8 end,
            div_test = 2,
            true_target = 7,
            false_target = 6
        },
        #monkey{
            items = [60, 67, 80, 65],
            fn_op = fun(V) -> V * 13 end,
            div_test = 5,
            true_target = 1,
            false_target = 6
        },
        #monkey{
            items = [61, 70, 76, 69, 82, 56],
            fn_op = fun(V) -> V + 7 end,
            div_test = 3,
            true_target = 5,
            false_target = 2
        },
        #monkey{
            items = [79, 98],
            fn_op = fun(V) -> V + 2 end,
            div_test = 7,
            true_target = 0,
            false_target = 3
        },
        #monkey{
            items = [72, 79, 55],
            fn_op = fun(V) -> V + 1 end,
            div_test = 13,
            true_target = 2,
            false_target = 1
        },
        #monkey{
            items = [63],
            fn_op = fun(V) -> V + 4 end,
            div_test = 19,
            true_target = 7,
            false_target = 4
        },
        #monkey{
            items = [72, 51, 93, 63, 80, 86, 81],
            fn_op = fun(V) -> V * V end,
            div_test = 11,
            true_target = 0,
            false_target = 4
        }
    ]).

increment_inspect_count(M, V) -> M#monkey{inspect_count = M#monkey.inspect_count + V}.
add_item(M, Item) -> M#monkey{items = M#monkey.items ++ [Item]}.
remove_item(M, Item) -> M#monkey{items = M#monkey.items -- [Item]}.

update_monkey(Monkeys, Idx, New) -> array:set(Idx, New, Monkeys).

% shrink_it(X) when X > ?magic_divisor ->
%     shrink_it(X - ?magic_divisor);
% shrink_it(X) -> X.

throw(Monkeys, From, To, OldItem, NewItem) ->
    NewFrom = remove_item(array:get(From, Monkeys), OldItem),
    NewTo = add_item(array:get(To, Monkeys), NewItem),
    Monkeys1 = update_monkey(Monkeys, From, NewFrom),
    Monkeys2 = update_monkey(Monkeys1, To, NewTo),
    Monkeys2.

throw_all_the_things({Monkeys, Idx}, WorryDivisor) ->
    Monkey = array:get(Idx, Monkeys),
    Items = Monkey#monkey.items,
    Monkey1 = increment_inspect_count(Monkey, length(Items)),
    Monkeys1 = update_monkey(Monkeys, Idx, Monkey1),
    throw_all_the_things(Items, {Monkeys1, Idx}, WorryDivisor).
throw_all_the_things([], {Monkeys, _Idx}, _WorryDivisor) ->
    Monkeys;
throw_all_the_things([Item | Rest], {Monkeys, Idx}, WorryDivisor) ->
    Monkey = array:get(Idx, Monkeys),
    FnOp = Monkey#monkey.fn_op,
    OpItem = floor(FnOp(Item) / WorryDivisor),
    NewItemValue = OpItem rem ?magic_divisor,
    IsDivisble = is_divisble(NewItemValue, Monkey#monkey.div_test),
    Recipient =
        if
            IsDivisble -> Monkey#monkey.true_target;
            true -> Monkey#monkey.false_target
        end,
    Monkeys1 = throw(Monkeys, Idx, Recipient, Item, NewItemValue),
    throw_all_the_things(Rest, {Monkeys1, Idx}, WorryDivisor).

is_divisble(V, Div) -> V rem Div =:= 0.

step(Monkeys, Num, WorryDivisor) -> step(Monkeys, 0, Num, WorryDivisor).
step(Monkeys, _Idx, 0, _WorryDivisor) ->
    Monkeys;
step(Monkeys, Idx, Num, WorryDivisor) ->
    DoneAllMonkeys = Idx >= array:size(Monkeys),
    if
        DoneAllMonkeys ->
            step(Monkeys, 0, Num - 1, WorryDivisor);
        true ->
            Monkeys1 = throw_all_the_things({Monkeys, Idx}, WorryDivisor),
            step(Monkeys1, Idx + 1, Num, WorryDivisor)
    end.

answer() ->
    % Dat = aoc22:data("11", fun process_dat/1),
    Dat = cheat_hardcoded_monkeys(),
    Result1 = array:to_list(step(Dat, 20, 3)),
    [Top1A, Top1B | _] = lists:reverse(lists:sort([Monkey#monkey.inspect_count || Monkey <- Result1])),
    Part1 = Top1A * Top1B,
    Result2= array:to_list(step(Dat, 10000, 1)),
    [Top2A, Top2B | _] = lists:reverse(lists:sort([Monkey#monkey.inspect_count || Monkey <- Result2])),
    Part2 = Top2A * Top2B,
    {Part1, Part2}
    .
