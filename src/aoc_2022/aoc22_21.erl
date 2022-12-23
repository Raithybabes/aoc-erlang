-module(aoc22_21).

-export([answer/0]).

build_tree(Nodes, {exp, Op, Dep1, Dep2}) ->
    {Op, build_tree(Nodes, maps:get(Dep1, Nodes)), build_tree(Nodes, maps:get(Dep2, Nodes))};
build_tree(_Nodes, {val, Val}) ->
    Val.

eval_tree({"+", Branch1, Branch2}) ->
    eval_tree(Branch1) + eval_tree(Branch2);
eval_tree({"-", Branch1, Branch2}) ->
    eval_tree(Branch1) - eval_tree(Branch2);
eval_tree({"*", Branch1, Branch2}) ->
    eval_tree(Branch1) * eval_tree(Branch2);
eval_tree({"/", Branch1, Branch2}) ->
    eval_tree(Branch1) / eval_tree(Branch2);
eval_tree(V) ->
    V.

parse_expr([[], [], [], Num]) ->
    {val, list_to_integer(Num)};
parse_expr([Src1, Op, Src2]) ->
    {exp, Op, Src1, Src2};
parse_expr(Expr) ->
    parse_expr(dat:extract(Expr, "^(\\w{4}) ([+-/*]) (\\w{4})$|^(\\d+)$")).

process_dat(Line) ->
    [Monkey, Expr] = dat:extract(Line, "(\\w{4}): (.+)$"),
    {Monkey, parse_expr(Expr)}.

answer() ->
    Dat = aoc22:data("21", fun process_dat/1),
    Nodes = maps:from_list(Dat),
    Root = maps:get("root", Nodes),
    Humn = maps:get("humn", Nodes),

    io:format("root = ~p~n", [Root]),
    io:format("humn = ~p~n", [Humn]),

    Tree1 = build_tree(Nodes, maps:get("root", Nodes)),
    % io:format("Tree1 = ~p~n", [Tree1]),
    Part1 = floor(eval_tree(Tree1)), % 353837700405464
    Part1 = 353837700405464,
    io:format("Part 1 = ~p~n", [Part1]).
