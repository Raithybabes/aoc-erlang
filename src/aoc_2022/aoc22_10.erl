-module(aoc22_10).

-export([answer/0]).

run_op({noop}, X) -> [X];
run_op({addx, V}, X) -> [X, X + V].

run(Ops) -> run(Ops, 1, [1]).
run([], _, Cycles) ->
    Cycles;
run([Op | T], X, Cycles) ->
    AppendCycles = run_op(Op, X),
    run(T, rwm:last(AppendCycles), Cycles ++ AppendCycles).

count_em(Cycs, Checks) -> count_em(Cycs, Checks, 0).
count_em(_, [], Total) ->
    Total;
count_em(Cycs, [Check | T], Total) ->
    V = lists:nth(Check, Cycs),
    % io:format("Taking during cycle ~p, ~p = ~p~n", [Check, V, Total + (V * Check)]),
    count_em(Cycs, T, Total + (V * Check)).

render(Cycles) -> render(Cycles, 1, []).
render([], _, Rendered) ->
    Rendered;
render([Pixel | Rest], Count, Rendered) ->
    ScanPos = (Count - 1) rem 40,
    SetPixel =
        if
            abs(ScanPos - Pixel) =< 1 -> 1;
            true -> 0
        end,
    render(Rest, Count + 1, Rendered ++ [SetPixel]).

chunks(Inp, ChunkSize) -> chunks(Inp, ChunkSize, [], []).
chunks(Inp, ChunkSize, Buffer, Out) when length(Buffer) =:= 40 ->
    chunks(Inp, ChunkSize, [], Out ++ [Buffer]);
chunks([], _, _, Out) ->
    Out;
chunks([V | T], ChunkSize, Buffer, Out) ->
    chunks(T, ChunkSize, Buffer ++ [V], Out).

plot([]) ->
    {ok};
plot([Line | T]) ->
    plot_line(Line),
    plot(T).

plot_line(Line) -> plot_line(Line, "").
plot_line([], Output) -> io:format("~p~n", [Output]);
plot_line([0 | T], Output) -> plot_line(T, Output ++ "  ");
plot_line([1 | T], Output) -> plot_line(T, Output ++ "[]").

process_dat_instruction(["noop"]) -> {noop};
process_dat_instruction(["addx", V]) -> {addx, list_to_integer(V)}.

process_dat(Line) -> process_dat_instruction(dat:extract(Line, "(\\w+) ?([\\d-]+)?")).

answer() ->
    Dat = aoc22:data("10", fun process_dat/1),
    Cycles = run(Dat),
    Part1 = count_em(Cycles, [20, 60, 100, 140, 180, 220]),
    Render = render(Cycles),
    Chunks = chunks(Render, 40),
    plot(Chunks),
    {Part1, "EZFCHJAB"}.
