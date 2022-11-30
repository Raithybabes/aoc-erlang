-module(aoc_runner).

-export([expect/1]).

assert_answer({Mod, Expected}) ->
    Actual = Mod:answer(),
    rwm:iif(Actual =:= Expected, {pass, Mod}, {fail, Mod, {expected, Expected}, {actual, Actual}}).

expect(Expected) ->
    Results = lists:map(fun assert_answer/1, Expected),
    Passes = [X || {pass, _} = X <- Results],
    io:format("Results:~n~p~n~n", [Results]),
    rwm:iif(
        Results =:= Passes,
        {ok, "all tests passed"},
        {fail, "there were failures"}
    ).
