-module(aoc_runner).

-include("macros.hrl").

-export([expect/1]).

assert_answer({Mod, Expected}) ->
    Actual = Mod:answer(),
    ?CASE(Actual =:= Expected, {pass, Mod}, {fail, Mod, {expected, Expected}, {actual, Actual}}).

expect(Expected) ->
    Results = lists:map(fun assert_answer/1, Expected),
    Passes = [X || {pass, _} = X <- Results],
    io:format("Results:~n~p~n~n", [Results]),
    ?CASE(Results =:= Passes, {ok, "all tests passed"}, {fail, "there were failures"}).
