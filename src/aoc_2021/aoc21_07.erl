-module(aoc21_07).

-export([answer/0]).

% keep_cheapest([C | T]) -> keep_cheapest(C, T).
% keep_cheapest(Cheapest, []) ->
%     Cheapest;
% keep_cheapest({_, CheapestCost} = Cheapest, [{_, CheckCost} = Check | T]) ->
%     keep_cheapest(rwm:iif(CheapestCost =< CheckCost, Cheapest, Check), T).

% fac(Num) -> fac(Num, 0).
% fac(0, Total) -> Total;
% fac(N, Total) -> fac(N - 1, Total + N).

fast_fac(Num) -> floor((Num + 1) / 2 * Num).

answer() ->
    Dat = aoc21:data("07", fun dat:to_integer_list/1),
    Range = lists:seq(lists:min(Dat), lists:max(Dat)),
    % SimpleCosts = lists:map(fun(Pos) -> {Pos, lists:sum([abs(X - Pos) || X <- Dat])} end, Range),
    % {_Pos, Cost} = keep_cheapest(SimpleCosts),
    SimpleCosts = lists:map(fun(Pos) -> lists:sum([abs(X - Pos) || X <- Dat]) end, Range),
    FactorialCosts = lists:map(fun(Pos) -> lists:sum([fast_fac(abs(X - Pos)) || X <- Dat]) end, Range),
    {
        lists:min(SimpleCosts),
        lists:min(FactorialCosts)
    }.
