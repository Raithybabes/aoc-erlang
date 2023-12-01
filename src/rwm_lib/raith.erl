-module(raith).

% Note: do not use these funs! These are experiments and a reminder of how to do things.

-export([
    version/0,
    list_intersect/2,
    factorial/1,
    list_reverse/1,
    sort/1,
    partition/2,
    psort/1
]).

-define(version, [{major, 0}, {minor, 1}, {patch, 1}]).

-define(v(Part), integer_to_list(proplists:get_value(Part, ?version))).

version() -> ?v(major) ++ "." ++ ?v(minor) ++ "." ++ ?v(patch).

list_intersect(List1, List2) -> lists:uniq([V1 || V1 <- List1, V2 <- List2, V1 =:= V2]).

factorial(0) -> 1;
factorial(N) when N > 0 -> N * factorial(N - 1).

list_reverse([], Rev) -> Rev;
list_reverse([H | T], Rev) -> list_reverse(T, [H | Rev]).

list_reverse(L) -> list_reverse(L, []).

% Very Naive quicksort - doesn't determine a suitable pivot value, and traverses the sort items twice
sort([]) -> [];
sort([H | T]) -> sort([X || X <- T, X =< H]) ++ [H] ++ sort([X || X <- T, X > H]).

% A partitioner using tail recursion, ultimately delivers the lists of Lesser and Greater items
partition(_, [], Lesser, Greater) ->
    {Lesser, Greater};
partition(SplitValue, [H | T], Lesser, Greater) when H =< SplitValue ->
    partition(SplitValue, T, [H | Lesser], Greater);
partition(SplitValue, [H | T], Lesser, Greater) when H > SplitValue ->
    partition(SplitValue, T, Lesser, [H | Greater]).
partition(SplitValue, List) -> partition(SplitValue, List, [], []).

% Slightly less naive quicksort - uses partitioner to split the list in a single pass of tail-recursion
psort([]) ->
    [];
psort([H | T]) ->
    {Lesser, Greater} = partition(H, T),
    psort(Lesser) ++ [H] ++ psort(Greater).
