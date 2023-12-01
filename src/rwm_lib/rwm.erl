-module(rwm).

-export([string_to_atoms/1, strings_to_atoms/1]).
-export([sort_lists_by_length/1, first/1, first/2, last/1, last/2, intersect/1, intersect/2]).
-export([frequencies/1]).
-export([pipe/2]).
-export([pipeline/2]).

strings_to_atoms(Strings) -> [string_to_atoms(X) || X <- Strings].
string_to_atoms(String) -> lists:sort([list_to_atom([X]) || X <- String]).

sort_lists_by_length(Lists) -> lists:sort(fun(L1, L2) -> length(L1) =< length(L2) end, Lists).

% https://stackoverflow.com/questions/59148096/erlang-finding-the-number-of-occurrences-of-a-number-in-a-list
frequencies(List) ->
    frequencies(List, #{}).

frequencies([], Freqs) ->
    maps:to_list(Freqs);
frequencies([H | T], Freqs) ->
    Incrementer = fun(Count) -> Count + 1 end,
    NewFreqs = maps:update_with(H, Incrementer, _Default = 1, Freqs),
    frequencies(T, NewFreqs).

pipe(V, []) -> V;
pipe(V, [F | FF]) -> pipe(F(V), FF).

pipeline(L, []) -> L;
pipeline(L, [F | FF]) -> pipeline([F(X) || X <- L], FF).

first(List) ->
    [Keep] = first(1, List),
    Keep.
first(N, L) ->
    {Keep, _} = lists:split(N, L),
    Keep.

last(List) ->
    [Keep] = last(1, List),
    Keep.
last(N, L) ->
    {_, Keep} = lists:split(length(L) - N, L),
    Keep.

intersect(ListA, ListB) -> lists:uniq([A || A <- ListA, B <- ListB, A =:= B]).

intersect([List1 | T]) -> lists:foldl(fun(L, A) -> intersect(L, A) end, List1, T).
