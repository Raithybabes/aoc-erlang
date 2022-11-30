-module(rwm).

-export([iif/3]).
-export([string_to_atoms/1, strings_to_atoms/1]).
-export([sort_lists_by_length/1]).
-export([frequencies/1]).
-export([pipe/2]).
-export([pipeline/2]).

iif(Value, True, False) ->
    if
        Value -> True;
        not Value -> False
    end.

strings_to_atoms(Strings) -> [string_to_atoms(X) || X <- Strings].
string_to_atoms(String) -> lists:sort([list_to_atom([X]) || X <- String]).

sort_lists_by_length(Lists) -> lists:sort(fun (L1, L2) -> length(L1) =< length(L2) end, Lists).

% https://stackoverflow.com/questions/59148096/erlang-finding-the-number-of-occurrences-of-a-number-in-a-list
frequencies(List) ->
    frequencies(List, #{}).

frequencies([], Freqs) ->
    maps:to_list(Freqs);
frequencies([H|T], Freqs) ->
    Incrementer = fun(Count) -> Count+1 end,
    NewFreqs = maps:update_with(H, Incrementer, _Default=1, Freqs),
    frequencies(T, NewFreqs).

pipe(V, []) -> V;
pipe(V, [F|FF]) -> pipe(F(V), FF).

pipeline(L, []) -> L;
pipeline(L, [F|FF]) -> pipeline([F(X) || X <- L], FF).
