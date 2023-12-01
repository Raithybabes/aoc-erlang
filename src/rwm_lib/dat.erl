-module(dat).

-export([
    nop/1,
    duplicate/2,
    map_data/2,
    to_integer/1,
    to_bits/1,
    to_integer_list/1,
    to_positive_integer_list/1,
    to_digit_list/1,
    to_float_list/1,
    to_token_list/1,
    match/2,
    match_global/2,
    extract/2,
    extract_global/2
]).

nop(V) -> V.

to_integer(B) -> binary_to_integer(B).

to_bits(B) -> bits:parse(match(B, "(.+)")).

to_token_list(<<>>) ->
    [];
to_token_list(B) ->
    {match, Captures} = re:run(B, "\\w+", [global, {capture, all, list}]),
    [lists:flatten(X) || X <- Captures].

to_positive_integer_list(<<>>) ->
    [];
to_positive_integer_list(B) ->
    {match, Captures} = re:run(B, "\\d+", [global, {capture, all, list}]),
    lists:map(fun(X) -> list_to_integer(lists:flatten(X)) end, Captures).

to_integer_list(<<>>) ->
    [];
to_integer_list(B) ->
    {match, Captures} = re:run(B, "-?\\d+", [global, {capture, all, list}]),
    lists:map(fun(X) -> list_to_integer(lists:flatten(X)) end, Captures).

to_digit_list(<<>>) ->
    [];
to_digit_list(B) ->
    {match, Captures} = re:run(B, "\\d", [global, {capture, all, list}]),
    lists:map(fun(X) -> list_to_integer(lists:flatten(X)) end, Captures).

to_float_list(B) ->
    {match, Captures} = re:run(B, "-?[\\d.]+", [global, {capture, all, list}]),
    lists:map(fun(X) -> list_to_float(lists:flatten(X)) end, Captures).

duplicate(Number, Item) -> duplicate(Number, Item, []).
duplicate(0, _, L) -> L;
duplicate(N, I, L) when N > 0 -> duplicate(N - 1, I, [I | L]).

match(V, RX) ->
    {match, [Capture]} = re:run(V, RX, [{capture, first, list}]),
    Capture.

match_global(V, RX) ->
    {match, Captures} = re:run(V, RX, [global, {capture, first, list}]),
    [X || [X] <- Captures].

extract(V, RX) ->
    {match, Captures} = re:run(V, RX, [{capture, all_but_first, list}]),
    Captures.

extract_global(V, RX) ->
    {match, Captures} = re:run(V, RX, [global, {capture, all_but_first, list}]),
    Captures.

map_data(Filename, Processor) ->
    list_or_single(lists:map(Processor, dat_to_list(Filename))).

dat_to_list(Filename) ->
    {ok, Data} = file:read_file(Filename),
    string:split(Data, "\n", all).

list_or_single(List) when length(List) =:= 1 -> lists:nth(1, List);
list_or_single(List) -> List.
