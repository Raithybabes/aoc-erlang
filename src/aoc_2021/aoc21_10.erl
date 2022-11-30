-module(aoc21_10).

-export([answer/0]).

process_dat(V) -> [X || X <- dat:match_global(V, ".")].

check_nav(N) -> check_nav(N, []).
check_nav([], []) ->
    {ok};
check_nav([], Unmatched) ->
    {incomplete, unclosed, Unmatched};
check_nav([H | T], Stack) when (H =:= "<") or (H =:= "(") or (H =:= "[") or (H =:= "{") ->
    check_nav(T, [H | Stack]);
check_nav([H | T], [Match | Stack]) when (H =:= ">") and (Match =:= "<") ->
    check_nav(T, Stack);
check_nav([H | T], [Match | Stack]) when (H =:= ")") and (Match =:= "(") ->
    check_nav(T, Stack);
check_nav([H | T], [Match | Stack]) when (H =:= "]") and (Match =:= "[") ->
    check_nav(T, Stack);
check_nav([H | T], [Match | Stack]) when (H =:= "}") and (Match =:= "{") ->
    check_nav(T, Stack);
check_nav([H | _], [Expected | _]) when
    (H =:= ">") or (H =:= ")") or (H =:= "]") or (H =:= "}")
->
    {corrupt, found, H, expected_to_close, Expected}.

corrupt_score() ->
    #{
        ")" => 3,
        "]" => 57,
        "}" => 1197,
        ">" => 25137
    }.

autocomplete_score(R) -> autocomplete_score(R, 0).
autocomplete_score([], Score) ->
    Score;
autocomplete_score([H | T], Score) when H =:= "(" ->
    autocomplete_score(T, Score * 5 + 1);
autocomplete_score([H | T], Score) when H =:= "[" ->
    autocomplete_score(T, Score * 5 + 2);
autocomplete_score([H | T], Score) when H =:= "{" ->
    autocomplete_score(T, Score * 5 + 3);
autocomplete_score([H | T], Score) when H =:= "<" ->
    autocomplete_score(T, Score * 5 + 4).

answer() ->
    Dat = aoc21:data("10", fun process_dat/1),
    Results = [check_nav(V) || V <- Dat],
    CorruptScore = lists:sum([
        maps:get(Found, corrupt_score())
     || {corrupt, found, Found, _, _} <- Results
    ]),
    AutocompleteScores = lists:sort([
        autocomplete_score(Unmatched)
     || {incomplete, unclosed, Unmatched} <- Results
    ]),
    AutocompleteScore = lists:nth(ceil(length(AutocompleteScores) / 2), AutocompleteScores),
    {CorruptScore, AutocompleteScore}.
