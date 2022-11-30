-module(aoc21_08).

-export([answer/0]).

%    aaaa     ....     aaaa     aaaa     ....     aaaa     aaaa     aaaa     aaaa     aaaa
%   b    c   .    c   .    c   .    c   b    c   b    .   b    .   .    c   b    c   b    c
%   b    c   .    c   .    c   .    c   b    c   b    .   b    .   .    c   b    c   b    c
%    ....     ....     dddd     dddd     dddd     dddd     dddd     ....     dddd     dddd
%   e    f   .    f   e    .   .    f   .    f   .    f   e    f   .    f   e    f   .    f
%   e    f   .    f   e    .   .    f   .    f   .    f   e    f   .    f   e    f   .    f
%    gggg     ....     gggg     gggg     ....     gggg     gggg     ....     gggg     gggg

digit_set() ->
    [
        {0, [a, b, c, e, f, g]},
        {1, [c, f]},
        {2, [a, c, d, e, g]},
        {3, [a, c, d, f, g]},
        {4, [b, c, d, f]},
        {5, [a, b, d, f, g]},
        {6, [a, b, d, e, f, g]},
        {7, [a, c, f]},
        {8, [a, b, c, d, e, f, g]},
        {9, [a, b, c, d, f, g]}
    ].

determine_segment_mapping(Digits) ->
    % digit 1 = uniquely has 2 segs
    [One] = [X || X <- Digits, length(X) =:= 2],
    % digit 7 = uniquely has 3 segs
    [Seven] = [X || X <- Digits, length(X) =:= 3],
    % digit 4 = uniquely has 4 segs
    [Four] = [X || X <- Digits, length(X) =:= 4],
    % seg a = appears in 7 but not in 1
    [Xa] = Seven -- One,
    SegmentFrequenciesWithoutA = rwm:frequencies([X || X <- lists:flatten(Digits), X =/= Xa]),
    % seg b = unique freq 6
    [Xb] = [X || {X, 6} <- SegmentFrequenciesWithoutA],
    % seg c = unique freq 8 (after removing seg a occurences)
    [Xc] = [X || {X, 8} <- SegmentFrequenciesWithoutA],
    % seg e = unique freq 4
    [Xe] = [X || {X, 4} <- SegmentFrequenciesWithoutA],
    % seg f = unique freq 9
    [Xf] = [X || {X, 9} <- SegmentFrequenciesWithoutA],
    SegFreq7 = [X || {X, 7} <- SegmentFrequenciesWithoutA],
    % seg d = freq 7 and is missing from digit 4
    [Xd] = lists:uniq((Four ++ SegFreq7) -- lists:uniq(Four ++ SegFreq7)),
    % seg g = freq 7 and isn't seg d
    [Xg] = SegFreq7 -- [Xd],
    Mapping = [{Xa, a}, {Xb, b}, {Xc, c}, {Xd, d}, {Xe, e}, {Xf, f}, {Xg, g}],
    Mapping.

map_digit_to_value(Digit, Mapping) ->
    MappedSegments = lists:sort([MO || I <- Digit, {MI, MO} <- Mapping, MI =:= I]),
    [Value] = [V || {V, Segs} <- digit_set(), Segs =:= MappedSegments],
    Value.

map_digits_to_value(Digits, Mapping) ->
    Value = list_to_integer(
        lists:concat([integer_to_list(map_digit_to_value(D, Mapping)) || D <- Digits])
    ),
    Value.

process_dat(V) ->
    [L, R] = dat:extract(V, "^(.*) \\| (.*)$"),
    {rwm:strings_to_atoms(dat:to_token_list(L)), rwm:strings_to_atoms(dat:to_token_list(R))}.

answer() ->
    _Test_Dat = [process_dat(X) || X <- test_data_total_61229()],
    Dat = aoc21:data("08", fun process_dat/1),
    OneFourSevenEightAppearances = lists:sum([
        1
     || {_, R} <- Dat, X1478 <- R, (length(X1478) < 5) or (length(X1478) > 6)
    ]),
    Total = lists:sum([map_digits_to_value(R, determine_segment_mapping(L)) || {L, R} <- Dat]),
    {OneFourSevenEightAppearances, Total}.

test_data_total_61229() ->
    [
        "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe",
        "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc",
        "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg",
        "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb",
        "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea",
        "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb",
        "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe",
        "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef",
        "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb",
        "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"
    ].
