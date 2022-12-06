-module(aoc22_06).

-export([answer/0]).

has_no_dupes(L) -> length(lists:uniq(L)) =:= length(L).

% find_start_of_packet(L) -> find_start_of_packet(L, 0).
% find_start_of_packet([H1, H2, H3, H4 | T], Count) ->
%     MarkerFound = has_no_dupes([H1, H2, H3, H4]),
%     if
%         MarkerFound -> Count + 4;
%         true -> find_start_of_packet([H2, H3, H4 | T], Count + 1)
%     end.

find_uniq_seq(List, Length) -> find_uniq_seq(List, Length, 0).
find_uniq_seq([_H | T] = List, Length, Pos) ->
    Found = has_no_dupes(rwm:first(Length, List)),
    if
        Found -> Pos + Length;
        true -> find_uniq_seq(T, Length, Pos + 1)
    end.

process_dat(Line) -> binary_to_list(Line).

answer() ->
    Dat = aoc22:data("06", fun process_dat/1),
    {find_uniq_seq(Dat, 4), find_uniq_seq(Dat, 14)}.
