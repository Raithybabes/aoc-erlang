-module(aoc21_04).

-export([answer/0]).

extract_data([Calls | Rest]) ->
    extract_data(Rest, Calls, []).
extract_data([], Calls, Boards) ->
    {Calls, Boards};
extract_data([_Blank, Line1, Line2, Line3, Line4, Line5 | Rest], Calls, Boards) ->
    extract_data(
        Rest,
        Calls,
        Boards ++ [Line1 ++ Line2 ++ Line3 ++ Line4 ++ Line5]
    ).

play_all_boards(Boards, Calls) ->
    lists:sort(
        fun boards_order_by_win/2,
        lists:map(fun(Board) -> play_board(Board, Calls) end, Boards)
    ).

play_board(Board, [FirstCall | RemainingCalls]) -> play_board(Board, [FirstCall], RemainingCalls).
play_board(Board, CallsMade, [NextCall | Rest]) ->
    case board_has_won(Board, CallsMade) of
        true -> {Board, CallsMade, length(CallsMade)};
        false -> play_board(Board, CallsMade ++ [NextCall], Rest)
    end.

board_has_won(Board, Calls) ->
    Rows = board_win_lines(Board),
    % if any win line has all of its numbers in the list of Calls
    lists:any(fun(Row) -> lists:all(fun(V) -> lists:member(V, Calls) end, Row) end, Rows).

board_win_lines(Board) ->
    % these boards have 10 win lines, 5 rows and 5 columns, no diagonals
    [
        extract_win_line(Board, 1, 1),
        extract_win_line(Board, 6, 1),
        extract_win_line(Board, 11, 1),
        extract_win_line(Board, 16, 1),
        extract_win_line(Board, 21, 1),
        extract_win_line(Board, 1, 5),
        extract_win_line(Board, 2, 5),
        extract_win_line(Board, 3, 5),
        extract_win_line(Board, 4, 5),
        extract_win_line(Board, 5, 5)
    ].

extract_win_line(Board, Offset, Inc) ->
    lists:map(
        fun(Iter) -> lists:nth(Offset + Iter * Inc, Board) end,
        lists:seq(0, 4)
    ).

boards_order_by_win({_, _, TurnA}, {_, _, TurnB}) -> TurnA =< TurnB.

board_value(Board) ->
    {Nums, Called, _CallsTaken} = Board,
    Unmarked = lists:sum(lists:filter(fun(Num) -> not lists:member(Num, Called) end, Nums)),
    LastCalled = lists:last(Called),
    Unmarked * LastCalled.

answer() ->
    Dat = aoc21:data("04", fun dat:to_integer_list/1),
    {Calls, Boards} = extract_data(Dat),
    Completed = play_all_boards(Boards, Calls),
    WinningBoard = lists:nth(1, Completed),
    LosingBoard = lists:last(Completed),
    {board_value(WinningBoard), board_value(LosingBoard)}.
