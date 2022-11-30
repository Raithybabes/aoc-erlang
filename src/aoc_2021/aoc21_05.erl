-module(aoc21_05).

-export([answer/0]).

dir(From, To) ->
    if
        To > From -> 1;
        From > To -> -1;
        From == To -> 0
    end.

orthogonal_lines(Dat) ->
    lists:filter(fun([X1, Y1, X2, Y2]) -> (X1 == X2) or (Y1 == Y2) end, Dat).

plot_all(Lines) ->
    lists:flatten(lists:map(fun(Line) -> plot_orthogonal_or_diagonal_line(Line) end, Lines)).

plot_orthogonal_or_diagonal_line(Line) ->
    plot_orthogonal_or_diagonal_line(Line, line_steps(Line), []).
plot_orthogonal_or_diagonal_line(_, -1, Plotted) ->
    Plotted;
plot_orthogonal_or_diagonal_line(Line, Step, Plotted) ->
    [X1, Y1, X2, Y2] = Line,
    PlotX = X1 + dir(X1, X2) * Step,
    PlotY = Y1 + dir(Y1, Y2) * Step,
    plot_orthogonal_or_diagonal_line(Line, Step - 1, [{PlotX, PlotY} | Plotted]).

line_steps([X1, Y1, X2, Y2]) ->
    max(abs(X1 - X2), abs(Y1 - Y2)).

duplicated_items(List) -> duplicated_items(lists:sort(List), '', 0, []).
duplicated_items([], _, _, Dups) ->
    Dups;
duplicated_items([H | T], Prev, CountPrev, Dups) ->
    NewCount =
        case H == Prev of
            true -> CountPrev + 1;
            false -> 0
        end,
    NewDups =
        case NewCount == 1 of
            true -> [H];
            false -> []
        end,
    duplicated_items(T, H, NewCount, Dups ++ NewDups).

answer() ->
    Dat = aoc21:data("05", fun dat:to_integer_list/1),
    % {length(Dat), lists:nth(1, Dat)},
    % get_bounds(Dat),
    {
        length(duplicated_items(plot_all(orthogonal_lines(Dat)))),
        length(duplicated_items(plot_all(Dat)))
    }.
