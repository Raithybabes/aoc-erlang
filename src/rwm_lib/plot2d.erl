-module(plot2d).

-export([filter_orthogonal_lines/1, filter_diagonal_lines/1, plot_lines/3]).

-define(line, [X1, Y1, X2, Y2]).

dir(From, To) ->
    if
        To > From -> 1;
        From > To -> -1;
        From == To -> 0
    end.

filter_orthogonal_lines(Lines) ->
    lists:filter(fun(?line) -> (X1 == X2) or (Y1 == Y2) end, Lines).

filter_diagonal_lines(Lines) ->
    lists:filter(fun(?line) -> abs(X1 - X2) == abs(Y1 - Y2) end, Lines).

plot_lines([], Canvas, _F) ->
    Canvas;
plot_lines([Line | Unplotted], Canvas, F) ->
    plot_lines(Unplotted, plot_orthogonal_diagonal_line(Line, Canvas, F), F).

plot_orthogonal_diagonal_line(?line, Canvas, F) ->
    Xinc = dir(X1, X2),
    Yinc = dir(Y1, Y2),
    NumSteps = max(abs(X1 - X2), abs(Y1 - Y2)),
    plot_orthogonal_diagonal_line(X1, Y1, Xinc, Yinc, NumSteps, Canvas, F).
plot_orthogonal_diagonal_line(_, _, _, _, -1, Canvas, _) ->
    Canvas;
plot_orthogonal_diagonal_line(X, Y, Xinc, Yinc, Steps, Canvas, F) ->
    V = F(canvas2d:get(X, Y, Canvas)),
    C = canvas2d:set(X, Y, V, Canvas),
    plot_orthogonal_diagonal_line(X + Xinc, Y + Yinc, Xinc, Yinc, Steps - 1, C, F).
