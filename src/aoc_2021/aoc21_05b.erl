-module(aoc21_05b).

-export([answer/0]).

get_dimensions(Dat) ->
    Width = lists:max([max(X1, X2) || [X1, _, X2, _] <- Dat]) + 1,
    Height = lists:max([max(Y1, Y2) || [_, Y1, _, Y2] <- Dat]) + 1,
    {Width, Height}.

filter_overplotted(List) -> lists:filter(fun(V) -> V > 1 end, List).

count_overplotted_pixels_in_canvas(Canvas) -> length(filter_overplotted(canvas2d:to_list(Canvas))).

plot_value(V) -> V + 1.

answer() ->
    Dat = aoc21:data("05", fun dat:to_integer_list/1),
    Canvas = canvas2d:new(get_dimensions(Dat)),
    {
        count_overplotted_pixels_in_canvas(
            plot2d:plot_lines(plot2d:filter_orthogonal_lines(Dat), Canvas, fun plot_value/1)
        ),
        count_overplotted_pixels_in_canvas(plot2d:plot_lines(Dat, Canvas, fun plot_value/1))
    }.
