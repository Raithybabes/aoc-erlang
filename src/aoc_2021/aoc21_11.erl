-module(aoc21_11).

-export([answer/0]).

-define(else, true).

-define(FLASH_MARKER, x).

octopus_step({X, Y, _V_STALE, Grid, Ext}) ->
    V = grid:get(X, Y, Grid),
    if
        V < 9 ->
            {grid:set(X, Y, V + 1, Grid), Ext};
        V =:= 9 ->
            NewGrid1 = grid:set(X, Y, ?FLASH_MARKER, Grid),
            Neighbours = grid:adjacent_and_diagonal_neighbours(X, Y, Grid),
            {NewGrid2, _} = lists:foldl(
                fun({NX, NY, _}, {NGrid, NExt}) -> octopus_step({NX, NY, null, NGrid, NExt}) end,
                {NewGrid1, Ext},
                Neighbours
            ),
            {NewGrid2, Ext};
        ?else ->
            {Grid, Ext}
    end.

octopus_flash({X, Y, V, Grid, {Steps, Flashes}}) when V =:= ?FLASH_MARKER ->
    {grid:set(X, Y, 0, Grid), {Steps, Flashes + 1}};
octopus_flash({_X, _Y, _V, Grid, {Steps, Flashes}}) ->
    {Grid, {Steps, Flashes}}.

process_flashes({Grid, {Steps, Flashes}}) ->
    grid:process_ext(fun octopus_flash/1, Grid, {Steps + 1, Flashes}).

finish_when_all_flash({{W, H, _G} = Grid, {Steps, Flashes}}) ->
    IsFinished = (Flashes =:= (W * H)) or (Steps =:= 1000),
    {Grid, {Steps, 0}, IsFinished}.

process_dat(Line) -> [list_to_integer(X) || X <- dat:match_global(Line, "\\d")].

answer() ->
    Dat = aoc21:data("11", fun process_dat/1),
    Grid = grid:from_list2d(Dat),
    {_, {_Steps, FlashesAfter100Steps}} = grid:process_ext_times(
        fun octopus_step/1, fun process_flashes/1, Grid, {0, 0}, 100
    ),
    {_, {StepsToAllFlash, _Flashes}} = grid:process_ext_until(
        fun octopus_step/1, fun process_flashes/1, fun finish_when_all_flash/1, Grid, {0, 0}
    ),
    {FlashesAfter100Steps, StepsToAllFlash}.
