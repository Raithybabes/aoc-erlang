-module(aoc22_02).

-export([answer/0]).

outcome(Opp, You) when Opp =:= You -> dra;
outcome(roc, pap) -> win;
outcome(roc, sci) -> los;
outcome(pap, roc) -> los;
outcome(pap, sci) -> win;
outcome(sci, roc) -> win;
outcome(sci, pap) -> los.

cheat(Opp, IWantTo) ->
    Results = [{outcome(Opp, Try), Try} || Try <- [roc, pap, sci]],
    [Play] = [Play || {Outcome, Play} <- Results, Outcome =:= IWantTo],
    Play.

score({Outcome, Shape}) -> score(Shape) + score(Outcome);
score(win) -> 6;
score(dra) -> 3;
score(los) -> 0;
score(roc) -> 1;
score(pap) -> 2;
score(sci) -> 3.

translate_1("A") -> roc;
translate_1("B") -> pap;
translate_1("C") -> sci;
translate_1("X") -> roc;
translate_1("Y") -> pap;
translate_1("Z") -> sci.

translate_2("X") -> los;
translate_2("Y") -> dra;
translate_2("Z") -> win.

play_1([OppDat, YouDat]) ->
    Opp = translate_1(OppDat),
    You = translate_1(YouDat),
    score({outcome(Opp, You), You}).

play_2([OppDat, YouDat]) ->
    Opp = translate_1(OppDat),
    You = cheat(Opp, translate_2(YouDat)),
    score({outcome(Opp, You), You}).

total_score(FStrategy, Dat) -> lists:sum([FStrategy(X) || X <- Dat]).

process_dat(X) -> dat:extract(X, "(\\w) (\\w)").

answer() ->
    Dat = aoc22:data("02", fun process_dat/1),
    Total_1 = total_score(fun play_1/1, Dat),
    Total_2 = total_score(fun play_2/1, Dat),
    {Total_1, Total_2}.
