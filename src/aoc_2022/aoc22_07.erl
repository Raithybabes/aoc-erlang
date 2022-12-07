-module(aoc22_07).

-export([answer/0]).

-record(file, {name, size}).
-record(dir, {name, files = [], dirs = []}).

new_file(Name, Size) -> #file{name = Name, size = list_to_integer(Size)}.

new_dir(Name) -> #dir{name = Name}.
add_dir(DirName, R) ->
    Existing = [D || D <- R#dir.dirs, D#dir.name =:= DirName],
    R#dir{dirs = R#dir.dirs ++ new_or_existing_dir(Existing, DirName)}.
add_file(File, R) ->
    Existing = [F || F <- R#dir.files, F#file.name =:= File#file.name],
    R#dir{files = (R#dir.files -- Existing) ++ [File]}.

new_or_existing_dir([], DirName) ->
    [new_dir(DirName)];
new_or_existing_dir(Existing, _) ->
    Existing.

process_dat(Line) -> binary_to_list(Line).

% process_line([$$, $\s, $c, $d, $\s, $/]) -> "ROOT";
% process_line([$$, $\s, $c, $d, $\s, $., $.]) -> "UP";
% process_line([$$, $\s, $c, $d, $\s | X]) -> "DOWN " ++ X;
% process_line([$$, $\s, $l, $s]) -> "LIST";
% process_line([$$, $\s | T]) -> "UNSUPPORTED COMMAND " ++ T;
% process_line([$d,$i,$r,$\s|X]) -> "-- DIR " ++ X;
% process_line(X) -> "-- FILE " ++ X.

test_data() ->
    [
        "$ cd /",
        "$ ls",
        "dir a",
        "14848514 b.txt",
        "8504156 c.dat",
        "dir d",
        "$ cd a",
        "$ ls",
        "dir e",
        "29116 f",
        "2557 g",
        "62596 h.lst",
        "$ cd e",
        "$ ls",
        "584 i",
        "$ cd ..",
        "$ cd ..",
        "$ cd d",
        "$ ls",
        "4060174 j",
        "8033020 d.log",
        "5626152 d.ext",
        "7214296 k"
    ].

process(Lines) ->
    {_, FS} = process(Lines, null, 1),
    FS.
process([], Cwd, _Pos) ->
    % io:format("~p EXIT ~p~n", [Pos, Cwd]),
    {[], Cwd};
process([[$$, $\s, $c, $d, $\s, $/] | T], _Cwd, Pos) ->
    % ROOT
    % io:format("~p ROOT ~p~n", [Pos, Cwd]),
    process(T, new_dir("/"), Pos + 1);
process([[$$, $\s, $c, $d, $\s, $., $.] | T], Cwd, _Pos) ->
    % UP
    % io:format("~p UP ~p~n", [Pos, Cwd]),
    {T, Cwd};
process([[$$, $\s, $c, $d, $\s | Name] | T], Cwd, Pos) ->
    % DOWN
    % io:format("~p DOWN ~p~p~n", [Pos, Name, Cwd]),
    Dirs = Cwd#dir.dirs,
    % io:format("-- Dirs ~p~n", [Dirs]),
    [Sub] = [D || D <- Dirs, D#dir.name =:= Name],
    {T1, Sub1} = process(T, Sub, Pos + 1),
    Cwd1 = Cwd#dir{dirs = (Dirs -- [Sub]) ++ [Sub1]},
    process(T1, Cwd1, Pos + 1);
process([[$$, $\s, $l, $s] | T], Cwd, Pos) ->
    % LIST
    % if
    %     length(Cwd#dir.files) + length(Cwd#dir.dirs) > 0 ->
    %         io:format("~p LIST !WARNING! Cwd has already been populated ~p~n", [Pos, Cwd]);
    %     true ->
    %         io:format("~p LIST ~p~n", [Pos, Cwd])
    % end,
    process(T, Cwd, Pos + 1);
process([[$$, $\s | T] | T], _Cwd, Pos) ->
    io:format("~p UNSUPPORTED COMMAND ~p~n", [Pos, T]),
    {error, Pos, T};
process([[$d, $i, $r, $\s | DirName] | T], Cwd, Pos) ->
    % DIR X
    % io:format("~p ++ DIR ~p ~p~n", [Pos, DirName, Cwd]),
    process(T, add_dir(DirName, Cwd), Pos + 1);
process([FileDetails | T], Cwd, Pos) ->
    % FILE X
    % io:format("~p ++ FILE ~p ~p~n", [Pos, FileDetails, Cwd]),
    [Size, Name] = dat:extract(FileDetails, "(\\d+) (.+)"),
    File = new_file(Name, Size),
    process(T, add_file(File, Cwd), Pos + 1).

dir_size(Dir) ->
    Subs = [dir_size(D) || D <- Dir#dir.dirs],
    Size =
        lists:sum([F#file.size || F <- Dir#dir.files]) +
            lists:sum([SubSize || {_, SubSize} <- Subs]),
    {Dir#dir.name, Size}.

walk_fs(Dir, F) ->
    [F(Dir)] ++ lists:flatten([walk_fs(D, F) || D <- Dir#dir.dirs]).

answer() ->
    % Dat = test_data(),
    Dat = aoc22:data("07", fun process_dat/1),
    FS = process(Dat),
    {_, RootBytes} = RootSize = dir_size(FS),
    Sizes = walk_fs(FS, fun dir_size/1) -- [RootSize],
    Part1 = lists:sum([Size || {_, Size} <- Sizes, Size =< 100000]),
    RemainingSpace = 70000000 - RootBytes,
    NeedToFree = 30000000 - RemainingSpace,
    Part2Candidates = [Size || {_, Size} <- Sizes, Size >= NeedToFree],
    Part2 = lists:min(Part2Candidates),
    {Part1, Part2}.
