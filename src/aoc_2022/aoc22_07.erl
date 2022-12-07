-module(aoc22_07).

-export([answer/0]).

-record(file, {name, size}).
-record(dir, {name, files = [], dirs = []}).

new_file(Name, Size) -> #file{name = Name, size = list_to_integer(Size)}.

new_dir(Name) -> #dir{name = Name}.

new_or_existing_dir([], DirName) ->
    [new_dir(DirName)];
new_or_existing_dir(Existing, _) ->
    Existing.

add_dir(DirName, R) ->
    Existing = [D || D <- R#dir.dirs, D#dir.name =:= DirName],
    R#dir{dirs = R#dir.dirs ++ new_or_existing_dir(Existing, DirName)}.
add_file(File, R) ->
    Existing = [F || F <- R#dir.files, F#file.name =:= File#file.name],
    R#dir{files = (R#dir.files -- Existing) ++ [File]}.

process(Lines) ->
    {_, FS} = process(Lines, null),
    FS.
process([], Cwd) ->
    % Finished
    {[], Cwd};
process([[$$, $\s, $c, $d, $\s, $/] | T], _Cwd) ->
    % cd /
    process(T, new_dir("/"));
process([[$$, $\s, $c, $d, $\s, $., $.] | T], Cwd) ->
    % cd ..
    {T, Cwd};
process([[$$, $\s, $c, $d, $\s | Name] | T], Cwd) ->
    % cd <DirName>
    Dirs = Cwd#dir.dirs,
    [Sub] = [D || D <- Dirs, D#dir.name =:= Name],
    {T1, Sub1} = process(T, Sub),
    Cwd1 = Cwd#dir{dirs = (Dirs -- [Sub]) ++ [Sub1]},
    process(T1, Cwd1);
process([[$$, $\s, $l, $s] | T], Cwd) ->
    % ls
    process(T, Cwd);
process([[$$, $\s | T] | T], _Cwd) ->
    % <SomethingUnsupported>
    io:format("UNSUPPORTED COMMAND ~p~n", [T]),
    {error, T};
process([[$d, $i, $r, $\s | DirName] | T], Cwd) ->
    % dir <DirName>
    process(T, add_dir(DirName, Cwd));
process([FileDetails | T], Cwd) ->
    % <FileSize> <FileName>
    [Size, Name] = dat:extract(FileDetails, "(\\d+) (.+)"),
    File = new_file(Name, Size),
    process(T, add_file(File, Cwd)).

dir_size(Dir) ->
    SubDirs = [dir_size(D) || D <- Dir#dir.dirs],
    Size =
        lists:sum([F#file.size || F <- Dir#dir.files]) +
            lists:sum([SubSize || {_, SubSize} <- SubDirs]),
    {Dir#dir.name, Size}.

walk_fs(Dir, F) ->
    [F(Dir)] ++ lists:flatten([walk_fs(D, F) || D <- Dir#dir.dirs]).

process_dat(Line) -> binary_to_list(Line).

answer() ->
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
