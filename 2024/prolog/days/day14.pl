:- [library(dcg/basics)].
:- [library(clpfd)].
:- [library(yall)].
:- pack_install('https://github.com/damianoazzolini/plstat.git').
:- [library(plstat)].

width(101).
height(103).

single_dimenstion_quadrant(Dim, 0, Max) :-
    Max #= Dim // 2.
single_dimenstion_quadrant(Dim, Min, Max) :-
    Min #= (Dim // 2) + 1,
    Max #= Dim.

quadrant(MinX, MaxX, MinY, MaxY) :-
    width(W),
    height(H),
    single_dimenstion_quadrant(W, MinX, MaxX),
    single_dimenstion_quadrant(H, MinY, MaxY).

init(Data) :-
    set_prolog_flag(double_quotes, chars),
    parse_file("inputs/day14.txt", Data).

main :-
    init(Data),
    part1(Data, Part1),
    part2(Data, Part2),
    format("Part 1: ~w\n", [Part1]),
    format("Part 2: ~w\n", [Part2]),
    halt(0).

% Part 1

part1(Data, Result) :-
    maplist(step(100), Data, Final),
    bagof(Count, bots_in_quadrant(Final, Count), Counts),
    foldl(mul, Counts, 1, Result).

mul(V1, V2, R) :- R #= V1 * V2.

bots_in_quadrant(Data, Count) :-
    quadrant(MinX, MaxX, MinY, MaxY),
    include(in_quadrant(MinX, MaxX, MinY, MaxY), Data, InQuadrant),
    length(InQuadrant, Count).

in_quadrant(MinX, MaxX, MinY, MaxY, point_velocity(point(X, Y), _)) :-
    X >= MinX,
    X < MaxX,
    Y >= MinY,
    Y < MaxY.

% Part 2

part2(Data, Result) :-
    Result in 0..1000000,
    label([Result]),
    maplist(step(Result), Data, NewData),
    maplist(coords, NewData, Coords),
    sort(Coords, Sorted),
    length(NewData, L1),
    length(Sorted, L2),
    L1 == L2,
    !,
    print_tree(NewData).
coords(point_velocity(P, _), P).

% Common

print_tree(Data) :-
    width(Width),
    height(Height),
    AdjustedHeight is Height - 1,
    forall(between(0, AdjustedHeight, Y), (
        print_row(Data, Width, Y),
        nl
    )).
    % (member(point_velocity(point(X, Y), _), Data) -> write('#'); write('.')),

print_row(Data, Width, Y) :-
    AdjustedWidth is Width - 1,
    forall(between(0, AdjustedWidth, X), (
        (member(point_velocity(point(X, Y), _), Data) -> write('#'); write('.'))
    )).

step(0, point_velocity(point(X, Y), velocity(VX, VY)), point_velocity(point(X, Y), velocity(VX, VY))).
step(N, point_velocity(point(X, Y), velocity(VX, VY)), point_velocity(point(X1, Y1), velocity(VX, VY))) :-
    N #> 0, !,
    width(Width),
    height(Height),
    X1 #= (X + (VX * N)) mod Width,
    Y1 #= (Y + (VY * N)) mod Height.

% File reading
parse_file(File, Data) :-
    phrase_from_file(lines(Data), File).

% Parse multiple lines
lines([Line|Lines]) -->
    line(Line), eol, !, lines(Lines).
lines([]) --> [].

% Parse a single line: p=X,Y v=VX,VY
line(point_velocity(point(X, Y), velocity(VX, VY))) -->
    "p=", integer(X), ",", integer(Y), " ",
    "v=", integer(VX), ",", integer(VY).
