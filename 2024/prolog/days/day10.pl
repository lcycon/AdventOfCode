:- table reachable_by_valid_trail/4.
:- initialization main.

main :-
    init,
    part1(Score1),
    format('Part 1: ~w~n', [Score1]),
    part2(Score2),
    format('Part 2: ~w~n', [Score2]),
    halt(0).

part1(Score) :-
    findall(Score, (
            trailhead(X, Y),
            trailhead_score(X, Y, Score)
    ), Scores),
    sum_list(Scores, Score).

part2(Score) :-
    findall((X,Y), (
            trailhead(X, Y),
            trailend(X2, Y2),
            reachable_by_valid_trail(X, Y, X2, Y2)
    ), Trails),
    length(Trails, Score).

trailhead(X, Y) :-
    probmap(X, Y, 0).

trailend(X, Y) :-
    probmap(X, Y, 9).

trailhead_score(X, Y, Score) :-
    trailhead(X, Y),
    setof((X2, Y2), (
            trailend(X2, Y2),
            reachable_by_valid_trail(X, Y, X2, Y2)
    ), Trails),
    length(Trails, Score).

reachable_by_valid_trail(X1, Y1, X2, Y2) :-
    probmap(X1, Y1, 8),
    neighbor(X1, Y1, X2, Y2),
    probmap(X2, Y2, 9).
reachable_by_valid_trail(X1, Y1, X2, Y2) :-
    probmap(X1, Y1, H1),
    neighbor(X1, Y1, XN, YN),
    succ(H1, HN),
    probmap(XN, YN, HN),
    reachable_by_valid_trail(XN, YN, X2, Y2).

neighbor(X, Y, XN, Y) :- XN is X + 1.
neighbor(X, Y, XN, Y) :- XN is X - 1.
neighbor(X, Y, X, YN) :- YN is Y + 1.
neighbor(X, Y, X, YN) :- YN is Y - 1.

% Dumb init stuff

init :-
    read_2d_vector('inputs/day10.txt', Input),
    assert_vector(Input).

read_2d_vector(FileName, Vector) :-
    open(FileName, read, Stream),
    read_lines(Stream, Lines),
    close(Stream),
    maplist(string_chars, Lines, Vector).

read_lines(Stream, []) :-
    at_end_of_stream(Stream), !.
read_lines(Stream, [Line|Lines]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, Line),
    read_lines(Stream, Lines).

assert_row(_, [], _).
assert_row(Y, [E|Rest], X) :-
    atom_number(E, Num),
    assert(probmap(X, Y, Num)),
    NextX is X + 1,
    assert_row(Y, Rest, NextX).

assert_map([], _, _).
assert_map([Row|RestRows], Y, InitX) :-
    assert_row(Y, Row, InitX),
    NextY is Y + 1,
    assert_map(RestRows, NextY, InitX).

assert_vector(Vector) :-
    assert_map(Vector, 0, 0).
