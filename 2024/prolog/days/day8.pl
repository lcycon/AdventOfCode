:- initialization main.

init :-
    read_2d_vector('inputs/day08.txt', Input),
    assert_vector(Input).

main :-
    init,
    part1(Count1),
    print(Count1), nl,
    part2(Count2),
    print(Count2), nl,
    halt(0).

part1(Count) :-
    setof((X, Y), E^antinode(1, X, Y, E), Pairs),
    length(Pairs, Count).

part2(Count) :-
    setof((X, Y), E^antinode(0, X, Y, E), Pairs),
    length(Pairs, Count).

antinode(Distance, X, Y, E) :-
    matching_pair(X1, Y1, X2, Y2, E),
    antinodes_for(Distance, X1, Y1, X2, Y2, X, Y).

antinodes_for(Distance, X1, Y1, X2, Y2, X, Y) :-
    XDiff is X1 - X2,
    YDiff is Y1 - Y2,
    GCD is gcd(XDiff, YDiff),
    NXDiff is XDiff / GCD,
    NYDiff is YDiff / GCD,
    (
        (is_diff_off(X1, NXDiff, X, N1), is_diff_off(Y1, NYDiff, Y, N1));
        (is_diff_off(X2, NXDiff, X, N2), is_diff_off(Y2, NYDiff, Y, N2))
    ),
    (
        Distance is 0;
        (
            distance_from(X1, Y1, X, Y, D1),
            distance_from(X2, Y2, X, Y, D2),
            (D1 is 2*D2; D2 is 2*D1)
        )
    ),
    probmap(X, Y, _).

distance_from(X1, Y1, X2, Y2, Distance) :-
    Distance is sqrt((X2 - X1) ** 2 + (Y2 - Y1) ** 2).

matching_pair(X1, Y1, X2, Y2, E) :-
    probmap(X1, Y1, E1),
    probmap(X2, Y2, E2),
    E1 \= '.', E1 \= '#',
    X1 \= X2, Y1 \= Y2, E1 == E2,
    (X1 < X2; (X1 == X2, Y1 < Y2)),
    E = E1.

is_diff_off(Base, Diff, Out, N) :-
    between(-100, 100, N),
    Out is Base + (Diff * N).
% is_diff_off(Base, Diff, Out, N) :-
%     number(Diff), Diff \= 0,
%     number(Base),
%     number(Out),
%     N is (Out - Base) / Diff,
%     integer(N).

% read_2d_vector(+FileName, -Vector)
% Reads a text file and converts it to a 2D list of characters.
read_2d_vector(FileName, Vector) :-
    open(FileName, read, Stream),
    read_lines(Stream, Lines),
    close(Stream),
    maplist(string_chars, Lines, Vector). % Convert each line to a list of characters

% read_lines(+Stream, -Lines)
% Reads all lines from a stream into a list of strings.
read_lines(Stream, []) :-
    at_end_of_stream(Stream), !. % Base case: Stop when the end of the file is reached
read_lines(Stream, [Line|Lines]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, Line),
    read_lines(Stream, Lines).

assert_row(_, [], _). % End of row
assert_row(Y, [E|Rest], X) :-
    assert(probmap(X, Y, E)), % Assert map fact for current element
    NextX is X + 1,       % Move to the next column
    assert_row(Y, Rest, NextX).

assert_map([], _, _). % End of vector, no assertions
assert_map([Row|RestRows], Y, InitX) :-
    assert_row(Y, Row, InitX), % Assert map facts for the current row
    NextY is Y + 1,            % Move to the next row
    assert_map(RestRows, NextY, InitX).

% Helper predicate to assert map facts starting from (0, 0)
assert_vector(Vector) :-
    assert_map(Vector, 0, 0). % Start at row 0, column 0

take(0, _, []).
take(_, [], []).
take(N, [H|T], [H|R]) :-
    N > 0,                     % Ensure N is greater than 0
    N1 is N - 1,               % Decrease N by 1
    take(N1, T, R).            % Recurse with the tail
