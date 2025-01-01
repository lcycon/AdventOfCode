:- use_module(library(clpfd)).

init(Data) :-
    set_prolog_flag(double_quotes, chars),
    parse_file("inputs/day13.txt", Data).

main :-
    init(Data),
    part1(Data, Part1),
    part2(Data, Part2),
    format("Part 1: ~w\n", [Part1]),
    format("Part 2: ~w\n", [Part2]),
    halt(0).

part1(Data, Result) :-
    convlist(game_solution, Data, Solutions),
    sum_list(Solutions, Result).

part2(Data, Result) :-
    maplist(part2_transform, Data, Transformed),
    convlist(game_solution, Transformed, Solutions),
    sum_list(Solutions, Result).

game_solution(block(button(AX,AY),button(BX,BY),prize(X,Y)), Price) :-
    1 #=< AP,
    1 #=< BP,
    X #= (AP * AX) + (BP * BX),
    Y #= (AP * AY) + (BP * BY),
    Price #= (AP * 3) + BP.

part2_transform(block(button(AX,AY),button(BX,BY),prize(X,Y)),block(button(AX,AY),button(BX,BY),prize(NX,NY))) :-
    NX #= X + 10000000000000,
    NY #= Y + 10000000000000.

% Parsing Code
% Entry point to parse the file
parse_file(File, Blocks) :-
    open(File, read, Stream),
    read_stream_to_codes(Stream, Codes),
    close(Stream),
    phrase(blocks(Blocks), Codes).

% --- Top-level DCG Rule for Blocks ---
blocks([Block|Rest]) -->
    block(Block),
    blank_lines,
    blocks(Rest).
blocks([]) --> [].

% --- Parse a Single Block ---
block(block(ButtonA, ButtonB, Prize)) -->
    button_a(ButtonA),
    button_b(ButtonB),
    prize(Prize).

% --- Parse Button A ---
button_a(button(X, Y)) -->
    "Button A: ", coords(X, Y), "\n".

% --- Parse Button B ---
button_b(button(X, Y)) -->
    "Button B: ", coords(X, Y), "\n".

% --- Parse Prize ---
prize(prize(X, Y)) -->
    "Prize: ", prize_coords(X, Y), ("\n"; []).

% --- Parse Coordinates (X+N, Y+M) ---
coords(X, Y) -->
    "X+", integer(X), ", Y+", integer(Y).

% --- Parse Prize Coordinates (X=N, Y=M) ---
prize_coords(X, Y) -->
    "X=", integer(X), ", Y=", integer(Y).

% --- Blank Lines Handling ---
blank_lines --> ("\n"; "\r\n"), blank_lines.
blank_lines --> [].

% --- Parse an Integer ---
integer(N) -->
    digit_sequence(Digits),
    { number_codes(N, Digits) }.

digit_sequence([D|Rest]) -->
    [D], { char_type(D, digit) },
    digit_sequence(Rest).
digit_sequence([]) --> [].
