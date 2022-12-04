#!/usr/bin/env swipl

:- use_module(library(yall)).
:- use_module(library(apply)).
:- use_module(library(apply_macros)).

:- [util].

:- initialization(main, main).

win("C", "X").
win("A", "Y").
win("B", "Z").

tie("A", "X").
tie("B", "Y").
tie("C", "Z").

pair_score(A, B, 6) :- win(A, B), !.
pair_score(A, B, 3) :- tie(A, B), !.
pair_score(A, B, 0) :- \+ win(A, B), \+ tie(A, B).

score(A, "X", Points) :- pair_score(A, "X", PairPoints), Points is PairPoints + 1.
score(A, "Y", Points) :- pair_score(A, "Y", PairPoints), Points is PairPoints + 2.
score(A, "Z", Points) :- pair_score(A, "Z", PairPoints), Points is PairPoints + 3.

score_game([A,B], Score) :- score(A, B, Score).
score_game(_, 0).

stream_lines(In, Games) :-
    read_string(In, _, Str),
    split_string(Str, "\n", "", List),
    maplist([In, Out]>>split_string(In, " ", "", Out), List, Games).

main :- file_lines("inputs/02.txt", stream_lines, Games), maplist(score_game, Games, GameScores), sumlist(GameScores, Score), print(Score), nl.
