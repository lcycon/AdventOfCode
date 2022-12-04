#!/usr/bin/env swipl

:- use_module(library(yall)).
:- use_module(library(apply)).
:- use_module(library(apply_macros)).

:- [util].

:- initialization(main, main).

digit(D) --> [D], { code_type(D, digit) }.

digits([D|Ds]) --> digit(D), digits(Ds).
digits([D]) --> digit(D).

num(N) --> digits(Cs), { number_chars(N, Cs) }.

pair(tkPair(One, Two)) --> range(One), comma, range(Two).
range(tkRange(One, Two)) --> num(One), dash, num(Two).
comma --> [','].
dash --> ['-'].

main :- file_lines("inputs/04.txt", stream_lines, Pairs),
    include(contains, Pairs, Containing),
    length(Containing, CCount),
    print(CCount),
    nl,
    include(overlaps, Pairs, Overlapping),
    length(Overlapping, OCount),
    print(OCount),
    nl.

contains(tkPair(tkRange(AS, AE), tkRange(BS, BE))) :- AS @=< BS, BE @=< AE; BS @=< AS, AE @=< BE.

overlaps(tkPair(tkRange(AS, AE), tkRange(BS, BE))) :- (AS @>= BS, AS @=< BE; AE @>= BS, AE @=< BE); (BS @>= AS, BS @=< AE; BE @>= AS, BE @=< AE).

stream_lines(In, Pairs) :-
    read_string(In, _, Str),
    split_string(Str, "\n", "", ListR),
    exclude(empty, ListR, List),
    maplist(parse_line, List, Pairs).

parse_line(Line, Out) :- string_chars(Line, Chars), phrase(pair(Out), Chars, []).
