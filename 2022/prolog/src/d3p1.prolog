#!/usr/bin/env swipl

:- [util].

:- initialization(main, main).

main :-
    file_lines("inputs/03.txt", stream_lines, Bags),
    maplist(process, Bags, Scores),
    sumlist(Scores, Score),
    print(Score),
    nl.

process(Bag, Score) :- common_elememnt(Bag, E), score(E, Score).

common_elememnt(Bag, Element) :-
    split_evenly(Bag, First, Second),
    intersection(First, Second, Inter),
    sort(Inter, [Element]).

score(Char, Score) :- Char @>= 'A', Char @=< 'Z', char_code(Char, Code), Score is Code - 38.
score(Char, Score) :- Char @>= 'a', Char @=< 'z', char_code(Char, Code), Score is Code - 96.

stream_lines(In, Bags) :-
    read_string(In, _, Str),
    split_string(Str, "\n", "", Strings),
    maplist(string_chars, Strings, BagsR),
    exclude(empty, BagsR, Bags).
