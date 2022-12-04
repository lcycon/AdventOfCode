#!/usr/bin/env swipl

:- [util].

:- initialization(main, main).

main :-
    file_lines("inputs/03.txt", stream_lines, Groups),
    maplist(process, Groups, Scores),
    sumlist(Scores, Score),
    print(Score),
    nl.

process(Bags, Score) :- common_elememnt(Bags, E), score(E, Score).

common_elememnt([Bag1, Bag2, Bag3], Element) :-
    intersection(Bag1, Bag2, Inter1),
    intersection(Inter1, Bag3, Inter2),
    sort(Inter2, [Element]).

score(Char, Score) :- Char @>= 'A', Char @=< 'Z', char_code(Char, Code), Score is Code - 38.
score(Char, Score) :- Char @>= 'a', Char @=< 'z', char_code(Char, Code), Score is Code - 96.

stream_lines(In, Groups) :-
    read_string(In, _, Str),
    split_string(Str, "\n", "", Strings),
    maplist(string_chars, Strings, BagsR),
    exclude(empty, BagsR, Bags),
    part(Bags, 3, Groups).
