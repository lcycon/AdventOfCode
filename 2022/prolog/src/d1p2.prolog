#!/usr/bin/env swipl

:- [util].

:- initialization(main, main).

main :-
    file_lines("inputs/01.txt", stream_lines, Groups),
    maplist(sumlist, Groups, Summed),
    sort(0, @>, Summed, Sorted),
    take(3, Sorted, Elems),
    sumlist(Elems, Elem),
    print(Elem).

stream_lines(In, Groups) :-
    read_string(In, _, Str),
    split_string(Str, "\n", "", List),
    split(List, "", StringGroups),
    maplist(maplist(atom_number), StringGroups, GroupsPre),
    exclude(empty, GroupsPre, Groups).

