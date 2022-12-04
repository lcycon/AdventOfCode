empty([]).
empty("").

file_lines(File, ReadInput, Groups) :-
    setup_call_cleanup(open(File, read, In),
       call(ReadInput, In, Groups),
       close(In)).

split(In, Sep, [Left|Rest]) :-
    append(Left, [Sep|Right], In), !, split(Right, Sep, Rest).
split(In, _Sep, [In]).

take(N, In, Out) :-
    prefix(Out, In),
    length(Out, N).

split_evenly(L, A, B) :-
    append(A, B, L),
    length(A, N),
    length(B, N).

part([], _, []).
part(L, N, [DL|DLTail]) :-
   length(DL, N),
   append(DL, LTail, L),
   part(LTail, N, DLTail).
