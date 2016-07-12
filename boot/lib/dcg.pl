assertz_dcg(A --> B) :-
    convert_dcg_head(A, Ap, L, R),
    convert_dcg_body(B, Bp, L, R),
    assertz(Ap :- Bp).

convert_dcg_head(F, G, L, R) :-
    F =.. FArgs,
    append(FArgs, [L, R], GArgs),
    G =.. GArgs.

convert_dcg_body(V,_,_,_) :- var(V), !, fail.
convert_dcg_body((A, B), (Ap, Bp), L, R) :-
    !,
    convert_dcg_body(A, Ap, L, X),
    convert_dcg_body(B, Bp, X, R).
convert_dcg_body((A; B), (Ap; Bp), L, R) :-
    !,
    convert_dcg_body(A, Ap, L, R),
    convert_dcg_body(B, Bp, L, R).
convert_dcg_body(!, (!, C), L, R) :- !, C=(L=R, ! ; string_concat([], L, R)).
convert_dcg_body({X},(X,C),L,R) :- !, C=(L=R, ! ; string_concat([], L, R)).
convert_dcg_body([], C, L, R) :- !, C=(L=R, ! ; string_concat([], L, R)).
convert_dcg_body([H|T], append([H|T], R, L), L, R) :- ! .
convert_dcg_body(S, string_concat(S, R, L), L, R) :- string(S), ! .
convert_dcg_body(F, G, L, R) :-
    F =.. FArgs,
    append(FArgs, [L, R], GArgs),
    G =.. GArgs.

whitespace --> " "; [9]; [10].

many0(G) --> call(G), many0(G); {true}.

many1(G) --> call(G), many0(G).

many0(G, [H|T]) --> call(G, H), many0(G, T).
many0(G, []) --> {true}.

many1(G, [H|T]) --> call(G, H), many0(G, T).

char(C, S, R) :- string_first(S, C), string_concat(C, R, S).

digit(D) --> char(D), { digit(D) }.

digit(D) :- member(D, ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]).

alpha(C) --> char(C), { alpha(C) }.

alpha(C) :- member(C, [
    "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N",
    "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z",
    "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n",
    "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"]).
