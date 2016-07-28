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
convert_dcg_body([H|T], (append([H|T], R, L), ! ; string_concat([H|T], R, L)), L, R) :- ! .
convert_dcg_body(S, string_concat(S, R, L), L, R) :- string(S), ! .
convert_dcg_body(F, G, L, R) :-
    F =.. FArgs,
    append(FArgs, [L, R], GArgs),
    G =.. GArgs.

whitespace --> " "; [9]; [10].

','(A, B, L, R) :- call(A, L, M), call(B, M, R).

';'(A, B, L, R) :- call(A, L, R); call(B, L, R).

many0(G) --> call(G), many0(G); {true}.

many1(G) --> call(G), many0(G).

many0(G, [H|T]) --> call(G, H), many0(G, T).
many0(G, []) --> {true}.

many1(G, [H|T]) --> call(G, H), many0(G, T).

char(C, S, R) :- string_first(S, C), string_concat(C, R, S).

digit(D) --> char(C), { digit(C, D) }.

digit("0", 0).
digit("1", 1).
digit("2", 2).
digit("3", 3).
digit("4", 4).
digit("5", 5).
digit("6", 6).
digit("7", 7).
digit("8", 8).
digit("9", 9).

digit(C) :- digit(C, _).

alpha(C) --> char(C), { alpha(C) }.

alpha(C) :- member(C, [
    "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N",
    "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z",
    "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n",
    "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"]).

string_to_integer(S, N) :- integer(N, S, []).

integer(N) --> many1(digit, D), !, { fold(add_digit, 0, D, N) }.

add_digit(A, D, AA) :- AA is A * 10 + D.

oneof([X|_], X) --> [X].
oneof([_|Xs], X) --> oneof(Xs, X).
