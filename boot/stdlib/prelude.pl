A ; _ :- A.
_ ; B :- B.

append([], Xs, Xs).
append([X|Xs], Ys, [X|Zs]) :- append(Xs, Ys, Zs).

assertz_dcg(A --> B) :-
    convert_dcg_head(A, Ap, L, R),
    convert_dcg_body(B, Bp, L, R),
    assertz(Ap :- Bp).

convert_dcg_head(F, G, L, R) :-
    F =.. FArgs,
    append(FArgs, [L, R], GArgs),
    G =.. GArgs.

convert_dcg_body((A, B), (Ap, Bp), L, R) :-
    !,
    convert_dcg_body(A, Ap, L, X),
    convert_dcg_body(B, Bp, X, R).
convert_dcg_body(!,!,L,L) :-
    ! .
convert_dcg_body([], true, L, L) :- ! .
convert_dcg_body([H|T], append([H|T], R, L), L, R) :- ! .
convert_dcg_body(F, G, L, R) :-
    F =.. FArgs,
    append(FArgs, [L, R], GArgs),
    G =.. GArgs.
