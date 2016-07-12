
append([], Xs, Xs).
append([X|Xs], Ys, [X|Zs]) :- append(Xs, Ys, Zs).

between(A, A, A) :- ! .
between(A, B, A).
between(A, B, C) :- (AA is (A + 1)), between(AA, B, C).

member(A, [A|_]).
member(A, [_|T]) :- member(A, T).
