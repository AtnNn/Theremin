z3_open(z3(In, Out, Err)) :-
    process_create("z3", ["-in"], In, Out, Err, _).

z3_close(z3(In, Out, Err)) :-
    close(In),
    close(Out),
    close(Err).

z3_query(z3(In, Out, _), Q) :-
    to_sexp(Q, S),
    write_string(In, S),
    read_string(Out, 256, R),
    print(R).

from_sexp(S, A) :- from_sexp(A, S, []), ! .

from_sexp(X) --> many1(whitespace), !, from_sexp(X) .
from_sexp(String) -->
    ['"'], !, many1(string_char, String), ['"'].
from_sexp(Integer) -->integer(Integer), ! .
from_sexp(Atom) -->
    sym_char(C), { not(digit(C)) }, !, many0(sym_char, S), { atom_string(Atom,[C|S]) }.
from_sexp(C) -->
    "(", !, many0(whitespace), from_sexp_multi(Terms), many0(whitespace), ")",
    { C =.. Terms, ! ; C = Terms }.

from_sexp_multi([]) --> [] .
from_sexp_multi([H|T]) --> from_sexp(H), whitespace, from_sexp_multi(T).

string_char(C) --> [92], !, char(C).
string_char(_) --> ['"'], !, { fail }.
string_char(C) --> char(C).

sym_char(C) --> char(C), { alpha(C); digit(C); C="-" }.

to_sexp(A, S) :- to_sexp(A, S, []), ! .

to_sexp(A) --> { A =.. [_] }, !, [A].
to_sexp(F) --> { F =.. [H|T] }, !, ["(", H], sexp_args(T), ")".
to_sexp(X) --> [X].

sexp_args([]) --> [].
sexp_args([X|Xs]) --> sexp_arg(X), sexp_args(Xs).

sexp_arg(X) --> " ", sexp(X).


