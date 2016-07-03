sexp(A) --> { A =.. [_] }, !, [A].
sexp(F) --> { F =.. [H|T] }, !, ["(", H], sexp_args(T), ")".
sexp(X) --> [X].

sexp_args([]) --> [].
sexp_args([X|Xs]) --> sexp_arg(X), sexp_args(Xs).

sexp_arg(X) --> " ", sexp(X).
