:- string_concat(X,b,ab), print(X), nl.
:- string_concat(a,X,ab), print(X), nl.
:- string_concat(a,b,X), print(X), nl.
:- string_concat(a,"b","ab"), print(yes), nl; print(no), nl.
:- string_concat(a,"b","abc"), print(yes), nl; print(no), nl.
