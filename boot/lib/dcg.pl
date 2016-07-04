
whitespace --> " "; [9]; [10].

many0(G) --> G, !, many0(G); {true}.

many1(G) --> G, many0(G).

char(C, S, R) :- string_first(S, C), string_append(C, R, S).

digit(D) --> char(D), { member(D, ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]) }.
