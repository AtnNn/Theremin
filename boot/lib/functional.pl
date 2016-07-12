call(G) :- G.
call(G, A) :- G =.. L, append(L, [A], LL), GG =.. LL, GG.
call(G, A, B) :- G =.. L, append(L, [A, B], LL), GG =.. LL, GG.
call(G, A, B, C) :- G =.. L, append(L, [A, B, C], LL), GG =.. LL, GG.
call(G, A, B, C, D) :- G =.. L, append(L, [A, B, C, D], LL), GG =.. LL, GG.
call(G, A, B, C, D, E) :- G =.. L, append(L, [A, B, C, D, E], LL), GG =.. LL, GG.
