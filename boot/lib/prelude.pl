
% TODO: make prim
A ; _ :- cut_passthrough, A.
_ ; B :- cut_passthrough, B.

not(G) :- G, !, fail.
not(G).
