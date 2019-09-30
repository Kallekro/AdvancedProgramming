% AP2019 Assignment 3
% Skeleton for warm-up part. Predicates to implement:

% add(N1, N2, N)

add(X, z, X).
add(X, s(Y), s(N)) :- add(X, Y, N).

% mult(N1, N2, N)
mult(z, _, z).
mult(s(X), Y, N) :- mult(X, Y, M), add(Y, M, N).

% comp(N1, N2, A)
comp(z, z, eq).
comp(z, s(_), lt).
comp(s(_), z, gt).
comp(s(X), s(Y), A) :- comp(X, Y, A).

% insert(N, TI, TO)
insert(N, leaf, node(N, leaf, leaf)).
insert(N, node(N, T1, T2), node(N, T1, T2)).% :- comp(N, M, eq).
insert(N, node(M, T1, T2), node(M, T1new, T2)) :-
    comp(N, M, lt), insert(N, T1, T1new).
insert(N, node(M, T1, T2), node(M, T1, T2new)) :-
    comp(N, M, gt), insert(N, T2, T2new).

% insertlist(Ns, TI, TO)
insertlist([], T, T).
insertlist([N|Ns], Tin, Tout) :- insert(N, Tin, T1), insertlist(Ns, T1, Tout).
