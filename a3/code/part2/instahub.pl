% AP2019 Assignment 3
% Skeleton for main part. Predicates to implement:

%%% level 0 %%%

% follows(G, X, Y)
% case 1: If the first element of X's follow list is Y, succeed.
follows([person(X, [Y|_])|_], X, Y).
% case 2: Recurse with the first element of X's follow list removed
follows([person(X, [_|Fs])|_], X, Y) :- follows([person(X, Fs)], X, Y).
% case 3: If the first element of G is not X remove it and recurse
follows([_|Gs], X, Y) :- follows(Gs, X, Y).

% aux. predicate: concat(L1, L2, L3)
% for concatting two lists L1 and L2 resulting in L3.
concat([],L,L).
concat([H|T], L2, [H|L3]) :- concat(T, L2, L3).

% aux. predicate: different(G, X, Y)
% for checking if X and Y are two different members of G.
% case 1+2: If G starts with (X,Y) or (Y,X), succeed.
different([person(X,_),person(Y,_)|_], X, Y).
different([person(Y,_),person(X,_)|_], X, Y).
% case 3+4: If G starts with (X,Z) or (Y,Z) remove Z and recurse.
different([person(X,XF),_|Gs], X, Y) :- different([person(X,XF)|Gs], X, Y).
different([person(Y,YF),_|Gs], X, Y) :- different([person(Y,YF)|Gs], X, Y).
% case 5: Remove the first element of G and recurse.
different([_|Gs], X, Y) :- different(Gs, X, Y).

% ignores(G, X, Y)
% case 1: If X has empty follow list and Y follows X, succeed.
ignores([person(X,[])|Gs], X, Y) :- follows(Gs, Y, X).
% case 2: If the first element of X's follow list is not Y,
% recurse by removing that element from X's follow list.
ignores([person(X,[XF|XFs])|Gs], X, Y) :-
    different(Gs, Y, XF),
    ignores([person(X,XFs)|Gs], X, Y).
% case 3: If the first element of G is not X,
% re-position it to the back and recurse.
ignores([person(Z,ZF)|Gs], X, Y) :-
    different([person(Z,ZF)|Gs], Z, X),
    concat(Gs, [person(Z,ZF)], Gnew),
    ignores(Gnew, X, Y).

%%% level 1 %%%

% popular(G, X)
popular([person(X,[])|_], X).
popular([person(X, [XF|XFs])|Gs], X) :-
    follows([person(X,[XF|XFs])|Gs], XF, X),
    popular([person(X,XFs)|Gs], X).
popular([person(Z,ZF)|Gs], X) :-
    different([person(Z,ZF)|Gs], Z, X),
    concat(Gs, [person(Z,ZF)], Gnew),
    popular(Gnew, X).

% outcast(G, X)
outcast([person(X,[])|_], X).
outcast([person(X, [XF|XFs])|Gs], X) :-
    ignores([person(X,[XF|XFs])|Gs], XF, X),
    outcast([person(X,XFs)|Gs], X).
outcast([person(Z,ZF)|Gs], X) :-
    different([person(Z,ZF)|Gs], Z, X),
    concat(Gs, [person(Z,ZF)], Gnew),
    outcast(Gnew, X).

% allFollowers(G, X, L)
allFollowers(G, X, L) :- allFollowersWorker(G, G, X, [], L).

% allFollowersWorker(Gorg, G, X, L1, L2)
allFollowersWorker(_, [], _, L, L).
allFollowersWorker(Gorg, [person(Y, _)|Gs], X, L, [Y|Lout]) :-
    different(Gorg, Y, X),
    follows(Gorg, Y, X),
    allFollowersWorker(Gorg, Gs, X, L, Lout).
allFollowersWorker(Gorg, [person(Y, [])|Gs], X, L, Lout) :-
    different(Gorg, Y, X),
    allFollowersWorker(Gorg, Gs, X, L, Lout).
allFollowersWorker(Gorg, [person(Y, [YF|YFs])|Gs], X, L, Lout) :-
    different(Gorg, Y, X),
    different(Gorg, YF, X),
    allFollowersWorker(Gorg, [person(Y, YFs)|Gs], X, L, Lout).
allFollowersWorker(Gorg, [person(X, _)|Gs], X, L, Lout) :-
    allFollowersWorker(Gorg, Gs, X, L, Lout).

% followsAll(G, X, L)
followsAll(_, _, []).
followsAll(G, X, [L|Ls]) :- follows(G, X, L), followsAll(G, X, Ls).

% friendly(G, X)
friendly(G, X) :- allFollowers(G, X, L), followsAll(G, X, L).

% ignoresAll(G, X, L)
ignoresAll(_, _, []).
ignoresAll(G, X, [L|Ls]) :- ignores(G, X, L), ignoresAll(G, X, Ls).

% hostile(G, X)
hostile(G, X) :- allFollowers(G, X, L), ignoresAll(G, X, L).

%%% level 2 %%%

% aware(G, X, Y)

aware(G, X, Y) :- follows(G, X, Y).

aware([person(X,[XF|_])|Gs], X, Y) :-
    different([person(X,[XF|_])|Gs], X, Y),
    aware(Gs, XF, Y). % tried to remove cycles here

aware([person(X,[_|XFs])|Gs], X, Y) :-
    different([person(X,[_|XFs])|Gs], X, Y),
    aware([person(X,XFs)|Gs], X, Y).

aware([person(Z,ZF)|Gs], X, Y) :-
    different([person(Z,ZF)|Gs], Z, X),
    concat(Gs, [person(Z,ZF)], Gnew),
    aware(Gnew, X, Y).

% ignorant(G, X, Y)

%%% level 3 %%%

% same_world(G, H, K)

% optional!
% different_world(G, H)
