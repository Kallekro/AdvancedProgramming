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

% friendly(G, X)
friendly(G, X) :- friendlyHelper(G, G, X).

friendlyHelper(_, [], _).
friendlyHelper(Gorg, [person(Y, _)|Gs], X) :-
    different(Gorg, X, Y),
    follows(Gorg, Y, X),
    follows(Gorg, X, Y),
    friendlyHelper(Gorg, Gs, X).

friendlyHelper(Gorg, [person(Z, [])|Gs], X) :-
    different(Gorg, Z, X),
    friendlyHelper(Gorg, Gs, X).

friendlyHelper(Gorg, [person(Z, [ZF|ZFs])|Gs], X) :-
    different(Gorg, Z, X),
    different(Gorg, ZF, X),
    friendlyHelper(Gorg, [person(Z, ZFs)|Gs], X).

friendlyHelper(Gorg, [person(X,_),Y|Gs], X) :-
    friendlyHelper(Gorg, [Y|Gs], X).

% hostile(G, X)
hostile(G, X) :- hostileHelper(G, G, X).

hostileHelper(_, [], _).
hostileHelper(Gorg, [person(Y, _)|Gs], X) :-
    different(Gorg, X, Y),
    follows(Gorg, Y, X),
    ignores(Gorg, X, Y),
    hostileHelper(Gorg, Gs, X).

hostileHelper(Gorg, [person(Z, [])|Gs], X) :-
    different(Gorg, Z, X),
    hostileHelper(Gorg, Gs, X).

hostileHelper(Gorg, [person(Z, [ZF|ZFs])|Gs], X) :-
    different(Gorg, Z, X),
    different(Gorg, ZF, X),
    hostileHelper(Gorg, [person(Z, ZFs)|Gs], X).

hostileHelper(Gorg, [person(X,_),Y|Gs], X) :-
    hostileHelper(Gorg, [Y|Gs], X).


%%% level 2 %%%

% aware(G, X, Y)

% ignorant(G, X, Y)

%%% level 3 %%%

% same_world(G, H, K)

% optional!
% different_world(G, H)
