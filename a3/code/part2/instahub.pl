% AP2019 Assignment 3
% Skeleton for main part. Predicates to implement:

%%% level 0 %%%

% follows(G, X, Y)
follows([person(X, [Y|_])|_], X, Y).
follows([person(X, [_|Fs])|_], X, Y) :- follows([person(X, Fs)], X, Y).
follows([_|Gs], X, Y) :- follows(Gs, X, Y).

% aux. predicate: concat(L1, L2, L3)
% for concatting two lists L1 and L2 resulting in L3.
concat([],L,L).
concat([H|T], L2, [H|L3]) :- concat(T, L2, L3).

% aux. predicate: different(G, X, Y)
% for checking if X and Y are two different members of G.
different([person(X,_),person(Y,_)|_], X, Y).
different([person(Y,_),person(X,_)|_], X, Y).
different([person(X,XF),_|Gs], X, Y) :- different([person(X,XF)|Gs], X, Y).
different([person(Y,YF),_|Gs], X, Y) :- different([person(Y,YF)|Gs], X, Y).
different([_|Gs], X, Y) :- different(Gs, X, Y).

% ignores(G, X, Y)
ignores([person(X,[])|Gs], X, Y) :- follows(Gs, Y, X).
ignores([person(X,[XF|XFs])|Gs], X, Y) :-
    different(Gs, Y, XF),
    ignores([person(X,XFs)|Gs], X, Y).
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
friendly([G|Gs], X) :- allFollowers([G|Gs], X, L), followsAll([G|Gs], X, L).

% hostile(G, X)
hostile([G|Gs], X) :- allFollowers([G|Gs], X, L), ignoresAll([G|Gs], X, L).

% aux. predicate: allFollowers(G, X, L)
% For getting a list L of all people who follows X in G
allFollowers(G, X, L) :- allFollowersWorker(G, G, X, [], L).

% aux. predicate: allFollowersWorker(G, X, L)
% For doing the work of allFollowers using a few more parameters
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

% aux. predicate: followsAll(G, X, L)
% For checking that X follows all people in L
followsAll(_, _, []).
followsAll(G, X, [L|Ls]) :- follows(G, X, L), followsAll(G, X, Ls).

% aux. predicate: ignoresAll(G, X, L)
% For checking that X ignores all people in L
ignoresAll(_, _, []).
ignoresAll(G, X, [L|Ls]) :- ignores(G, X, L), ignoresAll(G, X, Ls).

%%% level 2 %%%
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
ignorant(G, X, Y) :-
    allAware(G, [X], AA),
    notIn(G, Y, AA).

% aux. predicate: allAware(G, L, Lo)
% For getting a list Lo of all people that the people in L are aware of
allAware(G, L, Lo) :-
    allFollowingOfList(G, L, AF),
    concatNoDups(G, L, AF, Ltmp),
    notEqLength(Ltmp, L),
    allAware(G, Ltmp, Lo).
allAware(G, L, L) :-
    allFollowingOfList(G, L, AF),
    concatNoDups(G, L, AF, Ltmp),
    eqLength(Ltmp, L).

% aux. predicate: allFollowingOfList(G, Li, Lo)
% For getting a list Lo of all people that are followed by people in Li
allFollowingOfList(_, [], []).
allFollowingOfList(G, [H|T], Lout) :-
    allFollowing(G, H, Ltmp),
    allFollowingOfList(G, T, Ltmp2),
    concatNoDups(G, Ltmp, Ltmp2, Lout).

% aux. predicate: allFollowing(G, X, XF)
% For getting the follower list of X
allFollowing([person(X, XF)|_], X, XF).
allFollowing([person(_, _)|Gs], X, XF) :-
    allFollowing(Gs, X, XF).

% aux. predicate: concatNoDups(L1, L2, Lo)
% For concatting two lists while removing duplicates between the lists.
concatNoDups(_, [], L2, L2).
concatNoDups(G, [H1|T1], L2, [H1|Lo]) :-
    notIn(G, H1, L2),
    concatNoDups(G, T1, L2, Lo).
concatNoDups(G, [H1|T1], L2, Lo) :-
    isIn(H1, L2),
    concatNoDups(G, T1, L2, Lo).

% aux. predicate: notIn(G, X, L)
% For checking that X is not in the list L
notIn(_, _, []).
notIn(G, X, [L|Ls]) :-
    different(G, X, L),
    notIn(G, X, Ls).

% aux. predicate: isIn(X, L)
% For checking that X is in the list L
isIn(H, [H|_]).
isIn(H, [_|T]) :- isIn(H, T).

% aux. predicate: eqLength(L1, L2)
% For checking that the two lists L1 and L2 have equal length
eqLength([], []).
eqLength([_|T1], [_|T2]) :-
    eqLength(T1, T2).

% aux. predicate: notEqLength(L1, L2)
% For checking that the two lists L1 and L2 does not have equal length
notEqLength([_|_], []).
notEqLength([], [_|_]).
notEqLength([_|T1], [_|T2]) :-
    notEqLength(T1, T2).

%%% level 3 %%%
% same_world(G, H, K)
same_world([], [], []).
same_world([G|Gs], [H|Hs], Pairs) :-
    findPairs([G|Gs], [H|Hs], Pairs),
    validate([G|Gs], [H|Hs], Pairs).

% aux. predicate: findPairs(G, H, K)
% Find a set of pairs K
findPairs(G, H, K) :-
    getNames(G, Gnames),
    getNames(H, [HnamesFst|HnamesRest]),
    findPairsWorker(Gnames, [HnamesFst|HnamesRest], HnamesFst, H, K).

% aux. predicate: findPairsWorker(L1, L2, L2orgH, G2, L3)
% For doing the work of findPairs using a few more parameters
findPairsWorker([H1], [H2], _, _, [p(H1, H2)]).
findPairsWorker([H1|T1], [H2fst,H2snd|T2], _, G, [p(H1, H2fst)|L3]) :-
    findPairsWorker(T1, [H2snd|T2], H2snd, G, L3).
findPairsWorker(L1, L2, L2orgH, G, L3) :-
    shiftList(L2, [L2ShiftH|L2ShiftT]),
    different(G, L2orgH, L2ShiftH),
    findPairsWorker(L1, [L2ShiftH|L2ShiftT], L2orgH, G, L3).

% aux. predicate: getNames(G, L)
% For getting a list of the unique names in G
getNames([], []).
getNames([person(X,_)|Gs], [X|L]) :- getNames(Gs, L).

% aux. predicate: shiftList(L, Lo)
% For shifting a list once to the left (Head becomes last)
shiftList([H|T], Lo) :- concat(T, [H], Lo).

% aux. predicate: validate(G, H, K)
% For validating a key on the graphs G and H
validate(G, H, K) :-
    getNames(G, Gnames),
    getNames(H, [HN1|HNs]),
    validateWorker(G, H, Gnames, [HN1|HNs], HN1, K).

% aux. predicate: validateWorker(G, H, Gnames, Hnames, HNorg1, K)
% For doing the work of validate using a few more parameters
validateWorker(G, H, [GN], [HN], _, K) :-
    allFollowing(G, GN, GF),
    allFollowing(H, HN, HF),
    connectionsMatch(G, H, GF, HF, K).

validateWorker(G, H, [GN1|GNs], [HN1, HN2|HNs], _, K) :-
    allFollowing(G, GN1, GF),
    allFollowing(H, HN1, HF),
    connectionsMatch(G, H, GF, HF, K),
    validateWorker(G, H, GNs, [HN2|HNs], HN2, K).

validateWorker(G, H, GN, HN, HNorg1, K) :-
    shiftList(HN, [HNShiftH|HNShiftT]),
    different(H, HNShiftH, HNorg1),
    validateWorker(G, H, GN, [HNShiftH|HNShiftT], HNorg1, K).

% aux. predicate: connectionsMatch(G, H, GF, HF)
% For checking if the people in GF matches those in HF with aliases
connectionsMatch(_, _, [], [], _).
connectionsMatch(G, H, [GF1|GFs], HF, K) :-
    getAlias(GF1, K, Alias),
    delete(H, Alias, HF, HFnew),
    connectionsMatch(G, H, GFs, HFnew, K).

% aux. predicate: getAlias(X, K)
% For getting the alias of X given a key K
getAlias(X, [p(X, Y)|_], Y).
getAlias(X, [_|Ks], Y) :- getAlias(X, Ks, Y).

% aux. predicate: delete(G, X, Li, Lo)
% For deleting X from Li yielding Lo (fails if X is not in Li).
delete(_, X, [X|T], T).
delete(G, X, [H|T], [H|Lo]) :-
    different(G, X, H),
    delete(G, X, T, Lo).
