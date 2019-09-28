% Rudimentary test suite. Feel free to replace anything

% Can run as: swipl -g run_tests -g halt instahub.pl instatest.pl

% The sample graphs from the assignment text:
g1([person(kara, [barry, clark]),
    person(bruce,[clark, oliver]),
    person(barry, [kara, oliver]),
    person(clark, [oliver, kara]),
    person(oliver, [kara])]).

g2([person(batman, [green_arrow, superman]),
    person(green_arrow, [supergirl]),
    person(supergirl, [flash, superman]),
    person(flash, [green_arrow, supergirl]),
    person(superman, [green_arrow, supergirl])]).

:- begin_tests(instahub).

% follows
test(follows1, [nondet]) :-
    g1(G), follows(G, bruce, clark).

test(follows2, [fail]) :-
    g1(G), follows(G, clark, bruce).

test(follows3, [set(X == [barry,clark,oliver])]) :-
    g1(G), follows(G, X, kara).

% different
test(different1, [nondet]) :-
    g1(G), different(G, kara, bruce).

test(different2, [fail]) :-
    g1(G), different(G, kara, kara).

test(different3, [fail]) :-
    g1(G), different(G, john, kara).

test(different4, [set(X == [bruce, barry, clark, oliver])]) :-
    g1(G), different(G, X, kara).

% ignores
test(ignores1, [nondet]) :-
    g1(G), ignores(G, kara, oliver).

test(ignores2, [fail]) :-
    g1(G), ignores(G, oliver, kara).

test(ignores3, [fail]) :-
    g1(G), ignores(G, bruce, barry).

test(ignores4, [set(X == [oliver])]) :-
    g1(G), ignores(G, kara, X).

% popular
test(popular1, [nondet]) :-
    g1(G), popular(G, kara).

test(popular2, [fail]) :-
    g1(G), popular(G, clark).

test(popular3, [set(X == [kara])]) :-
    g1(G), popular(G, X).

% outcast
test(outcast1, [nondet]) :-
    g1(G), outcast(G, bruce).

test(outcast2, [fail]) :-
    g1(G), outcast(G, kara).

test(outcast3, [set(X == [bruce, oliver])]) :-
    g1(G), outcast(G, X).

% friendly
test(friendly1, [nondet]) :-
    g1(G), friendly(G, barry).

test(friendly2, [fail]) :-
    g1(G), friendly(G, kara).

test(friendly3, [set(X == [barry, bruce])]) :-
    g1(G), friendly(G, X).

% hostile
test(hostile1, [nondet]) :-
    g1(G), hostile(G, oliver).

test(hostile2, [fail]) :-
    g1(G), hostile(G, kara).

test(hostile3, [set(X == [oliver])]) :-
    g1(G), hostile(G, X).

:- end_tests(instahub).
