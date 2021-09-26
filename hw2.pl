/*
Seymone Gugneja
3675 Homework 2 - Prolog
September 25, 2021
*/

%1A - Write the rules for a predicateisSet(S), which succeeds if the listS is a set
isMember(M, [M|_]).
isMember(M, [_|TAIL]) :-
  isMember(M, TAIL).

isSet([]). %base case

isSet([HEAD|TAIL]) :- \+
  isMember(HEAD, TAIL),
  isSet(TAIL).

%1B - Write the rules for a predicate subset(A,S), which succeeds if the set A is a subset of the set S.
subset([], _). %base case

subset([HEAD|TAIL], S) :-
  isMember(HEAD, S),
  subset(TAIL, S).

%1C - Write the rules for a predicate union(A,B,C), which succeeds if the union of sets A and B is the set C.
union([], B, B). %base case 1
union(A, [], A). %base case 2

union([HEAD|TAIL], B, C) :-
  isMember(HEAD, B),
  union(TAIL, B, C).

union([HEAD|TAIL], B, [HEAD|C]) :-
  \+ isMember(HEAD, B),
  union(TAIL, B, C).

%1D - Write the rules for a predicate intersection(A,B,C), which succeeds if the intersection of sets A and B is the set C.
intersection([], _, []). %base case 1
intersection(_, [], []). %base case 2

intersection([HEAD|TAIL], B, C) :-
  \+ isMember(HEAD, B),
  intersection(TAIL, B, C).

intersection([HEAD|TAIL], B, [HEAD|C]) :-
  isMember(HEAD, B),
  intersection(TAIL, B, C).

%2 - Write the rules for a predicate tally(E,L,N), which succeeds if N is the number of occurrences of element E in list L.
tally(_,[],0). %base case

tally(E, [HEAD|TAIL], N) :-
  E == HEAD,
  tally(E, TAIL, X),
  N is X+1.

tally(E, [HEAD|TAIL], N) :-
  E \= HEAD,
  tally(E, TAIL, N).

%3 - Write the rules for a predicate subst(X,Y,L,L1), which succeeds if list L1 is identical to list L except
% that all occurrences of X in L are replaced with Y in L1.
subst(_,_,[],[]). %base case

subst(X, Y, [X|TAIL1], [Y|TAIL2]) :-
  subst(X, Y, TAIL1, TAIL2).

subst(X, Y, [HEAD1|TAIL1], [HEAD1|TAIL2]) :-
  HEAD1 \= X,
  subst(X, Y, TAIL1, TAIL2).

%4 - Write the rules for a predicate insert(X,L,L1), which succeeds if list L1 is identical to the sorted list
% L with X inserted at the correct place.
insert(X,[],[X]). %base case

insert(X,[HEAD1|TAIL1],[HEAD1|TAIL2]) :-
  X > HEAD1,
  insert(X, TAIL1, TAIL2).

insert(X,[HEAD1|TAIL1],[X,HEAD1|TAIL1]) :-
  X < HEAD1.

%5 - Write the rules for a predicate flatten(A,B), which succeeds if A is a list (possibly containing sublists),
% and B is a list containing all elements in A and its sublists, but all at the same level.
flatten([],[]). %base case

flatten([HEAD|TAIL],B) :-
  flatten(HEAD, HEAD2),
  flatten(TAIL, TAIL2),
  append(HEAD2, TAIL2, B).

flatten([HEAD|TAIL],[HEAD|TAIL2]) :-
  HEAD \= [],
  HEAD \= [_|_],
  flatten(TAIL, TAIL2).
