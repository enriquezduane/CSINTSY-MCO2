% defines the module and export the predicates to be used by other files.
:- module(logic, [parent/2, child/2, sibling/2, brother/2, sister/2, father/2, mother/2, 
grandfather/2, grandmother/2, uncle/2, aunt/2, relative/2, male/1, female/1]).

% allows for facts to be added or removed from the database at runtime.
:- dynamic parent/2, child/2, sibling/2, brother/2, sister/2, father/2, mother/2, 
grandfather/2, grandmother/2, uncle/2, aunt/2, male/1, female/1.

% relationship definitions
sibling(X, Y) :- parent(Z, X), parent(Z, Y), X \= Y.
brother(X, Y) :- male(X), sibling(X, Y).
sister(X, Y) :- female(X), sibling(X, Y).
father(X, Y) :- male(X), parent(X, Y).
mother(X, Y) :- female(X), parent(X, Y).
grandfather(X, Y) :- father(X, Z), parent(Z, Y).
grandmother(X, Y) :- mother(X, Z), parent(Z, Y).
child(X, Y) :- parent(Y, X).
son(X, Y) :- male(X), child(X, Y).
daughter(X, Y) :- female(X), child(X, Y).
uncle(X, Y) :- brother(X, Z), parent(Z, Y).
aunt(X, Y) :- sister(X, Z), parent(Z, Y).

relative(X, Y) :- parent(X, Y) ; sibling(X, Y) ; child(X, Y) ; grandfather(X, Y) ; grandmother(X, Y) ; uncle(X, Y) ; aunt(X, Y).


