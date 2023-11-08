% defines the module and export the predicates to be used by other files.
:- module(logic, [parent/2, child/2]).

% allows for facts to be added or removed from the database at runtime.
:- dynamic parent/2.

% relationship definitions
child(X, Y) :- parent(Y, X).
sibling(X, Y) :- parent(Z, X), parent(Z, Y), X \= Y.
