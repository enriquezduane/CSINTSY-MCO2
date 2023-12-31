% defines the module and export the predicates to be used by other files.
:- module(logic, [parent/2, child/2, sibling/2, brother/2, sister/2, father/2, mother/2, 
grandfather/2, grandmother/2, son/2, daughter/2, uncle/2, aunt/2, relative/2, male/1, female/1, 
impossible_sibling/2, impossible_brother/2, impossible_sister/2, impossible_parent/2, impossible_parent/3, impossible_father/2, 
impossible_mother/2, impossible_grandmother/2, impossible_grandfather/2, impossible_child/2, impossible_son/2, impossible_daughter/2,
impossible_uncle/2, impossible_aunt/2]).

% allows for facts to be added or removed from the database at runtime.
:- dynamic parent/2, sibling/2, brother/2, sister/2, father/2, mother/2, grandfather/2, grandmother/2, 
child/2, son/2, daughter/2, uncle/2, aunt/2, relative/2, male/1, female/1, impossible_sibling/2, impossible_brother/2,
impossible_sister/2, impossible_parent/2, impossible_father/2, impossible_mother/2, impossible_grandmother/2, 
impossible_grandfather/2, impossible_child/2, impossible_son/2, impossible_daughter/2, impossible_uncle/2, impossible_aunt/2.

% relationship definitions
sibling(X, Y) :- parent(Z, X), parent(Z, Y), X \= Y.
brother(X, Y) :- male(X), sibling(X, Y).
sister(X, Y) :- female(X), sibling(X, Y).
father(X, Y) :- male(X), parent(X, Y).
mother(X, Y) :- female(X), parent(X, Y).
grandfather(X, Y) :- father(X, Z), parent(Z, Y).
grandmother(X, Y) :- mother(X, Z), parent(Z, Y).
child(X, Y) :- parent(Y, X).
son(X, Y) :- male(X), parent(Y, X).
daughter(X, Y) :- female(X), parent(Y, X).
uncle(X, Y) :- brother(X, Z), parent(Z, Y).
aunt(X, Y) :- sister(X, Z), parent(Z, Y).
cousin(X, Y) :- parent(Z, X), sibling(Z, C), parent(C, Y).
relative(X, Y) :- parent(X, Y) ; child(X, Y) ; sibling(X, Y) ; grandfather(X, Y) ; grandmother(X, Y)
; uncle(X, Y) ; aunt(X, Y) ; cousin(X, Y).


% impossible relationship definitions
impossible_sibling(X, Y) :- X = Y ; different_parents(X, Y) ; parent(X, Y) ; child(X, Y) ; grandfather(X, Y) ; grandfather(Y, X) 
; grandmother(X, Y) ; grandmother(Y, X) ; uncle(X, Y) ; uncle(Y, X) ; aunt(X, Y) ; aunt(Y, X).

impossible_brother(X, Y) :- female(X) ; impossible_sibling(X, Y).

impossible_sister(X, Y) :- male(X) ; impossible_sibling(X, Y).

impossible_parent(X, Y) :- X = Y ; already_has_parents(Y, X) ; sibling_has_different_parents(X, Y) ; child(X, Y) ; sibling(X, Y) 
; grandfather(X, Y) ; grandfather(Y, X); grandmother(X, Y) ; grandmother(Y, X) ; uncle(X, Y) ; uncle(Y, X) ; aunt(X, Y) ; aunt(Y, X).

impossible_parent(X, Y, Z) :- X = Y ; already_has_parents(Z, Y, X) ; impossible_parent(X, Z) ; impossible_parent(Y, Z).

impossible_father(X, Y) :- female(X) ; already_has_father(Y) ; impossible_parent(X, Y).

impossible_mother(X, Y) :- male(X) ; already_has_mother(Y) ; impossible_parent(X, Y).

impossible_grandmother(X, Y) :- male(X) ; X = Y ; already_has_grandmothers(Y) ; unrelated_grandparent(X, Y) ; parent(X, Y) 
; child(X, Y) ; sibling(X, Y) ; grandmother(Y, X) ; aunt(X, Y) ; aunt(Y, X).

impossible_grandfather(X, Y) :- female(X) ; X = Y ; already_has_grandfathers(Y) ; unrelated_grandparent(X, Y) ; parent(X, Y) 
; child(X, Y) ; sibling(X, Y) ; grandfather(Y, X) ; uncle(X, Y) ; uncle(Y, X).

impossible_child(X, Y) :- X = Y ; already_has_parents(X) ; sibling_has_different_parents(Y, X) ; parent(X, Y) ; sibling(X, Y) 
; grandfather(X, Y) ; grandfather(Y, X) ; grandmother(X, Y) ; grandmother(Y, X) ; uncle(X, Y) ; uncle(Y, X) 
; aunt(X, Y) ; aunt(Y, X).

impossible_son(X, Y) :- female(X) ; impossible_child(X, Y).

impossible_daughter(X, Y) :- male(X) ; impossible_child(X, Y).

impossible_uncle(X, Y) :- female(X) ; X = Y ; unrelated_uncle(X, Y) ; parent(X, Y) ; sibling(X, Y) 
; grandfather(X, Y) ; grandfather(Y, X) ; uncle(Y, X).

impossible_aunt(X, Y) :- male(X) ; X = Y ; unrelated_aunt(X, Y) ; parent(X, Y) ; sibling(X, Y) 
; grandmother(X, Y) ; grandmother(Y, X) ; aunt(Y, X).


% helper definitions
different_parents(X, Y) :-
(   % X and Y can not be siblings if they do not share a single parent
    parent(Parent1X, X), parent(Parent2X, X), Parent1X \= Parent2X, 
    parent(Parent1Y, Y), parent(Parent2Y, Y), Parent1Y \= Parent2Y, 
    Parent1X \= Parent1Y, Parent1X \= Parent2Y, Parent2X \= Parent1Y, Parent2X \= Parent2Y
).

already_has_parents(X) :-
(   % X can not have a parent anymore if X already has two different parents
    parent(Parent1X, X), parent(Parent2X, X), Parent1X \= Parent2X
).

already_has_parents(X, Y) :-
(   % X can not have a parent anymore if X already has two different parents and Y is not one of them
    parent(Parent1X, X), parent(Parent2X, X), Parent1X \= Parent2X, Y \= Parent1X, Y \= Parent2X
).

already_has_parents(X, Y, Z) :-
(   % X can not have two new parents if X already has one different parent
    parent(ParentX, X), Y \= ParentX, Z \= ParentX
).

sibling_has_different_parents(X, Y) :- 
(   % X can not be a parent of Y if Y has one defined parent that is not X and Y has a sibling with two different parents
    parent(ParentY, Y), sibling(Y, Z), parent(Parent1Z, Z), parent(Parent2Z, Z), 
    Parent1Z \= Parent2Z, ParentY \= Parent1Z, ParentY \= Parent2Z, X \= ParentY, X \= Parent1Z, X \= Parent2Z
).

already_has_father(X) :-
(   % X can not have a father anymore if X already has a father
    father(_, X)
).

already_has_mother(X) :-
(   % X can not have a mother anymore if X already has a mother
    mother(_, X)
).

already_has_grandmothers(X) :-
(   % X can not have a grandmother anymore if X already has two different grandmothers
    grandmother(Grandmother1X, X), grandmother(Grandmother2X, X), Grandmother1X \= Grandmother2X
).

already_has_grandfathers(X) :-
(   % X can not have a grandfather anymore if X already has two different grandfathers
    grandfather(Grandfather1X, X), grandfather(Grandfather2X, X), Grandfather1X \= Grandfather2X
).

unrelated_grandparent(X, Y) :-
(   % X can not be a grandfather or grandmother of Y if any of the parents of Y are not children of X
    parent(Parent1Y, Y), parent(Parent2Y, Y), Parent1Y \= Parent2Y, (\+ child(Parent1Y, X) ; \+ child(Parent2Y, X))
).

unrelated_uncle(X, Y) :-
(   % X can not be an uncle of Y if X is not a brother to any of the parents of Y
    parent(Parent1Y, Y), parent(Parent2Y, Y), Parent1Y \= Parent2Y, (\+ brother(X, Parent1Y) ; \+ brother(X, Parent2Y))
).

unrelated_aunt(X, Y) :-
(   % X can not be an aunt of Y if X is not a sister to any of the parents of Y
    parent(Parent1Y, Y), parent(Parent2Y, Y), Parent1Y \= Parent2Y, (\+ sister(X, Parent1Y) ; \+ sister(X, Parent2Y))
).