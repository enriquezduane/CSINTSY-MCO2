% defines the module and export the predicates to be used by other files.
:- module(statements, [ add_sibling/2, add_brother/2, add_sister/2, add_father/2,
add_mother/2, add_parents/3,add_grandmother/2, add_grandfather/2, add_child/2, add_daughter/2, add_son/2, add_uncle/2, add_aunt/2]).

% statement definitions (Fix their logic by replacing the relationship condition used)

add_sibling(Sibling1, Sibling2) :-
  (   sibling(Sibling1, Sibling2)
  ->  writeln('That relationship already exists.')
  ;   assertz(sibling(Sibling1, Sibling2)), assertz(sibling(Sibling2, Sibling1)), writeln('New relationship added.')
  ).

add_brother(Brother, Sibling) :-
  (   brother(Brother, Sibling)
  ->  writeln('That relationship already exists.')
  ;   assertz(male(Brother)), assertz(brother(Brother, Sibling)), writeln('New relationship added.')
  ).

add_sister(Sister, Sibling) :-
  (   sister(Sister, Sibling)
  ->  writeln('That relationship already exists.')
  ;   assertz(female(Sister)), assertz(sister(Sister, Sibling)), writeln('New relationship added.')
  ).

add_father(Father, Child) :-
  (   father(Father, Child)
  ->  writeln('That relationship already exists.')
  ;   assertz(male(Father)), assertz(parent(Father, Child)), assertz(father(Father, Child)), writeln('New relationship added.')
  ).

add_mother(Mother, Child) :-
  (   mother(Mother, Child)
  ->  writeln('That relationship already exists.')
  ;   assertz(female(Mother)), assertz(parent(Mother, Child)), assertz(mother(Mother, Child)), writeln('New relationship added.')
  ).

add_parents(Parent1, Parent2, Child) :-
  (   parent(Parent1, Child), parent(Parent2, Child)
  ->  writeln('That relationship already exists.')
  ;   assertz(parent(Parent1, Child)), assertz(parent(Parent2, Child)), writeln('New relationship added.')
  ).

add_grandmother(Grandmother, Grandchild) :-
  (   grandmother(Grandmother, Grandchild)
  ->  writeln('That relationship already exists.')
  ;   assertz(female(Grandmother)), assertz(grandmother(Grandmother, Grandchild)), writeln('New relationship added.')
  ).

add_grandfather(Grandfather, Grandchild) :-
  (   grandfather(Grandfather, Grandchild)
  ->  writeln('That relationship already exists.')
  ;   assertz(male(Grandfather)), assertz(grandfather(Grandfather, Grandchild)), writeln('New relationship added.')
  ).

add_daughter(Daughter, Parent) :-
  (   daughter(Daughter, Parent)
  ->  writeln('That relationship already exists.')
  ;   assertz(female(Daughter)), assertz(daughter(Daughter, Parent)), assertz(parent(Parent, Daughter)), writeln('New relationship added.')
  ).

add_son(Son, Parent) :-
  (   son(Son, Parent)
  ->  writeln('That relationship already exists.')
  ;   assertz(male(Son)), assertz(son(Son, Parent)), assertz(parent(Parent, Son)), writeln('New relationship added.')
  ).

add_child(Child, Parent) :-
  (   child(Child, Parent)
  ->  writeln('That relationship already exists.')
  ;   assertz(child(Child, Parent)), assertz(parent(Parent, Child)), writeln('New relationship added.')
  ).

% add add_children command here

add_uncle(Uncle, NieceNephew) :-
  (   uncle(Uncle, NieceNephew)
  ->  writeln('That relationship already exists.')
  ;   assertz(male(Uncle)), assertz(uncle(Uncle, NieceNephew)), writeln('New relationship added.')
  ).

add_aunt(Aunt, NieceNephew) :-
  (   aunt(Aunt, NieceNephew)
  ->  writeln('That relationship already exists.')
  ;   assertz(female(Aunt)), assertz(aunt(Aunt, NieceNephew)), writeln('New relationship added.')
  ).

