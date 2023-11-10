% defines the module and export the predicates to be used by other files.
:- module(statements, [ add_sibling/2, add_brother/2, add_sister/2, add_father/2,
add_mother/2, add_parents/3,add_grandmother/2, add_grandfather/2, add_child/2, add_daughter/2, add_son/2, add_uncle/2, add_aunt/2]).

% statement definitions (Fix their logic by replacing the relationship condition used)

add_sibling(Sibling1, Sibling2) :-
  (   sibling(Sibling1, Sibling2)
  ->  writeln('That relationship already exists.')
  ;   assertz(sibling(Sibling1, Sibling2)), 
      assertz(sibling(Sibling2, Sibling1)), 
      writeln('New relationship added.')
  ).

add_brother(Brother, Sibling) :-
  (   brother(Brother, Sibling)
  ->  writeln('That relationship already exists.')
  ;   \+ male(Brother) -> assertz(male(Brother)), 
      \+ sibling(Brother, Sibling) -> assertz(sibling(Brother, Sibling)), 
      \+ sibling(Sibling, Brother) -> assertz(sibling(Sibling, Brother)), 
      assertz(brother(Brother, Sibling)), 
      writeln('New relationship added.')
  ).

add_sister(Sister, Sibling) :-
  (   sister(Sister, Sibling)
  ->  writeln('That relationship already exists.')
  ;   \+ female(Sister) -> assertz(female(Sister)), 
      \+ sibling(Sister, Sibling) -> assertz(sibling(Sister, Sibling)), 
      \+ sibling(Sibling, Sister) -> assertz(sibling(Sibling, Sister)), 
      assertz(sister(Sister, Sibling)), 
      writeln('New relationship added.')
  ).

add_father(Father, Child) :-
  (   father(Father, Child)
  ->  writeln('That relationship already exists.')
  ;   \+ male(Father) -> assertz(male(Father)), 
      \+ parent(Father, Child) -> assertz(parent(Father, Child)), 
      assertz(father(Father, Child)), 
      writeln('New relationship added.')
  ).

add_mother(Mother, Child) :-
  (   mother(Mother, Child)
  ->  writeln('That relationship already exists.')
  ;   \+ female(Mother) -> assertz(female(Mother)), 
      \+ parent(Mother, Child) -> assertz(parent(Mother, Child)), 
      assertz(mother(Mother, Child)), 
      writeln('New relationship added.')
  ).

add_parents(Parent1, Parent2, Child) :-
  (   parent(Parent1, Child), parent(Parent2, Child)
  ->  writeln('That relationship already exists.')
  ;   assertz(parent(Parent1, Child)), 
      assertz(parent(Parent2, Child)), 
      writeln('New relationship added.')
  ).

add_grandmother(Grandmother, Grandchild) :-
  (   grandmother(Grandmother, Grandchild)
  ->  writeln('That relationship already exists.')
  ;   \+ female(Grandmother) -> assertz(female(Grandmother)), 
      assertz(grandmother(Grandmother, Grandchild)), 
      writeln('New relationship added.')
  ).

add_grandfather(Grandfather, Grandchild) :-
  (   grandfather(Grandfather, Grandchild)
  ->  writeln('That relationship already exists.')
  ;   \+ male(Grandfather) -> assertz(male(Grandfather)), 
      assertz(grandfather(Grandfather, Grandchild)), 
      writeln('New relationship added.')
  ).

add_daughter(Daughter, Parent) :-
  (   daughter(Daughter, Parent)
  ->  writeln('That relationship already exists.')
  ;   \+ female(Daughter) -> assertz(female(Daughter)), 
      \+ parent(Parent, Daughter) -> assertz(parent(Parent, Daughter)),
      assertz(daughter(Daughter, Parent)), 
      writeln('New relationship added.')
  ).

add_son(Son, Parent) :-
  (   son(Son, Parent)
  ->  writeln('That relationship already exists.')
  ;   \+ male(Son) -> assertz(male(Son)), 
      \+ parent(Parent, Son) -> assertz(parent(Parent, Son)), 
      assertz(son(Son, Parent)),
      writeln('New relationship added.')
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
  ;   \+ male(Uncle) -> assertz(male(Uncle)), 
      assertz(uncle(Uncle, NieceNephew)), 
      writeln('New relationship added.')
  ).

add_aunt(Aunt, NieceNephew) :-
  (   aunt(Aunt, NieceNephew)
  ->  writeln('That relationship already exists.')
  ;   \+ female(Aunt) -> assertz(female(Aunt)), 
      assertz(aunt(Aunt, NieceNephew)), 
      writeln('New relationship added.')
  ).