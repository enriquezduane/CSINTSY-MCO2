% defines the module and export the predicates to be used by other files.
:- module(statements, [ add_sibling/2, add_brother/2, add_sister/2, add_father/2,
add_mother/2, add_parents/3, add_grandmother/2, add_grandfather/2, add_child/2, add_children/3, add_children/2, 
add_daughter/2, add_son/2, add_uncle/2, add_aunt/2]).

:- dynamic checked /1.


% statement definitions (Fix their logic by replacing the relationship condition used)
add_sibling(Sibling1, Sibling2) :-
  (   sibling(Sibling1, Sibling2)
  ->  writeln('That relationship already exists.')
  ;   impossible_sibling(Sibling1, Sibling2)
  ->  writeln('That is impossible!')
  ;   assertz(sibling(Sibling1, Sibling2)),
      ( \+ sibling(Sibling2, Sibling1)
      -> assertz(sibling(Sibling2, Sibling1))
      ;  true
      ),
      writeln('OK! I learned something.')
  ).

add_brother(Brother, Sibling) :-
  (   brother(Brother, Sibling)
  ->  writeln('That relationship already exists.')
  ;   impossible_brother(Brother, Sibling)
  ->  writeln('That is impossible!')
  ;   ( \+ male(Brother) 
      -> assertz(male(Brother))
      ;  true
      ),
      ( \+ sibling(Brother, Sibling) 
      -> assertz(sibling(Brother, Sibling))
      ;  true
      ),
      ( \+ sibling(Sibling, Brother) 
      -> assertz(sibling(Sibling, Brother))
      ;  true
      ),
      retractall(checked(_)),
      (define_genders_from_neutral(Brother) ; true),
      writeln('OK! I learned something.')
  ).

add_sister(Sister, Sibling) :-
  (   sister(Sister, Sibling)
  ->  writeln('That relationship already exists.')
  ;   impossible_sister(Sister, Sibling)
  ->  writeln('That is impossible!')
  ;   ( \+ female(Sister) 
      -> assertz(female(Sister))
      ;  true
      ),
      ( \+ sibling(Sister, Sibling) 
      -> assertz(sibling(Sister, Sibling))
      ;  true
      ),
      ( \+ sibling(Sibling, Sister) 
      -> assertz(sibling(Sibling, Sister))
      ;  true
      ),
      retractall(checked(_)),
      (define_genders_from_neutral(Sister) ; true),
      writeln('OK! I learned something.')
  ).

add_father(Father, Child) :-
  (   father(Father, Child)
  ->  writeln('That relationship already exists.')
  ;   impossible_father(Father, Child)
  ->  writeln('That is impossible!')
  ;   ( \+ male(Father) 
      -> assertz(male(Father))
      ;  true
      ),
      ( \+ parent(Father, Child) 
      -> assertz(parent(Father, Child))
      ;  true
      ),
      (  sibling(Child, Sibling), mother(Mother1, Child), mother(Mother2, Sibling), Mother1 \= Mother2
      ->  ( \+ parent(Father, Sibling)
          -> assertz(parent(Father, Sibling))
          ;  true
          )
      ;   true
      ), 
      (   sibling(Child, Sibling), father(Father2, Sibling), mother(Mother, Sibling), Father \= Father2
      ->  ( \+ parent(Mother, Child)
          -> assertz(parent(Mother, Child))
          ;  true
          )
      ;   true
      ),
      ( \+ male(Father) 
      -> assertz(male(Father))
      ;  true
      ),
      retractall(checked(_)),
      (define_genders_from_neutral(Father) ; true),
      writeln('OK! I learned something.')
  ).

add_mother(Mother, Child) :-
    (   mother(Mother, Child)
    ->  writeln('That relationship already exists.')
    ;   impossible_mother(Mother, Child)
    ->  writeln('That is impossible!')
    ;   ( \+ female(Mother)
        -> assertz(female(Mother))
        ;  true
        ),
        ( \+ parent(Mother, Child)
        -> assertz(parent(Mother, Child))
        ;  true
        ),
        (   sibling(Child, Sibling), father(Father1, Child), father(Father2, Sibling), Father1 \= Father2
        ->  ( \+ parent(Mother, Sibling)
            -> assertz(parent(Mother, Sibling))
            ;  true
            )
        ;   true
        ), 
        (   sibling(Child, Sibling), mother(Mother2, Sibling), father(Father, Sibling), Mother \= Mother2
        ->  ( \+ parent(Father, Child)
            -> assertz(parent(Father, Child))
            ;  true
            )
        ;   true
        ),
        retractall(checked(_)),
        (define_genders_from_neutral(Mother) ; true),
        writeln('OK! I learned something.')
    ).

add_parents(Parent1, Parent2, Child) :-
  (   parent(Parent1, Child), parent(Parent2, Child)
  ->  writeln('That relationship already exists.')
  ;   impossible_parent(Parent1, Parent2, Child)
  ->  writeln('That is impossible!')
  ;   ( \+ parent(Parent1, Child)
      -> assertz(parent(Parent1, Child))
      ;  true
      ),
      ( \+ parent(Parent2, Child)
      -> assertz(parent(Parent2, Child))
      ;  true
      ),
      retractall(checked(_)),
      (define_genders_from_neutral(Parent1) ; true),
      writeln('OK! I learned something.')
  ).

add_grandmother(Grandmother, Grandchild) :-
  (   grandmother(Grandmother, Grandchild)
  ->  writeln('That relationship already exists.')
  ;   impossible_grandmother(Grandmother, Grandchild)
  ->  writeln('That is impossible!')
  ;   ( \+ female(Grandmother) 
      -> assertz(female(Grandmother))
      ;  true
      ),
      assertz(grandmother(Grandmother, Grandchild)), 
      retractall(checked(_)),
      (define_genders_from_neutral(Grandmother) ; true),
      writeln('OK! I learned something.')
  ).

add_grandfather(Grandfather, Grandchild) :-
  (   grandfather(Grandfather, Grandchild)
  ->  writeln('That relationship already exists.')
  ;   impossible_grandfather(Grandfather, Grandchild)
  ->  writeln('That is impossible!')
  ;   ( \+ male(Grandfather) 
      -> assertz(male(Grandfather))
      ;  true
      ),
      assertz(grandfather(Grandfather, Grandchild)), 
      retractall(checked(_)),
      (define_genders_from_neutral(Grandfather) ; true),
      writeln('OK! I learned something.')
  ).

add_daughter(Daughter, Parent) :-
  (   daughter(Daughter, Parent)
  ->  writeln('That relationship already exists.')
  ;   daughter(Daughter, Parent)
  ->  writeln('That is impossible!')
  ;   ( \+ female(Daughter) 
      -> assertz(female(Daughter))
      ;  true
      ),
      ( \+ parent(Parent, Daughter) 
      -> assertz(parent(Parent, Daughter))
      ;  true
      ),
      retractall(checked(_)),
      (define_genders_from_neutral(Daughter) ; true),
      retractall(checked(_)),
      (define_genders_from_neutral(Parent) ; true),
      writeln('OK! I learned something.')
  ).

add_son(Son, Parent) :-
  (   son(Son, Parent)
  ->  writeln('That relationship already exists.')
  ;   son(Son, Parent)
  ->  writeln('That is impossible!')
  ;   ( \+ male(Son) 
      -> assertz(male(Son))
      ;  true
      ),
      ( \+ parent(Parent, Son) 
      -> assertz(parent(Parent, Son))
      ;  true
      ),
      retractall(checked(_)),
      (define_genders_from_neutral(Son) ; true),
      retractall(checked(_)),
      (define_genders_from_neutral(Parent) ; true),
      writeln('OK! I learned something.')
  ).

add_child(Child, Parent) :-
  (   child(Child, Parent)
  ->  writeln('That relationship already exists.')
  ;   impossible_child(Child, Parent)
  ->  writeln('That is impossible!')
  ;   ( \+ parent(Parent, Child)
      -> assertz(parent(Parent, Child))
      ;  true
      ),
      retractall(checked(_)),
      (define_genders_from_neutral(Parent) ; true),
      writeln('OK! I learned something.')
  ).  

add_children([], Parent, Children) :- 
  member(Child, Children),
  (   child(Child, Parent)
  ->  writeln('That relationship already exists.')
  ;   add_children(Children, Parent),
      writeln('OK! I learned something.')
  ).

add_children([""|Rest], Parent, Children) :- add_children(Rest, Parent, Children).
add_children(["and"|Rest], Parent, Children) :- add_children(Rest, Parent, Children).

add_children([Child|Rest], Parent, ChildrenAcc) :-
  (   impossible_child(Child, Parent)
  ->  writeln('That is impossible!')
  ;   add_children(Rest, Parent, [Child|ChildrenAcc])
  ).

add_children([], _).
add_children([Child|Rest], Parent) :-
  ( 
  \+ parent(Parent, Child)
  -> assertz(parent(Parent, Child))
  ;  true
  ),
  add_children(Rest, Parent).

add_uncle(Uncle, NieceNephew) :-
  (   uncle(Uncle, NieceNephew)
  ->  writeln('That relationship already exists.')
  ;   impossible_uncle(Uncle, NieceNephew)
  ->  writeln('That is impossible!')
  ;   ( \+ male(Uncle) 
      -> assertz(male(Uncle))
      ;  true
      ),
      assertz(uncle(Uncle, NieceNephew)), 
      writeln('OK! I learned something.')
  ).

add_aunt(Aunt, NieceNephew) :-
  (   aunt(Aunt, NieceNephew)
  ->  writeln('That relationship already exists.')
  ;   impossible_aunt(Aunt, NieceNephew)
  ->  writeln('That is impossible!')
  ;   ( \+ female(Aunt) 
      -> assertz(female(Aunt))
      ;  true
      ),
      assertz(aunt(Aunt, NieceNephew)), 
      writeln('OK! I learned something.')
  ).


% helper definitions
define_genders_from_neutral(Parent) :-
    \+ checked(Parent), % Skip if Parent has already been checked
    assertz(checked(Parent)), % Mark Parent as checked
    findall(OtherParent, (parent(Parent, Child), parent(OtherParent, Child)), Parents),
    member(OtherParent, Parents),
    (   male(OtherParent)
    ->  (define_genders(OtherParent) ; false)
    ;   female(OtherParent)
    ->  (define_genders_from_mother(OtherParent) ; false)
    ;   true
    ),
    maplist(define_genders_from_neutral, Parents).

define_genders(Father) :-
    male(Father),
    findall(Mother, (parent(Father, Child), parent(Mother, Child), Mother \= Father, \+ female(Mother), \+ male(Mother)), Mothers),
    maplist(assert_as_female, Mothers),
    maplist(define_genders_from_mother, Mothers).

define_genders_from_mother(Mother) :-
    female(Mother),
    findall(Father, (parent(Mother, Child), parent(Father, Child), Father \= Mother, \+ male(Father), \+ female(Father)), Fathers),
    maplist(assert_as_male, Fathers),
    maplist(define_genders, Fathers).


assert_as_male(Person) :-
    ( \+ male(Person)
    -> assertz(male(Person))
    ;  true
    ).

assert_as_female(Person) :-
    ( \+ female(Person)
    -> assertz(female(Person))
    ;  true
    ).