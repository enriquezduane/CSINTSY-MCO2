% defines the module and export the predicates to be used by other files.
:- module(questions, [query_sibling/2, query_sibling/1, query_sister/2, query_sister/1, query_brother/2, query_brother/1, 
query_mother/2, query_mother/1, query_father/2, query_father/1, query_parents/3, query_parents/1,
query_grandfather/2, query_grandmother/2, query_daughter/2, query_daughter/1, query_son/2, query_son/1, 
query_child/2, query_child/1, query_child/4, query_uncle/2, query_aunt/2, query_relative/2]).

:- use_module(logic).

% query definitions
query_sibling(Sibling1, Sibling2) :-
  (   sibling(Sibling1, Sibling2)
  ->  writeln('Yes, that is correct.')
  ;   writeln('No, that is not correct.')
  ).

query_sibling(Sibling) :-
  findall(Siblings, sibling(Siblings, Sibling), Siblings),
  print_siblings(Siblings).

query_sister(Sister, Sibling) :-
  (   sister(Sister, Sibling)
  ->  writeln('Yes, that is correct.')
  ;   writeln('No, that is not correct.')
  ).

query_sister(Sibling) :-
  findall(Sister, sister(Sister, Sibling), Sisters),
  print_sisters(Sisters).
  
query_brother(Brother, Sibling) :-
  (   brother(Brother, Sibling)
  ->  writeln('Yes, that is correct.')
  ;   writeln('No, that is not correct.')
  ).

query_brother(Sibling) :-
  findall(Brother, brother(Brother, Sibling), Brothers),
  print_brothers(Brothers).

query_mother(Mother, Child) :-
  (   mother(Mother, Child)
  ->  writeln('Yes, that is correct.')
  ;   writeln('No, that is not correct.')
  ).

query_mother(Child) :-
  (   mother(Mother, Child)
  ->  write('The mother of '), write(Child), write(' is '), write(Mother), writeln('.')
  ;   write(Child), writeln(' does not have a mother.')
  ).

query_father(Father, Child) :-
  (   father(Father, Child)
  ->  writeln('Yes, that is correct.')
  ;   writeln('No, that is not correct.')
  ).

query_father(Child) :-
  (   father(Father, Child)
  ->  write('The father of '), write(Child), write(' is '), write(Father), writeln('.')
  ;   write(Child), writeln(' does not have a father.')
  ).

query_parents(Parent1, Parent2, Child) :-
  (   parent(Parent1, Child), parent(Parent2, Child), Parent1 \= Parent2
  ->  writeln('Yes, that is correct.')
  ;   writeln('No, that is not correct.')
  ).

query_parents(Child) :-
  (   parent(Parent1, Child), parent(Parent2, Child), Parent1 \= Parent2
  ->  write('The parents of '), write(Child), write(' are '), write(Parent1), write(' and '), write(Parent2), writeln('.')
  ;   write(Child), writeln(' does not have parents.')
  ).

query_grandmother(Grandmother, Grandchild) :-
  (   grandmother(Grandmother, Grandchild)
  ->  writeln('Yes, that is correct.')
  ;   writeln('No, that is not correct.')
  ).

query_grandfather(Grandfather, Grandchild) :-
  (   grandfather(Grandfather, Grandchild)
  ->  writeln('Yes, that is correct.')
  ;   writeln('No, that is not correct.')
  ).

query_daughter(Daughter, Parent) :-
  (   daughter(Daughter, Parent)
  ->  writeln('Yes, that is correct.')
  ;   writeln('No, that is not correct.')
  ).

query_daughter(Parent) :-
  findall(Daughter, daughter(Daughter, Parent), Daughters),
  print_daughters(Daughters).

query_son(Son, Parent) :-
  (   son(Son, Parent)
  ->  writeln('Yes, that is correct.')
  ;   writeln('No, that is not correct.')
  ).

query_son(Parent) :-
  findall(Son, son(Son, Parent), Sons),
  print_sons(Sons).

query_child(Child, Parent) :-
  (   parent(Parent, Child)
  ->  writeln('Yes, that is correct.')
  ;   writeln('No, that is not correct.')
  ).

query_child(Parent) :-
  findall(Child, child(Child, Parent), Children),
  print_children(Children).

query_child(Child1, Child2, Child3, Parent) :-
  (   parent(Parent, Child1), parent(Parent, Child2), parent(Parent, Child3), Child1 \= Child2, Child1 \= Child3, Child2 \= Child3
  ->  writeln('Yes, that is correct.')
  ;   writeln('No, that is not correct.')
  ).

query_uncle(Uncle, NieceNephew) :-
  (   uncle(Uncle, NieceNephew)
  ->  writeln('Yes, that is correct.')
  ;   writeln('No, that is not correct.')
  ).

query_aunt(Aunt, NieceNephew) :-
  (   aunt(Aunt, NieceNephew)
  ->  writeln('Yes, that is correct.')
  ;   writeln('No, that is not correct.')
  ).

query_relative(Relative1, Relative2) :-
  (   relative(Relative1, Relative2)
  ->  writeln('Yes, that is correct.')
  ;   writeln('No, that is not correct.')
  ).


% helper definitions
print_siblings([]) :- 
  writeln('There are no siblings.').
print_siblings(Siblings) :-
  writeln('The siblings are:'),
  print_list(Siblings).

print_brothers([]) :-
  writeln('There are no brothers.').
print_brothers(Brothers) :-
  writeln('The brothers are:'),
  print_list(Brothers).

print_sisters([]) :-
  writeln('There are no sisters.').
print_sisters(Sisters) :-
  writeln('The sisters are:'),
  print_list(Sisters).

print_daughters([]) :-
  writeln('There are no daughters.').
print_daughters(Daughters) :-
  writeln('The daughters are:'),
  print_list(Daughters).

print_sons([]) :-
  writeln('There are no sons.').
print_sons(Sons) :-
  writeln('The sons are:'),
  print_list(Sons).

print_children([]) :-
  writeln('There are no children.').
print_children(Children) :-
  writeln('The children are:'),
  print_list(Children).

print_list([]).
print_list([Head|Tail]) :-
  format('- ~w~n', [Head]),
  print_list(Tail).

