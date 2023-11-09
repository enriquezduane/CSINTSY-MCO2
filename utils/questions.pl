% defines the module and export the predicates to be used by other files.
:- module(questions, [query_child/2, query_children/1, query_sibling/2, query_siblings/1, query_brother/2, query_brothers/1, 
query_sister/2, query_sisters/1, query_isFather/2, query_whoFather/1, query_isMother/2, query_whoMother/1, 
query_grandfather/2, query_grandmother/2, query_uncle/2, query_aunt/2]).

:- use_module(logic).

% query definitions
query_child(Child, Parent) :-
  (   parent(Parent, Child)
  ->  writeln('Yes, that is correct.')
  ;   writeln('No, that is not correct.')
  ).

query_children(Parent) :-
  findall(Child, child(Child, Parent), Children),
  print_children(Children).

query_sibling(Sibling1, Sibling2) :-
  (   sibling(Sibling1, Sibling2)
  ->  writeln('Yes, that is correct.')
  ;   writeln('No, that is not correct.')
  ).

query_siblings(Sibling) :-
  findall(Sibling2, sibling(Sibling, Sibling2), Siblings),
  print_siblings(Siblings).

query_brother(Brother, Sibling) :-
  (   brother(Brother, Sibling)
  ->  writeln('Yes, that is correct.')
  ;   writeln('No, that is not correct.')
  ).

query_brothers(Sibling) :-
  findall(Brother, brother(Brother, Sibling), Brothers),
  print_siblings(Brothers).

query_sister(Sister, Sibling) :-
  (   sister(Sister, Sibling)
  ->  writeln('Yes, that is correct.')
  ;   writeln('No, that is not correct.')
  ).

query_sisters(Sibling) :-
  findall(Sister, sister(Sister, Sibling), Sisters),
  print_siblings(Sisters).

query_isFather(Father, Child) :-
  (   father(Father, Child)
  ->  writeln('Yes, that is correct.')
  ;   writeln('No, that is not correct.')
  ).

query_whoFather(Child) :-
  (   father(Father, Child)
  ->  writeln('The father of '), write(Child), writeln(' is '), write(Father), writeln('.')
  ;   write(Child), writeln(' does not have a father.')
  ).

query_isMother(Mother, Child) :-
  (   mother(Mother, Child)
  ->  writeln('Yes, that is correct.')
  ;   writeln('No, that is not correct.')
  ).

query_grandfather(Grandfather, Grandchild) :-
  (   grandfather(Grandfather, Grandchild)
  ->  writeln('Yes, that is correct.')
  ;   writeln('No, that is not correct.')
  ).

query_grandmother(Grandmother, Grandchild) :-
  (   grandmother(Grandmother, Grandchild)
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


% helper definitions
print_children([]) :-
  writeln('There are no children.').
print_children(Children) :-
  writeln('The children are:'),
  print_list(Children).

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

print_list([]).
print_list([Head|Tail]) :-
  format('- ~w~n', [Head]),
  print_list(Tail).

