% defines the module and export the predicates to be used by other files.
:- module(statements, [add_parent/2]).

add_parent(Parent, Child) :-
  (   parent(Parent, Child)
  ->  writeln('That relationship already exists.')
  ;   assertz(parent(Parent, Child)), writeln('New relationship added.')
  ).

