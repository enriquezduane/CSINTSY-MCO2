% defines the module and export the predicates to be used by other files.
:- module(questions, [query_child/2, query_children_of/1]).

:- use_module(logic).

query_child(Child, Parent) :-
  (   parent(Parent, Child)
  ->  writeln('Yes, that is correct.')
  ;   writeln('No, that is not correct.')
  ).

query_children_of(Parent) :-
  findall(Child, child(Child, Parent), Children),
  print_children(Children).

print_children([]) :-
  writeln('There are no children.').
print_children(Children) :-
  writeln('The children are:'),
  print_list(Children).

print_list([]).
print_list([Head|Tail]) :-
  format('- ~w~n', [Head]),
  print_list(Tail).

