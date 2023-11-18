% load modules
:- consult(utils/logic).
:- use_module(utils/statements).
:- use_module(utils/questions).


% entry point
start :-
  writeln('Welcome to the Prolog Family Tree!'),
  writeln('Type "quit" to exit.'),
  main_loop,
  writeln('Goodbye!').


% main loop for command prompt
main_loop :-
  repeat,
  write('> '),
  read_line_to_string(user_input, Input),
  process_input(Input),
  Input == "quit".


% process user input
process_input(Input) :-
  parse_input(Input, Parsed), 
  execute_command(Parsed).


% parses input into a list of words
parse_input(Input, List) :-
  split_string(Input, " ?.,", "", List).

% query commands are placed before statement commands because of the way Prolog searches predicates
execute_command(["Are", Sibling1, "and", Sibling2, "siblings", ""]) :-
  query_sibling(Sibling1, Sibling2), !.

execute_command(["are", Sibling1, "and", Sibling2, "siblings", ""]) :-
  query_sibling(Sibling1, Sibling2), !.


execute_command(["Who", "are", "the", "siblings", "of", Sibling, ""]) :-
  query_sibling(Sibling), !.

execute_command(["who", "are", "the", "siblings", "of", Sibling, ""]) :-
  query_sibling(Sibling), !.


execute_command(["Is", Sister, "a", "sister", "of", Sibling, ""]) :-
  query_sister(Sister, Sibling), !.

execute_command(["is", Sister, "a", "sister", "of", Sibling, ""]) :-
  query_sister(Sister, Sibling), !.


execute_command(["Who", "are", "the", "sisters", "of", Sibling, ""]) :-
  query_sister(Sibling), !.

execute_command(["who", "are", "the", "sisters", "of", Sibling, ""]) :-
  query_sister(Sibling), !.


execute_command(["Is", Brother, "a", "brother", "of", Sibling, ""]) :-
  query_brother(Brother, Sibling), !.

execute_command(["is", Brother, "a", "brother", "of", Sibling, ""]) :-
  query_brother(Brother, Sibling), !.


execute_command(["Who", "are", "the", "brothers", "of", Sibling, ""]) :-
  query_brother(Sibling), !.

execute_command(["who", "are", "the", "brothers", "of", Sibling, ""]) :-
  query_brother(Sibling), !.


execute_command(["Is", Mother, "the", "mother", "of", Child, ""]) :-
  query_mother(Mother, Child), !.

execute_command(["is", Mother, "the", "mother", "of", Child, ""]) :-
  query_mother(Mother, Child), !.


execute_command(["Who", "is", "the", "mother", "of", Child, ""]) :-
  query_mother(Child), !.

execute_command(["who", "is", "the", "mother", "of", Child, ""]) :-
  query_mother(Child), !.


execute_command(["Is", Father, "the", "father", "of", Child, ""]) :-
  query_father(Father, Child), !.

execute_command(["is", Father, "the", "father", "of", Child, ""]) :-
  query_father(Father, Child), !.


execute_command(["Who", "is", "the", "father", "of", Child, ""]) :-
  query_father(Child), !.  

execute_command(["who", "is", "the", "father", "of", Child, ""]) :-
  query_father(Child), !.


execute_command(["Are", Parent1, "and", Parent2, "the", "parents", "of", Child, ""]) :-
  query_parents(Parent1, Parent2, Child), !. 

execute_command(["are", Parent1, "and", Parent2, "the", "parents", "of", Child, ""]) :-
  query_parents(Parent1, Parent2, Child), !.  


execute_command(["Who", "are", "the", "parents", "of", Child, ""]) :-
  query_parents(Child), !.  

execute_command(["who", "are", "the", "parents", "of", Child, ""]) :-
  query_parents(Child), !.


execute_command(["Is", Grandmother, "a", "grandmother", "of", Grandchild, ""]) :-
  query_grandmother(Grandmother, Grandchild), !.

execute_command(["is", Grandmother, "a", "grandmother", "of", Grandchild, ""]) :-
  query_grandmother(Grandmother, Grandchild), !.


execute_command(["Is", Grandfather, "a", "grandfather", "of", Grandchild, ""]) :-
  query_grandfather(Grandfather, Grandchild), !.

execute_command(["is", Grandfather, "a", "grandfather", "of", Grandchild, ""]) :-
  query_grandfather(Grandfather, Grandchild), !.


execute_command(["Is", Daughter, "a", "daughter", "of", Parent, ""]) :-
  query_daughter(Daughter, Parent), !.

execute_command(["is", Daughter, "a", "daughter", "of", Parent, ""]) :-
  query_daughter(Daughter, Parent), !.


execute_command(["Who", "are", "the", "daughters", "of", Parent, ""]) :-
  query_daughter(Parent), !.

execute_command(["who", "are", "the", "daughters", "of", Parent, ""]) :-
  query_daughter(Parent), !.


execute_command(["Is", Son, "a", "son", "of", Parent, ""]) :-
  query_son(Son, Parent), !.

execute_command(["is", Son, "a", "son", "of", Parent, ""]) :-
  query_son(Son, Parent), !.


execute_command(["Who", "are", "the", "sons", "of", Parent, ""]) :-
  query_son(Parent), !.

execute_command(["who", "are", "the", "sons", "of", Parent, ""]) :-
  query_son(Parent), !.


execute_command(["Is", Child, "a", "child", "of", Parent, ""]) :-
  query_child(Child, Parent), !.

execute_command(["is", Child, "a", "child", "of", Parent, ""]) :-
  query_child(Child, Parent), !.


execute_command(["Who", "are", "the", "children", "of", Parent, ""]) :-
  query_child(Parent), !.

execute_command(["who", "are", "the", "children", "of", Parent, ""]) :-
  query_child(Parent), !.


execute_command(["Are", Child1, "", Child2, "", "and", Child3, "children", "of", Parent, ""]) :-
  query_child(Child1, Child2, Child3, Parent), !.

execute_command(["are", Child1, "", Child2, "", "and", Child3, "children", "of", Parent, ""]) :-
  query_child(Child1, Child2, Child3, Parent), !.


execute_command(["Is", Aunt, "an", "aunt", "of", NieceNephew, ""]) :-
  query_aunt(Aunt, NieceNephew), !.

execute_command(["is", Aunt, "an", "aunt", "of", NieceNephew, ""]) :-
  query_aunt(Aunt, NieceNephew), !.


execute_command(["Is", Uncle, "an", "uncle", "of", NieceNephew, ""]) :-
  query_uncle(Uncle, NieceNephew), !.

execute_command(["is", Uncle, "an", "uncle", "of", NieceNephew, ""]) :-
  query_uncle(Uncle, NieceNephew), !.


execute_command(["Are", Relative1, "and", Relative2, "relatives", ""]) :-
  query_relative(Relative1, Relative2), !.

execute_command(["are", Relative1, "and", Relative2, "relatives", ""]) :-
  query_relative(Relative1, Relative2), !.



% statement commands with 2 instances of the commannd for periods and no periods
execute_command([Sibling1, "and", Sibling2, "are", "siblings"]) :-
  add_sibling(Sibling1, Sibling2), !.

execute_command([Sibling1, "and", Sibling2, "are", "siblings", ""]) :-
  add_sibling(Sibling1, Sibling2), !.


execute_command([Brother, "is", "a", "brother", "of", Sibling]) :-
  add_brother(Brother, Sibling), !.

execute_command([Brother, "is", "a", "brother", "of", Sibling, ""]) :-
  add_brother(Brother, Sibling), !.


execute_command([Sister, "is", "a", "sister", "of", Sibling]) :-
  add_sister(Sister, Sibling), !.

execute_command([Sister, "is", "a", "sister", "of", Sibling, ""]) :-
  add_sister(Sister, Sibling), !.


execute_command([Father, "is", "the", "father", "of", Child]) :-
  add_father(Father, Child), !.

execute_command([Father, "is", "the", "father", "of", Child, ""]) :-
  add_father(Father, Child), !.


execute_command([Mother, "is", "the", "mother", "of", Child]) :-
  add_mother(Mother, Child), !.

execute_command([Mother, "is", "the", "mother", "of", Child, ""]) :-
  add_mother(Mother, Child), !.


execute_command([Parent1, "and", Parent2, "are", "the", "parents", "of", Child]) :-
  add_parents(Parent1, Parent2, Child), !.

execute_command([Parent1, "and", Parent2, "are", "the", "parents", "of", Child, ""]) :-
  add_parents(Parent1, Parent2, Child), !. 


execute_command([Grandmother, "is", "a", "grandmother", "of", Grandchild]) :-
  add_grandmother(Grandmother, Grandchild), !.

execute_command([Grandmother, "is", "a", "grandmother", "of", Grandchild, ""]) :-
  add_grandmother(Grandmother, Grandchild), !.


execute_command([Grandfather, "is", "a", "grandfather", "of", Grandchild]) :-
  add_grandfather(Grandfather, Grandchild), !.

execute_command([Grandfather, "is", "a", "grandfather", "of", Grandchild, ""]) :-
  add_grandfather(Grandfather, Grandchild), !.


execute_command([Child, "is", "a", "child", "of", Parent]) :-
  add_child(Child, Parent), !.

execute_command([Child, "is", "a", "child", "of", Parent, ""]) :-
  add_child(Child, Parent), !.


execute_command([First|Rest]) :-
  append(Children, ["are", "children", "of", Parent], [First|Rest]),
  add_children(Children, Parent, _), !.

execute_command([First|Rest]) :-
  append(Children, ["are", "children", "of", Parent, ""], [First|Rest]),
  add_children(Children, Parent, _), !.


execute_command([Daughter, "is", "a", "daughter", "of", Parent]) :-
  add_daughter(Daughter, Parent), !.

execute_command([Daughter, "is", "a", "daughter", "of", Parent, ""]) :-
  add_daughter(Daughter, Parent), !.


execute_command([Son, "is", "a", "son", "of", Parent]) :-
  add_son(Son, Parent), !.

execute_command([Son, "is", "a", "son", "of", Parent, ""]) :-
  add_son(Son, Parent), !.


execute_command([Uncle, "is", "an", "uncle", "of", NieceNephew]) :-
  add_uncle(Uncle, NieceNephew), !.

execute_command([Uncle, "is", "an", "uncle", "of", NieceNephew, ""]) :-
  add_uncle(Uncle, NieceNephew), !.


execute_command([Aunt, "is", "an", "aunt", "of", NieceNephew]) :-
  add_aunt(Aunt, NieceNephew), !.

execute_command([Aunt, "is", "an", "aunt", "of", NieceNephew, ""]) :-
  add_aunt(Aunt, NieceNephew), !.


% quit command
execute_command(["quit"]) :-
  true, !.


% if it could not identify the chat pattern, this will be executed
execute_command(_) :-
  writeln('I do not understand.').


% run the chatbot
:- start.

