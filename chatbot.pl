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
  split_string(Input, " ", "", List).

% query commands
execute_command(["are", Sibling1, "and", Sibling2, "siblings?"]) :-
  query_sibling(Sibling1, Sibling2), !.

execute_command(["who", "are", "the", "siblings", "of", Sibling]) :-
  query_siblings(Sibling), !.

execute_command(["is", Sister, "a", "sister", "of", Sibling]) :-
  query_sister(Sister, Sibling), !.

execute_command(["who", "are", "the", "sisters", "of", Sibling]) :-
  query_sisters(Sibling), !.

execute_command(["is", Brother, "a", "brother", "of", Sibling]) :-
  query_brother(Brother, Sibling), !.

execute_command(["who", "are", "the", "brothers", "of", Sibling]) :-
  query_brothers(Sibling), !.

execute_command(["is", Mother, "the", "mother", "of", Child]) :-
  query_isMother(Mother, Child), !.

execute_command(["who", "is", "the", "mother", "of", Child]) :-
  query_whoMother(Child), !.

execute_command(["is", Father, "the", "father", "of", Child]) :-
  query_isFather(Father, Child), !.

execute_command(["who", "is", "the", "father", "of", Child]) :-
  query_whoFather(Child), !.

execute_command(["is", Child, "a", "child", "of", Parent]) :-
  query_child(Child, Parent), !.

execute_command(["who", "are", "the", "children", "of", Parent]) :-
  query_children(Parent), !.
  

% statement commands
execute_command([Sibling1, "and", Sibling2, "are", "siblings"]) :-
  add_sibling(Sibling1, Sibling2), !.

execute_command([Brother, "is", "a", "brother", "of", Sibling]) :-
  add_brother(Brother, Sibling), !.

execute_command([Sister, "is", "a", "sister", "of", Sibling]) :-
  add_sister(Sister, Sibling), !.

execute_command([Father, "is", "the", "father", "of", Child]) :-
  add_father(Father, Child), !.

execute_command([Mother, "is", "the", "mother", "of", Child]) :-
  add_mother(Mother, Child), !.

execute_command([Parent1, "and", Parent2, "are", "the", "parents", "of", Child]) :-
  add_parents(Parent1, Parent2, Child), !.

execute_command([Grandmother, "is", "a", "grandmother", "of", Grandchild]) :-
  add_grandmother(Grandmother, Grandchild), !.

execute_command([Grandfather, "is", "a", "grandfather", "of", Grandchild]) :-
  add_grandfather(Grandfather, Grandchild), !.

execute_command([Child, "is", "a", "child", "of", Parent]) :-
  add_child(Child, Parent), !.

% add add_children command here


execute_command([Daughter, "is", "a", "daughter", "of", Parent]) :-
  add_child(Daughter, Parent), !.

execute_command([Son, "is", "a", "son", "of", Parent]) :-
  add_child(Son, Parent), !.

execute_command([Uncle, "is", "an", "uncle", "of", NieceNephew]) :-
  add_uncle(Uncle, NieceNephew), !.

execute_command([Aunt, "is", "an", "aunt", "of", NieceNephew]) :-
  add_aunt(Aunt, NieceNephew), !.

% query commands
execute_command(["are", Sibling1, "and", Sibling2, "siblings?"]) :-
  query_sibling(Sibling1, Sibling2), !.

execute_command(["who", "are", "the", "siblings", "of", Sibling]) :-
  query_siblings(Sibling), !.

execute_command(["is", Sister, "a", "sister", "of", Sibling]) :-
  query_sister(Sister, Sibling), !.

execute_command(["who", "are", "the", "sisters", "of", Sibling]) :-
  query_sisters(Sibling), !.

execute_command(["is", Brother, "a", "brother", "of", Sibling]) :-
  query_brother(Brother, Sibling), !.

execute_command(["who", "are", "the", "brothers", "of", Sibling]) :-
  query_brothers(Sibling), !.

execute_command(["is", Mother, "the", "mother", "of", Child]) :-
  query_isMother(Mother, Child), !.

execute_command(["who", "is", "the", "mother", "of", Child]) :-
  query_whoMother(Child), !.

execute_command(["is", Father, "the", "father", "of", Child]) :-
  query_isFather(Father, Child), !.

execute_command(["who", "is", "the", "father", "of", Child]) :-
  query_whoFather(Child), !.

execute_command(["is", Child, "a", "child", "of", Parent]) :-
  query_child(Child, Parent), !.

execute_command(["who", "are", "the", "children", "of", Parent]) :-
  query_children(Parent), !.


% quit command
execute_command(["quit"]) :-
  true, !.

% if it couldn't identify the chat pattern, this will be executed
execute_command(_) :-
  writeln('I do not understand').

% run the chatbot
:- start.

