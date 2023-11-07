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

execute_command([Parent, "is", "a", "parent", "of", Child]) :-
  add_parent(Parent, Child), !.

execute_command(["is", Child, "a", "child", "of", Parent]) :-
  query_child(Child, Parent), !.

execute_command(["who", "are", "the", "children", "of", Parent]) :-
  query_children_of(Parent), !.

execute_command(["quit"]) :-
  true, !.

% if it couldn't identify the chat pattern, this will be executed
execute_command(_) :-
  writeln('I do not understand').

% run the chatbot
:- start.

