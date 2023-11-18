# Family Tree Chatbot 

## How It Works

The `chatbot.pl` file serves as the interactive interface. It integrates querying and statement functionalities, allowing users to both retrieve information about familial relationships and add new data to the system.

### Module Integration
- **Loading Modules**: 
  - `:- consult(utils/logic).`
  - `:- use_module(utils/statements).`
  - `:- use_module(utils/questions).`
  - These lines load the necessary modules for logic handling, statement processing, and query functionalities, making them accessible to the chatbot.

### Entry Point
- **`start/0` Predicate**:
  - Initiates the chatbot, welcoming users and entering the main loop for command input and processing.

### Main Loop
- **`main_loop/0` Predicate**:
  - Manages the interactive session where user input is continuously read and processed until the user types "quit".

### Processing User Input
- **`process_input/1` Predicate**:
  - Parses the user input into a list of words and then executes the corresponding command.
- **`parse_input/2` Predicate**:
  - Splits the input string into a list of words, facilitating further processing.

### Executing Commands
- **`execute_command/1` Predicate**:
  - The core of the chatbot, where different commands are matched and executed.
  - It handles a variety of commands related to querying and adding family relationships.
  - The pattern matching in Prolog is utilized to identify and respond to specific user queries and statements.

### Query Commands
- **Example**: `execute_command(["Are", Sibling1, "and", Sibling2, "siblings", ""])`:
  - This pattern matches a query about whether two individuals are siblings and calls `query_sibling/2` to verify this.
  - Similar patterns are used for other family relationships like parent-child, siblings, and extended family members.

### Statement Commands
- **Example**: `execute_command([Sibling1, "and", Sibling2, "are", "siblings"])`:
  - Adds a sibling relationship between two individuals using `add_sibling/2`.
  - Other patterns follow a similar logic for different types of familial relationships, such as adding parents, children, uncles, aunts, etc.

### Special Cases
- **Quit Command**: 
  - Handles the "quit" command to exit the chatbot.
- **Unrecognized Input**:
  - Provides a response for inputs that do not match any known pattern.

### Helper Functions
- **`print_*` Functions**: 
  - Assist in presenting query results in a readable format.
