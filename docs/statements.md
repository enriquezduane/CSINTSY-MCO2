# `statements.pl`

This Prolog module is designed for managing and asserting various familial relationships. It provides predicates for adding relationships such as siblings, parents, children, and extended family members like uncles and aunts. The module also includes logic to check for inconsistencies or impossibilities in these relationships before asserting them.

## Predicates

### Module and Dynamic Declarations
- **Module Declaration**: 
    - `:- module(statements, [List of predicates]).` 
    - This line defines the `statements` module and exports a series of predicates that handle the addition of family relationships.
- **Dynamic Declarations**: 
    - `:- dynamic checked/1.` 
    - Allows the `checked` predicate to be dynamically updated, which is used in gender inference functions.

### Statement Definitions

1. **`add_sibling(Sibling1, Sibling2)`**
    - Checks if `Sibling1` and `Sibling2` are already siblings. If yes, it prints that the relationship exists.
    - If not, it checks if such a sibling relationship is logically impossible (e.g., due to contradictory relationships). If impossible, it indicates so.
    - If neither of these conditions is met, it asserts `Sibling1` and `Sibling2` as siblings in both directions (`Sibling1` as a sibling of `Sibling2` and vice versa).

2. **`add_brother(Brother, Sibling)`**
    - First, it checks if `Brother` is already a brother of `Sibling`. If this relationship exists or is impossible, it responds accordingly.
    - If `Brother` is not known to be male, it asserts his gender.
    - Then it ensures that both `Brother` and `Sibling` are marked as siblings of each other.
    - Finally, it tries to define genders from neutral relatives if needed.
3. **`add_sister(Sister, Sibling)`**
    - Similar to `add_brother`, but for establishing sister relationships. It asserts `Sister`'s gender as female if not already known.

4. **`add_father(Father, Child)`**
    - Checks if `Father` is already the father of `Child`. If this relationship is established or impossible, it responds accordingly.
    - Asserts `Father` as male and as the parent of `Child`.
    - Additionally, it handles cases where siblings have different mothers or fathers, ensuring consistency in parental relationships.

5. **`add_mother(Mother, Child)`**
    - Parallel to `add_father` but for mother relationships. It checks existing relationships, asserts gender, and handles complex family structures.

6. **`add_parents(Parent1, Parent2, Child)`**
    - Verifies if `Parent1` and `Parent2` are already parents of `Child`. If the relationship exists or is impossible, it indicates so.
    - Asserts both `Parent1` and `Parent2` as parents of `Child`.

7. **`add_grandmother(Grandmother, Grandchild)` &  `add_grandfather(Grandfather, Grandchild)`**
    - These predicates add grandmother and grandfather relationships, respectively. They check for existing relationships or impossibilities, assert gender, and then establish the grandparent relationship.

8. **`add_daughter(Daughter, Parent)` & `add_son(Son, Parent)`**
    - These predicates handle adding daughter or son relationships. They check for existing or impossible relationships, assert gender, and establish the parent-child relationship.

9. **`add_child(Child, Parent)`**
    - Checks if `Child` is already a child of `Parent`. If the relationship is established or impossible, it responds accordingly.
    - Asserts the parent-child relationship.

10. **`add_children(Children, Parent)`**
    - Manages the addition of multiple children to a parent, handling each child individually. It processes lists, including handling special cases like empty strings or the word "and."
    - Each child is individually checked and added as a child of the parent.

11. **`add_uncle(Uncle, NieceNephew)` & `add_aunt(Aunt, NieceNephew)`**
    - These predicates add uncle or aunt relationships

. They check for existing or impossible relationships, infer gender, and then assert the relationship.

### Helper Definitions


### `define_genders_from_neutral(Parent)`
- **Purpose and Logic**: Infers and asserts the gender of parents whose gender is not yet defined.
    - **Check & Mark**: First, it checks if `Parent` has already been processed to avoid redundant checks. If not already checked, it marks `Parent` as checked.
    - **Find Other Parents**: Retrieves all co-parents of `Parent's` children.
    - **Gender Inference**:
        - If any co-parent is known to be male, it invokes `define_genders` for further inference.
        - If a co-parent is female, it calls `define_genders_from_mother`.
        - If gender is unknown, no action is taken for that co-parent.
    - **Recursive Call**: Applies the same logic to all retrieved co-parents, ensuring comprehensive gender inference across the family tree.

### `define_genders(Father)`
- **Purpose and Logic**: Infers and asserts the gender of mothers related to a known male parent.
    - **Male Confirmation**: Confirms that `Father` is male.
    - **Find Mothers**: Identifies all co-parents of `Father's` children who are not yet identified as male or female.
    - **Assert Female**: Asserts these co-parents as female.
    - **Recursive Gender Inference**: Calls `define_genders_from_mother` for each co-parent to further infer genders in the family tree.

### `define_genders_from_mother(Mother)`
- **Purpose and Logic**: Similar to `define_genders`, but starts with a known female parent and infers male gender for co-parents.
    - **Female Confirmation**: Ensures that `Mother` is female.
    - **Find Fathers**: Identifies co-parents of `Mother's` children with an undefined gender.
    - **Assert Male**: Asserts these co-parents as male.
    - **Recursive Gender Inference**: Invokes `define_genders` for each male co-parent for further inference.

### `assert_as_male(Person)` and `assert_as_female(Person)`
- **Purpose and Logic**: These helper predicates assert a person as male or female, respectively, if their gender is not already defined.
    - **Check and Assert**: Each predicate checks if the person's gender is not already defined. If undefined, it asserts the person as male or female.
    - **No Redundancy**: Ensures no redundant assertions are made if the gender is already known.
