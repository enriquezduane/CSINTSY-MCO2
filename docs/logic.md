# `logic.pl`

This Prolog module, defines a set of predicates for modeling familial relationships and constraints on these relationships. It includes predicates for defining direct relationships (like parent, sibling, child), extended relationships (like uncle, aunt, cousin), and constraints for impossible relationships (to prevent logical inconsistencies).

## Predicates

### Module and Dynamic Declarations
- **Module Declaration**: 
    - `:- module(logic, [List of predicates]).` 
    - This line defines the `logic` module and exports the listed predicates so they can be used by other files.

- **Dynamic Declarations**: 
    - `:- dynamic predicate_name/arity.` 
    - These lines allow for the addition or removal of facts from the database at runtime for the listed predicates.

### Direct Relationship Definitions

1. **`sibling(X, Y)`**:
    - **Purpose**: Determines if `X` and `Y` are siblings.
    - **Definition**: `X` and `Y` are siblings if they share a common parent and are not the same person (`X \= Y`).

2. **`brother(X, Y)`**:
    - **Purpose**: Checks if `X` is a brother of `Y`.
    - **Definition**: `X` is a brother of `Y` if `X` is male and a sibling of `Y`.

3. **`sister(X, Y)`**:
    - **Purpose**: Identifies if `X` is a sister of `Y`.
    - **Definition**: `X` is a sister of `Y` if `X` is female and a sibling of `Y`.

4. **`father(X, Y)`**:
    - **Purpose**: Determines if `X` is the father of `Y`.
    - **Definition**: `X` is the father if `X` is male and a parent of `Y`.

5. **`mother(X, Y)`**:
    - **Purpose**: Identifies if `X` is the mother of `Y`.
    - **Definition**: `X` is the mother of `Y` if `X` is female and a parent of `Y`.

6. **`grandfather(X, Y)`**:
    - **Purpose**: Determines if `X` is the grandfather of `Y`.
    - **Definition**: `X` is the grandfather of `Y` if `X` is the father of one of Y's parents.

7. **`grandmother(X, Y)`**:
    - **Purpose**: Determines if `X` is the grandmother of `Y`.
    - **Definition**: `X` is the grandmother of `Y` if `X` is the mother of one of Y's parents.

8. **`child(X, Y)`**:
    - **Purpose**: Checks if `X` is a child of `Y`.
    - **Definition**: `X` is a child of `Y` if `Y` is a parent of `X`.

9. **`son(X, Y)`**:
    - **Purpose**: Determines if `X` is the son of `Y`.
    - **Definition**: `X` is the son of `Y` if `X` is male and a child of `Y`.

10. **`daughter(X, Y)`**:
    - **Purpose**: Determines if `X` is the daughter of `Y`.
    - **Definition**: `X` is the daughter of `Y` if `X` is female and a child of `Y`.

11. **`uncle(X, Y)`**:
    - **Purpose**: Identifies if `X` is the uncle of `Y`.
    - **Definition**: `X` is the uncle of `Y` if `X` is a brother of one of Y's parents.

12. **`aunt(X, Y)`**:
    - **Purpose**: Identifies if `X` is the aunt of `Y`.
    - **Definition**: `X` is the aunt of `Y` if `X` is a sister of one of Y's parents.

13. **`cousin(X, Y)`**:
    - **Purpose**: Determines if `X` and `Y` are cousins.
    - **Definition**: `X` and `Y` are cousins if a parent of `X` and a parent of `Y` are siblings.

14. **`relative(X, Y)`**:
    - **Purpose**: Checks if `X` and `Y` are relatives.
    - **Definition**: `X` and `Y` are considered relatives if they are connected through a parent, child, sibling, grandparent, uncle, aunt, or cousin relationship.

### Impossible Relationship Definitions

1. **`impossible_sibling(X, Y)`**:
    - **Purpose**: Determines conditions where `X` and `Y` cannot logically be siblings.
    - **Definition**: Includes conditions like being the same person, having different parents, or existing parent/child relationships that contradict them being siblings.

2. **`impossible_brother(X, Y)`**:
    - **Purpose**: Identifies situations where `X` cannot be a brother of `Y`.
    - **Definition**: Applies if `X` is female or if `X` and `Y` meet the criteria of `impossible_sibling`.

3. **`impossible_sister(X, Y)`**:
    - **Purpose**: Determines scenarios where `X` cannot be a sister of `Y`.
    - **Definition**: Applies if `X` is male or if `X` and `Y` are `impossible_sibling`s.

4. **`impossible_parent(X, Y)`**:
    - **Purpose**: Establishes conditions where `X` cannot be a parent of `Y`.
    - **Definition**: Includes being the same person, `Y` already having different parents, or other relationships that logically prevent a parent relationship.

5. **`impossible_parent(X, Y, Z)`**:
    - **Purpose**: Defines conditions where `X` and `Y` cannot both be parents of `Z`.
    - **Definition**: Considers cases like either parent being the same person as `Z`, or `Z` already having different parents.

6. **`impossible_father(X, Y)`**:
    - **Purpose**: Specifies when `X` cannot be the father of `Y`.
    - **Definition**: Includes `X` being female, `Y` already having a father, or conditions met for `impossible_parent`.

7. **`impossible_mother(X, Y)`**:
    - **Purpose**: Identifies when `X` cannot be the mother of `Y`.
    - **Definition**: Covers `X` being male, `Y` already having a mother, or meeting `impossible_parent` criteria.

8. **`impossible_grandmother(X, Y)`**:
    - **Purpose**: Defines when `X` cannot be the grandmother of `Y`.
    - **Definition**: Includes `X` being male, already having grandmothers, or other relationships that contradict a grandmother relationship.

9. **`impossible_grandfather(X, Y)`**:
    - **Purpose**: Establishes when `X` cannot be the grandfather of `Y`.
    - **Definition**: Criteria include `X` being female, already having grandfathers, or other contradictory relationships.

10. **`impossible_child(X, Y)`**:
    - **Purpose**: Details conditions where `X` cannot be a child of `Y`.
    - **Definition**: Considerations include `X` being the same as `Y`, already having parents, or other relationships negating a child-parent relationship.

11. **`impossible_son(X, Y)`**:
    - **Purpose**: Determines when `X` cannot be the son of `Y`.
    - **Definition**: Criteria include `X` being female or conditions met for `impossible_child`.

12. **`impossible_daughter(X, Y)`**:
    - **Purpose**: Specifies when `X` cannot be the daughter of `Y`.
    - **Definition**: Includes `X` being male or meeting conditions of `impossible_child`.

13. **`impossible_uncle(X, Y)`**:
    - **Purpose**: Defines conditions where `X` cannot be an uncle of `Y`.
    - **Definition**: Includes `X` being female, being the same person as `Y`, or not being a brother to any of Y's parents.

14. **`impossible_aunt(X, Y)`**:
    - **Purpose**: Establishes when `X` cannot be an aunt of `Y`.
    - **Definition**: Criteria include `X` being male, being the same person as `Y`, or not being a sister to any of Y's parents.

### Helper Definitions

1. **`different_parents(X, Y)`**:
    - **Purpose**: Checks if `X` and `Y` do not share a single parent.
    - **Definition**: Considers `X` and `Y` to have different parents if no parent is shared between them.

2. **`already_has_parents(X)`**:
    - **Purpose**: Determines if `X` already has two different parents.
    - **Definition**: True if `X` is recorded with two distinct parents in the database.

3. **`already_has_parents(X, Y)`**:
    - **Purpose**: Checks if `X` has two different parents, excluding `Y`.
    - **Definition**: True if `X` has two parents that are different from `Y`.

4. **`already_has_parents(X, Y, Z)`**:
    - **Purpose**: Determines if `X` cannot have two new parents `Y` and `Z`.
    - **Definition**: True if `X` already has one parent different from `Y` and `Z`.

5. **`sibling_has_different_parents(X, Y)`**:
    - **Purpose**: Checks if `X` cannot be a parent of `Y` based on siblings of `Y` with different parents.
    - **Definition**: True if `Y` has a sibling with different parents and `X` is not one of those parents.

6. **`already_has_father(X)`**:
    - **Purpose**: Determines if `X` already has a father.
    - **Definition**: True if `X` is recorded with a father in the database.

7. **`already_has_mother(X)`**:
    - **Purpose**: Checks if `X` already has a mother.
    - **Definition**: True if `X` is recorded with a mother in the database.

8. **`already_has_grandmothers(X)`**:
    - **Purpose**: Determines if `X` already has two different grandmothers.
    - **Definition**: True if `X` has two distinct grandmothers recorded.

9. **`already_has_grandfathers(X)`**:
    - **Purpose**: Checks if `X` already has two different grandfathers.
    - **Definition**: True if `X` has two distinct grandfathers recorded.

10. **`unrelated_grandparent(X, Y)`**:
    - **Purpose**: Determines if `X` cannot be a grandparent of `Y`.
    - **Definition**: True if the parents of `Y` are not children of `X`.

11. **`unrelated_uncle(X, Y)`**:
    - **Purpose**: Checks if `X` cannot be an uncle of `Y`.
    - **Definition**: True if `X` is not a brother to any of the parents of `Y`.

12. **`unrelated_aunt(X, Y)`**:
    - **Purpose**: Determines if `X` cannot be an aunt of `Y`.
    - **Definition**: True if `X` is not a sister to any of the parents of `Y`.
