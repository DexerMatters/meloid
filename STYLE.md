
# Style guide

File common settings

- Use `LF` line endings with `2-space` tab-width
- Leave **exactly one empty line** at the end of a file

## General

- Avoid *anti-patterns*
- Do not use the `OVERLAPPING`, `OVERLAPPABLE`, `OVERLAPS` and `INCOHERENT` pragmas
- Always compile with `-Wall`, as in the `meloid.cabal` file
- Never use unboxed types, as they are *behavior-volatile* between versions

## Modules

- Do not *re-export* imported modules
- if there are **multiple** imported/exported names:
  - the first name should be on the same line as the `(`
  - put *each subsequent name on a new line*, as well as the `)`
    (except if the line is **< 50** characters)
  - `,`-s separating the names should be put in the next line indented one level
  - the names should be aligned to the first letter of the first name
- No trailing `,` after the last name
- *exported* names should be listed in order of definition

## Naming

- Use `camelCase` for functions, `PascalCase` is reserved for types
- Be consistent: use `makeThis`or `createThat`, **not both**

## Comments

- Single line comments can either be
  - end of line with *at least one level of indentation after last character of line*
  - own-line with *at least 1 empty line before the comment line*
- Multiline comments should have 1 empty line **before** and **after** the comment delimiters

## Guards

- When writing guard expressions, put a new line before the first `|` character
- **Align all** `|` characters with each otheras well as the following `=`
- Indent all `|` characters by one level comapred to
  the first character of the function the guard is for
- Do no nest guard expressions, use local definitions and `case` expressions instead

## `if ... then ... else ...` and `case ... of ...`

- Use pattern matching whenever possible (e.g.: don't do `if x == 0 then ...`)
- `if ... then` and `case ... of` should be on one line, clauses after
- the `else` keyword should be on its own line, indented at least 1 level
- all clauses shold be indented at least 1 level more than the  `else` keyword in an `if ... then ... else ...` block
- all clauses should be indented at least 1 level in a `case ... of ...` block
- Write each clause on a new line
- Align all clauses with each other in a block
- Do not use inline `case ... of ...` notation
- Inline `if ... then ... else ...` is permissible if it is reasonably short (< 50 characters)
- When using `LambdaCase`, treat the `\case` as `case ... of`

## `do` blocks

- You can use both `do` and bind syntax
- Generally, avoid inlines `do` notation

## `data` and `deriving`:

- When defining multiple constructors, put each on a new line
- Align all `|` characters with the `=`
- put the `deriving` keyword on a new line
- use separate lines or `deriving` keywords:
  - when using **Quantified Constraints**, **Deriving Strategies** or `via`
  - a group of **default-derivable** classes
  - a group of `Functor`, `Foldable`and `Traversable`
- Enclose **single** derived typeclasses in prentheses `()`
- Do not use **deriving extensions** other than `DeriveFunctor`, `DeriveFoldable`, `DeriveTraversable`
  use **deriving strategies** and `DerivingVia` instead
- Do not use **redundant derivations** (e.g.: `Traversable` implies `Functor` and `Traversable`)
- Use `QuantifiedConstraints` (which implies `ExplicitForAll`) where applicable
- Using `UndecidableInstances` is forbidden

## Manual instancing

* Instance functions in the order of declarations in the `class` definition
* When instancing a `class`, separate each **complete definition**:
  1. if the definition is one line, **do not** put an empty line **before** or **after** it
  2. if the definition is multiline, put *one* empty line **before** and **after** it
  3. these empty lines **can overlap**
* Instance *infix* operators with an infix syntax

---

**Anything else not mentioned in this document is up to your discretion
The rule-of-thumb is to be consistent inside one file**

---

Example:

```Haskell
-- this example is incomplete, it will be amended in the future

class Ex a where
  (infixOp)  :: a   -> a        -> *
  (infixOp') :: a   -> a        -> *
  funcFrom   :: a   -> Int      -> *
  funcFrom'   :: a  -> Integral -> *
  funcTo     :: Int -> a        -> *

data A = A1 
       | A2 
       | A3

instance Ex A where
  a infixOp  a' = -- ...
  a infixOp' a' = -- ...
  				  -- empty line before
  funcFrom a n  = {-
    ...
  -}
  				  -- this is the overlapping empty line
  funcFrom' a n  = {-
    ...
  -}
  				  -- empty line after
  funcTo n a    = -- ...
```
