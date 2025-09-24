# Week 1 - Programming in Haskell

In this week we will look at the basics of Haskell programming, including type
classes. While we do have to spend some time on the minutiae of syntax and basic
definitions, our true goal is to start on acquiring an understanding of how to
structure programs along abstract and principled lines. Type classes are the
most explicit example of this idea.

## Slides and material

[Slides from Tuesday](1a.pdf)

## Suggested Reading

* Part 1 of *Programming in Haskell*. Most of this is standard functional
  programming material, so feel free to skim those parts. The most interesting
  chapter is chapter 8.

* [Course Notes Chapter 1](https://diku-dk.github.io/ap-notes/chapter_1.html)

### Going Beyond

* [Real World Haskell](https://book.realworldhaskell.org/) (a bit dated,
  however).

## Exercises

The mandatory assignment builds directly on the concepts *and code* developed
during the exercises. You are strongly advised to complete the exercises before
embarking on the assignment. It may also be a useful exercise to do other
exercises (unrelated to the assignment), for example the ones in *Programming in
Haskell*.

In these exercises you will be developing an interpreter for a very simple
language of arithmetic expressions, which we call *APL* for *AP Language*.

Your starting point is found in [handout/](handout/) and complete solutions are
in [solutions/](solutions/). For some of the tasks, partial or complete
solutions are also included inline below.

The starting point is a skeletal Haskell program consisting of the modules
`APL.AST` (in `src/APL/AST.hs`), `APL.Eval` (in `src/APL/Eval.hs`) and
`APL.Eval_Tests` (in `src/APL/Eval_Tests.hs`), as well as `week1.cabal` with
build instructions. You should start by running `cabal test` to ensure that your
Haskell setup is functional. This compiles everything and runs the (currently
empty) test suite. When going through these exercises, run `cabal test`
frequently to see whether your code still works. It is also a good idea to use
`ghci` to interactively debug and test your work.

The `APL.AST` module will contain the definition of APL. Initially it is almost
empty.

The `APL.Eval` module will import `APL.AST` and define an evaluation function
for APL programs.

The `APL.Eval_Tests` module should import definitions from `APL.AST` and
`APL.Eval` and then tests them. It also imports unit testing modules from `tasty`.
Initially it imports almost nothing, so you will have to extend the `import`
statements with the things you need.

The `runtests.hs` file is a very simple program that runs the test suite. You
should not modify it.

### Getting started

We will represent expressions by a Haskell datatype `Exp`. It will initially be
tiny, and then we will extend it as we go along. In the `APL.AST` module, add
the following definition:

```Haskell
data Exp
  = CstInt Integer
  deriving (Eq, Show)
```

The `CstInt` constructor represents an expression that is a constant integer,
like `1`. The `Eq` instance is so we can compare expressions with `==`, and the
`Show` is so we can print them to the screen (critical for debugging).

You should also export the `Exp` type, including its constructors, from the
`APL.AST` module so it can be imported by other modules. Do this by adding
`Exp(..)` to the module export list.

The result of *evaluating* an expression is a *value*, which we represent with
the type `Val`. Add the following definition to the `APL.Eval` module:

```Haskell
data Val
  = ValInt Integer
  deriving (Eq, Show)
```

(The `Exp` and `Val` types are currently isomorphic, but will not remain so for
long.)

We will now define an evaluation function that can transform an expression to a
value. In `APL.Eval`, define a function `eval` with the following type:

```Haskell
eval :: Exp -> Val
```

Implement the `eval` function case for the `CstInt` case. A constant integer
evaluates (unsurprisingly) to the corresponding integer value.

<details> <summary>Open this to see the answer</summary>

```Haskell
eval (CstInt x) = ValInt x
```

</details>

Export the `eval` function from `APL.Eval`, and add a test to `Eval_Tests.hs`
that tests whether your implementation of `CstInt` is correct. You will need to
extend the `import` statements at the beginning of the module to include at
least the following:

```Haskell
import APL.AST (Exp (..))
import APL.Eval (Val (..), eval)
import Test.Tasty.HUnit (testCase, (@?=))
```

Sabotage either your implementation of `eval` or the test itself such that the
test fails, and ensure that this is detected by `cabal test`. Then fix it before
continuing.

### Implementing arithmetic

Add the following constructors to `Exp`:

```Haskell
data Exp
  = ...
  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp
  | Div Exp Exp
  | Pow Exp Exp
```

These correspond to addition, subtraction, multiplication, division, and
exponentiation. Implement the corresponding cases in `eval` and add appropriate
tests to the test suite.

#### Examples of `Exp`

The following examples show how various terms in (relatively) conventional
mathematical notation are written as `Exp`.

| Math | `Exp` |
| --- | --- |
| 2+3 | `Add (CstInt 2) (CstInt 3)` |
| 2<sup>3</sup> | `Pow (CstInt 2) (CstInt 3)` |
| 2<sup>3+4</sup> | `Pow (CstInt 2) (Add (CstInt 3) (CstInt 4))` |
| 2+3*4 | `Add (CstInt 2) (Mul (CstInt 3) (CstInt 4))` |
| (2+3)\*4 | `Mul (Add (CstInt 2) (CstInt 3)) (CstInt 4)` |

Note that the parentheses that we use in mathematical notation are not present
in `Exp` - this is because the structure of the Haskell value already precisely
encodes which expressions are operands of which operators. In the nomenclature
of programming languages, the `Exp` value is an *Abstract Syntax Tree* (AST),
with unnecessary information excluded.

#### Hints

* The Haskell function for integer division is `div`. You can use it infix by
  enclosing it in backticks: `` x `div` y``.

* The Haskell function for integer exponentiation is `^`.

#### Consider

What happens with division by zero or using a negative exponent? Can you test
for this?

<details> <summary>Open this to see the answer</summary>

The Haskell functions `div` and `^` are *partial*, meaning they can crash with a
so-called *imprecise exception* (an exception that is not visible in the type,
also sometimes called *IO exceptions*). It is possible to catch and handle such
exceptions, but not with the parts of Haskell you have seen so far, so it is not
expected that you have tests for these cases - *yet*.

</details>

### Representing Failure

Now we will extend the interpreter to make it clear in the type of `eval` that
some operations can fail. For now, the only failure cases are division by zero
and taking a negative exponent.

Add a type synonym for error messages:

```Haskell
type Error = String
```

the type `Error` is completely interchangeable with `String`, but is named to
make its intended use in function types explicit. In a more sophisticated
system, we might use a more complicated `Error` type with various constructors
for indicating different kinds of errors. For now, we will treat it as a
human-readable string.

Now change the type of `eval` to be the following:

```Haskell
eval :: Exp -> Either Error Val
```

The `Either` type is often used to express computations that can fail. It is
defined as follows (it is already defined in the implicit Haskell prelude, so
you should *not* add it to `APL.AST`):

```Haskell
data Either a b = Left a
                | Right b
```

By convention, the `Left` constructor is used to represent failure, and the
`Right` constructor to represent success. It is documented at
[Data.Either](https://hackage.haskell.org/package/base-4.20.0.1/docs/Data-Either.html).

You must now modify the definition of `eval` to wrap the return value in either
`Left` or `Right` depending on whether evaluation succeeds. Evaluation of an
expression succeeds if its subexpressions succeed, and the expression itself
does not fail (by dividing with zero or taking a negative exponent).

You must also modify your tests. Make sure to add tests for the failure cases.

#### Hints

It is a good idea to start by commenting out all your existing code so you do
not get overwhelmed with type errors.

As a starting point, here is the clause for `CstInt`:

```Haskell
eval (CstInt x) = Right $ ValInt x
```

When handling compound operations such as `Add`, use `case` to determine whether
the subexpressions succeeded.

Your implementations of `Add`, `Sub`, `Mul`, and `Div` will be quite similar.
Consider constructing a helper function to reduce the duplication of logic.

#### Solution (partial)

<details> <summary>Open this to see the answer</summary>

```Haskell
eval (Add e1 e2) =
  case (eval e1, eval e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> Right $ ValInt $ x + y

```

</details>

### Add conditionals

To get slightly closer to a programming language, we will now add boolean values
and control flow.

Add the following constructors to `Exp`:

```Haskell
data Exp
  = ...
  | CstBool Bool
  | Eql Exp Exp
  | If Exp Exp Exp
```

Add the following constructor to `Val`:

```Haskell
data Val
  = ...
  | ValBool Bool
```

Your task is to implement the corresponding cases in `eval`.

* A `CstBool` should evaluate to a `ValBool`, similar to `CstInt`.

* A `Eql` should return `ValBool True` if the operands evaluate to the same
  value, and `ValBool False` otherwise.

* `If` should evaluate the first expression. If true, it should then evaluate
  the second expression, and otherwise the third expression.

Whenever we add new features, [we also add potential for new
bugs](https://dl.acm.org/doi/pdf/10.1145/1283920.1283936). We have added the
following new ways for evaluation to fail:

* Comparing an integer and a boolean with `Eql` (mixing types is not allowed).

* The first expression in an `If` (the condition) evaluates to an integer.

These should result in a `Left` value, similar to division by zero.

#### Consider

Would it be an acceptable implementation for `If` to start by evaluating all
three subexpressions (including propagating errors), similar to how we implement
`Add`? If not, make sure to write a test that would fail if that is how `If` was
implemented.

#### Solution

<details> <summary>Open this to see the answer</summary>

```Haskell
eval (Eql e1 e2) =
  case (eval e1, eval e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> Right $ ValBool $ x == y
    (Right (ValBool x), Right (ValBool y)) -> Right $ ValBool $ x == y
    (Right _, Right _) -> Left "Invalid operands to equality"
eval (If cond e1 e2) =
  case eval cond of
    Left err -> Left err
    Right (ValBool True) -> eval e1
    Right (ValBool False) -> eval e2
    Right _ -> Left "Non-boolean conditional."
```

</details>

### Adding Variables

We will now add variable bindings, through which an expression can be bound to a
name and used multiple times. We will assume you are already familiar with the
concept of variables.

Add the following type synonym:

```Haskell
type VName = String
```

Add the following constructors to `Exp`:

```Haskell
data Exp
  = ...
  | Var VName
  | Let VName Exp Exp
  deriving (Eq, Show)
```

`Var` is a reference to a variable, and `Let` binds a name to the result of
evaluating the first expression while evaluating the second expression.

#### Environments

To implement variables, we need to maintain *environments*. An environment maps
a name to a value. Evaluation of an expression takes place in an environment. We
will represent environments with this `Env` type, which you must add to
`APL.Eval`:

```Haskell
type Env = [(VName, Val)]
```

An environment consists of a list of pairs, which each pair comprising a
variable and its associated value. We call this a *variable binding*. For
operating on environments we will make use of definitions with the following
types:

```Haskell
-- | Empty environment, which contains no variable bindings.
envEmpty :: Env

-- | Extend an environment with a new variable binding,
-- producing a new environment.
envExtend :: VName -> Val -> Env -> Env

-- | Look up a variable name in the provided environment.
-- Returns Nothing if the variable is not in the environment.
envLookup :: VName -> Env -> Maybe Val
```

Finish the definition of `envEmpty`, `envExtend`, and `envLookup`. You may find
the standard [`lookup`
function](https://hackage.haskell.org/package/base-4.20.0.1/docs/Prelude.html#v:lookup)
useful, but you can also write it explicitly as a recursive function.

<details> <summary>Open this to see the answer</summary>

```Haskell
envEmpty :: Env
envEmpty = []

envExtend :: VName -> Val -> Env -> Env
envExtend v val env = (v, val) : env

envLookup :: VName -> Env -> Maybe Val
envLookup v env = lookup v env
```

</details>

#### Evaluating in an Environment

We are ready to extend `eval` itself with environments. As a starting point,
change the type of `eval` to the following:

```Haskell
eval :: Env -> Exp -> Either Error Val
```

Update your existing code such that it merely propagates this new parameter to
recursive `eval` applications. For the "top level" application of `eval` (for
example in your tests), pass `envEmpty`.

Now add `eval` clauses for `Var` and `Let`.

For `Var`, use `envLookup` to perform a lookup in the environment. If the
variable is not in the environment, then that should be reported as a failure.

For `Let`, evaluate the first expression. If that fails, evaluation of the
entire expression also fails. Otherwise, use `envExtend` to add a binding of the
variable to the environment, and use that extended environment when evaluating
the second expression.

#### Examples

```
> eval envEmpty (Let "x" (CstInt 3) (Add (Var "x") (Var "x")))
Right (ValInt 6)
> eval envEmpty (Let "x" (CstInt 3) (Add (Var "x") (Var "y")))
Left "Unknown variable: y"
```

#### Solution

<details> <summary>Open this to see the answer</summary>

```Haskell
eval env (Var v) = case envLookup v env of
  Just x -> Right x
  Nothing -> Left $ "Unknown variable: " ++ v
eval env (Let var e1 e2) =
  case eval env e1 of
    Left err -> Left err
    Right v -> eval (envExtend var v env) e2

```

</details>
