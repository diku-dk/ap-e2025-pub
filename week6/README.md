# Week 6 - Property-based testing

This week we will look at testing based on properties. It also serves as an
example of using monads.

## Slides and Material

## TBA

## Suggested Reading

* [QuickCheck Testing for Fun and Profit](QuickCheckTestingforFunandProfit.pdf)

* [QuickCheck manual](https://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html)

* [Course Notes Chapter 6](https://diku-dk.github.io/ap-notes/chapter_6.html)

### Going Beyond

* [How to Specify it - Guide to writing properties of pure functions](HowToSpecifyIt.pdf)

* [The sad state of property-based testing libraries](https://stevana.github.io/the_sad_state_of_property-based_testing_libraries.html)

## Exercises

In these exercises you will develop QuickCheck generators for expressions and
state some simple properties.

Your starting point is found in [handout/](handout/) and complete solutions are
in [solution/](solution/). For some of the tasks, partial or complete solutions
are also included inline below.

Note that the handout code contains a number of bugs! This is intentional and
part of your task will be discover and fix these bugs.

### Code Overview

The handout code is an amalgam of previous weeks, with some modifications.
In particular:
* `APL.AST` contains the familiar type of expressions. It has an additional
  method `subExp` for enumerating the subexpressions of an expressions.
* `APL.Error` is a new module defining a type of errors, which we will use
  instead of strings when reporting errors.
* `APL.Eval` is the evaluator from week 2, modified to use the new error type.
* `APL.Check` is the type checker from week 2, where `checkExp` is modified
  to return a *list* of possible errors.
* `APL.Tests` is a new module where you will be using QuickCheck.

### A Basic Generator

We will start by making a simple generator that generates a small subset of
expressions, namely lambda expressions and variables.

1. Define `genVar :: Gen VName` so that it generates valid variables names
   (i.e. those accepted by the grammar). You do not need to able to generate
   all possible variable names.

2. Define `genExp :: Gen Exp` so that it generates expressions built using the
   `Lambda` and `Var` constructors.

3. Use `sample` from QuickCheck to see examples of output from `genVar` and `genExp`.

#### Hints

You might find the following QuickCheck functions useful: `elements`, `oneof`, `listOf`.

#### Solution

<details>
<summary>Open this to see the answer</summary>

```Haskell
genVar :: Gen VName
genVar = do
    alpha <- elements ['a' .. 'z']
    alphaNums <- listOf $ elements $ ['a' .. 'z'] ++ ['0' .. '9']
    pure (alpha : alphaNums)

genExp :: Gen Exp
genExp = oneof [Var <$> genVar, Lambda <$> genVar <*> genExp]
```

Then run `sample genVar` and `sample genExp` in ghci.
</details>

### Sized Generation

Extend `genExp` so that it can also produce `Apply` expressions. Try out the
generator again using `sample`. What is the problem?

In order to fix this problem `genExp` take a parameter specifying the size of
its output. Change the type signature to `genExp :: Int -> Gen exp`. The
intention is that `genExp size` must generate expressions with *at most* `size`
constructors in them. For example, `Apply (Apply (Var "f") (Var "x")) (Var "y")`
has a total of 5 constructors.

Using `sized :: (Int -> Gen a) -> Gen a)` from QuickCheck you now have a better
expression generator, `sized genExp`. Try sampling again; is the problem solved?

#### Hints

Note that generated expressions are allowed to be smaller than `size`, but
ideally expressions with exactly `size` constructors should be generated at
least occasionally.

There is more than one reasonable approach to implementing this, so there is no
need to worry if your proposed solution does not seem like the "right" one.

#### Solution

<details>
<summary>Open this to see the answer</summary>

```Haskell
genExp :: Int -> Gen Exp
genExp size =
  if size <= 1
  then Var <$> genVar
  else
    let half = (size - 1) `div` 2
    in oneof
      [ Var <$> genVar
      , Lambda <$> genVar <*> genExp (size - 1)
      , Apply <$> genExp half <*> genExp half
      ]
```

Then run `sample (sized genExp)` in ghci.
</details>

### Completing the Generator

Extend `genExp` so that it can produce all possible expressions. Declare `Exp`
to be an instance of `Arbitrary` with `arbitrary = sized genExp`.

### Associativity

Addition of integers obeys associativity, meaning that `(n1 + n2) + n3 == n1 + (n2 + n3)`
for all integers `n1`, `n2` and `n3`. Declare a function `prop_integerAddAssoc :: Integer -> Integer -> Integer -> Bool`
in `APL.Tests` which expresses this fact. Then run `quickCheck prop_integerAddAssoc` to test this.

What about addition in APL? Do `Add (Add e1 e2) e3` and `Add e1 (Add e2 e3)` always evaluate to the same result?
Declare a function `prop_aplAddAssoc :: Exp -> Exp -> Exp -> Bool` which expresses this hypothesis.

#### Solution

<details>
<summary>Open this to see the answer</summary>

```Haskell
prop_integerAddAssoc :: Integer -> Integer -> Integer -> Bool
prop_integerAddAssoc n1 n2 n3 = (n1 + n2) + n3 == n1 + (n2 + n3)

prop_aplAddAssoc :: Exp -> Exp -> Exp -> Bool
prop_aplAddAssoc e1 e2 e3 = runEval (eval (Add (Add e1 e2) e3)) == runEval (eval (Add e1 (Add e2 e3)))
```

Running `quickCheck prop_aplAddAssoc` should generate a counterexample to the property.
</details>

### Shrinking

Many of the counterexamples for `prop_aplAddAssoc` will be somewhat large, making it hard to spot the issue.
In order to address this we will implement *shrinking* for `Exp` by giving a
definition for `shrink` in the `Arbitrary` instance for `Exp`.

Given an expression `e` we want `shrink e` to return the list of possible
*shrinks* of `e`, i.e. expressions that resemble `e` but are slightly simpler.
In general the possible shrinks of an expression should include its direct
subexpressions as well as a version of the expression where exactly one of its
arguments have been shrunk. For instance `Add e1 e2` shrinks to either
* `e1`
* `e2`
* `Add e1' e2` when `e1` shrinks to `e1'`
* `Add e1 e2'` when `e2` shrinks to `e2'`
Arguments of types other than `Exp` simply use their respective `shrink` methods.
For instance `Var name` shrinks to `Var name'` when `name` shrinks to `name'`.

However, shrinking should not produce expressions that could not have been
generated in the first place, such as `Var ""`.

Implement `shrink` in the `Arbitrary` instance for `Exp` using these rules.

#### Solution (partial)

<details>
<summary>Open this to see the answer</summary>

```Haskell
  shrink (Add e1 e2) =
    e1 : e2 : [Add e1' e2 | e1' <- shrink e1] ++ [Add e1 e2' | e2' <- shrink e2]
  shrink (If cond e1 e2) =
    e1 : e2 : [If cond' e1 e2 | cond' <- shrink cond] ++ [If cond e1' e2 | e1' <- shrink e1] ++ [If cond e1 e2' | e2' <- shrink e2]
  shrink (Var x) =
    [Var x' | x' <- shrink x, not (null x')]
  shrink (Let x e1 e2) =
    e1 : [Let x e1' e2 | e1' <- shrink e1] ++ [Let x e1 e2' | e2' <- shrink e2]
  shrink (Lambda x e) =
    e : [Lambda x e' | e' <- shrink e]
```

Running `quickCheck prop_aplAddAssoc` should now generate a smaller counterexample to the property.
</details>

### Fixing Associativity

Suppose that we *do* want associativity of addition in APL (this is not an
essential property for a programming language, but we will pretend to care).
Modify the evaluator so `prop_aplAddAssoc` passes.

#### Solution (partial)

<details>
<summary>Open this to see the answer</summary>

The most straightforward way is to change `evalIntBinOp` to the following:
```Haskell
evalIntBinOp f e1 e2 = do
  v1 <- eval e1
  case v1 of
    ValInt x -> do
      v2 <- eval e2
      case v2 of
        ValInt y -> ValInt <$> f x y
        _ -> failure NonInteger
    _ -> failure NonInteger
```

Even better would be to abstract out the notion of extracting the integer from
`ValInt` (or failing) into its own function.
</details>
