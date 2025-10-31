# Week 3 - Monadic Parsing

## Suggested Reading

* [Course Notes Chapter 3](https://diku-dk.github.io/ap-notes/chapter_3.html).
* [Grammars and parsing with Haskell using Parser Combinators](parsernotes.pdf).
* [Monadic Parsing in Haskell](monadic-parsing.pdf) - one of the classic papers.
* [Parsec: A practical parser library](2001-parsec.pdf) - a paper on
  Parsec, one of the first truly practical parser combinator
  libraries, which eventually became *Megaparsec*, which you will use.
* Chapter 13 in *Programming in Haskell*, but the material is similar
  (albeit with different Haskell code) to the course notes.

### Going Beyond

* [Parse, don't validate](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/)

## Exercises

In these exercises you will construct a parser for APL, using the same
AST definition as in the previous weeks. You are also given a [partial
interpreter](handout/apl.hs), corresponding to the solution to last
week's exercises, which allows you to actually run APL programs from
the command line. Feel free to replace this with the fully featured
`APL.Eval` module you developed for your assignments.

As always, the code handout is in [handout/](handout/) and a full
solution is in [solution](solution/).

### Getting started

The parser lives in the `APL.Parser` module you will put your tests in
`APL.Parser_Tests`. The parser module uses
[Megaparsec](https://hackage.haskell.org/package/megaparsec) as its
parser library. It contains various useful imports already, although
you are free to add more if you want.

We will start by implementing a parser for this very simple grammar,
written in
[EBNF](https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_form):

```
digit = "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9";
int = digit {digit};
Exp ::= int "+" int;
```

In the grammar, we write terminals in all-lowercase or enclosed in
single or double quotes, and nonterminals are written with a leading
capital letter.

For the Haskell code we follow the convention discussed in the course
notes where *lexer functions* are named with a leading `l`, and are
the only functions that are allowed to directly manipulate whitespace.
Parser functions are named with a leading `p`, and must not directly
manipulate whitespace (such as by using the `spaces` parser).

Implement a lexer function

```Haskell
lInteger :: Parser Integer
```

that parses (non-negative) integers according to the grammar above
(that is, one or more decimal digits).

To start, let us not worry about whitespace or similar complexities.

#### Hints

You can use the function `read` to convert a string containing decimal
numbers to a Haskell `Integer`. Note that `read` will blow up your
program if the input is malformed, so be careful.

Use `parseTest` to easily test your parser in the REPL:

```
> parseTest lInteger "123"
123
```

Use `isDigit` from `Data.Char` to determine whether a character
corresponds to a decimal digit.

Use the `some` combinator to repeatedly apply a parser and return a
list of the results.

Use the `satisfy` combinator to parse a single character satisfying
some predicate.

#### Solution

<details>
<summary>Open this to see the answer</summary>

```Haskell
lInteger :: Parser Integer
lInteger = read <$> some (satisfy isDigit)
```

</details>

### Handling whitespace

`lInteger` does not properly handle whitespace, which can be seen by
trying to parse an integer followed by end-of-input (`eof`), when the
string has trailing whitespace:

```
> parseTest (lInteger <* eof) "123 "
1:4:
  |
1 | 123
  |    ^
unexpected space
expecting end of input
```

Remember (from the course material) that the way we handle whitespace
is for each function to consume *trailing* whitespace.

Define a combinator

```Haskell
lexeme :: Parser a -> Parser a
```

that applies a given parser, consumes trailing whitespace, then
returns the value of that parser.

#### Hints
Use the `<*` operator.

<details>
<summary>Open this to see the answer</summary>

```Haskell
lexeme :: Parser a -> Parser a
lexeme p = p <* space
```

</details>

Now use `lexeme` to modify `lInteger` to consume trailing whitespace
such as in the `parseTest` example above.

<details>
<summary>Open this to see the answer</summary>

```Haskell
lInteger :: Parser Integer
lInteger = lexeme $ read <$> some (satisfy isDigit)
```

</details>

### Correct tokenisation

The `lInteger` definition still has a subtle issue that will become a
problem later, but we might as well fix it immediately to establish
good habits. To illustrate the example, suppose that we also have a
parser for parsing identifiers like `xyz`. (We will write such a
parser in a bit, but for now it will live only in our imagination). If
we then write a combined parser that parses *first* an integer, then an
identifier, then a string such as `123xyz` would be parsed into an
integer and a variable. This *may* be what we want, but it is not in
accordance with normal conventions regarding syntax, where tokens must
be separated from other tokens by either non-alphanumeric characters
or whitespace.

We want APL to follow normal conventions. That is, we want this
result:

```
> parseTest lInteger "123xyz"
1:4:
  |
1 | 123xyz
  |    ^
unexpected 'x'
```

Try it on your current definition and you will see a different result.

Modify your `lInteger` definition such that it only succeeds if *not*
followed by an alphabetic character.

#### Hints

Use `notFollowedBy` from Megaparsec.

#### Solution

<details>
<summary>Open this to see the answer</summary>

```Haskell
lInteger :: Parser Integer
lInteger =
  lexeme $ read <$> some (satisfy isDigit) <* notFollowedBy (satisfy isAlpha)
```

</details>

### Integer expressions and tests

Using `lInteger` as a building block, change `pExp` such that it can
parse integer expressions (using the `CstInt` constructor).

Then go to `APL.Parser_Tests` and add some tests that test whether you
can parse integers. Use the provided `parserTest` and `parserTestFail`
helper functions. Consider using nested `testGroup`s to organise the tests.

#### Solution

<details>
<summary>Open this to see the answer</summary>

In `APL.Parser`:

```Haskell
pExp :: Parser Exp
pExp = CstInt <$> lInteger
```

In `APL.Parser_Tests`:

```Haskell
parserTests :: TestTree
parserTests =
  testGroup
    "Parsing"
    [ testGroup
        "Constants"
        [ parserTest "123" $ CstInt 123,
          parserTest " 123" $ CstInt 123,
          parserTest "123 " $ CstInt 123,
          parserTestFail "123xyz"
        ]
    ]
```

</details>

### Parsing identifiers

An APL identifier follows this grammar:

```
alphabetic = ? any alphabetic character ?;
alphanumeric = ? any alphanumeric character ?;
var = alphabetic {alphanumeric};
```

That is, it consists of an alphabetic character followed by zero or
more alphanumeric characters.

Implement a parser

```Haskell
lVName :: Parser VName
```

that parses APL identifiers, including consuming trailing whitespace.

Also modify the `pExp` parser to handle identifiers (producing `Var`
nodes) and add appropriate tests.

#### Hints

Use `isAlpha` and `isAlphaNum` from `Data.Char`.

First read one character (which must be alphabetic), then read any
number of characters (which must be alphanumeric).

#### Solution

<details>
<summary>Open this to see the answer</summary>

```Haskell
lVName :: Parser VName
lVName = lexeme $ do
  c <- satisfy isAlpha
  cs <- many $ satisfy isAlphaNum
  pure $ c:cs

pExp :: Parser Exp
pExp =
  choice
    [ CstInt <$> lInteger,
      Var <$> lVName
    ]
```

</details>

### Parsing Boolean values

Booleans follow this grammar in APL:

```
bool ::= "true" | "false";
```

Implement a parser function

```Haskell
pBool :: Parser Bool
```

that parses booleans. As with numbers, it is important that inputs such
as `truee` are not parsed as `true` followed by another character.
Also add a case for `CstBool` to `pExp`.

#### Examples

```
> parseTest pBool "true"
True
> parseTest pBool "truee"
1:5:
  |
1 | truee
  |     ^
unexpected 'e'
> parseTest pBool "true e"
True
```

#### Hints

Consider defining a function

```Haskell
lKeyword :: String -> Parser ()
```

that parses a given alphanumeric string, and uses `notFollowedBy` to
require that it is not followed by another alphanumeric character.

#### Solution

<details>
<summary>Open this to see the answer</summary>

```Haskell
lKeyword :: String -> Parser ()
lKeyword s = lexeme $ void $ try $ chunk s <* notFollowedBy (satisfy isAlphaNum)

pBool :: Parser Bool
pBool =
  choice $
    [ const True <$> lKeyword "true",
      const False <$> lKeyword "false"
    ]

pExp :: Parser Exp
pExp =
  choice
    [ CstInt <$> lInteger,
      CstBool <$> pBool,
      Var <$> lVName
    ]
```

</details>

### Excluding keywords

In most programming languages, some names are *keywords* (sometimes called
*reserved words*) which are not allowed as identifiers. The subset of APL that
we have implemented so far has the following keywords: `true`, `false`. Modify
`lVName` such that it fails if the parsed name is a keyword. In future tasks,
when you implement syntax that adds more keywords (e.g. `if`, `try`), make sure
to modify your implementation such that they are recognised appropriately.

#### Examples

```
> parseTest lVName "if"
1:3:
  |
1 | if
  |   ^
Unexpected keyword
```

Note that the error location is a bit weird. This is because we check
whether the identifier is a keyword after it has been read, as far as
Megaparsec is concerned. Megaparsec does provide a facility for
providing the correct error location, but that is beyond the scope of
this course.

#### Hints

Use `fail` to signal a parse error if the identifier is a keyword.

Because we are signaling a parse error after reading input, you must
wrap the parser in the `try` combinator to indicate a backtracking
point. If you forget to do so, your code will seem to work for now,
but you will have subtle issues when we start to make use of keywords.

#### Solution

<details>
<summary>Open this to see the answer</summary>

```Haskell
-- Remember to extend this as you support more of APL!
keywords :: [String]
keywords =
  [ "true",
    "false"
  ]


lVName :: Parser VName
lVName = lexeme $ try $ do
  c <- satisfy isAlpha
  cs <- many $ satisfy isAlphaNum
  let v = c : cs
  if v `elem` keywords
    then fail "Unexpected keyword"
    else pure v
```

</details>

### Implement infix operators

In this task we will implement the infix operators `+`, `-`, `\`, and
`*`, without taking operator precedence into account. The challenge is
handling left recursion. We will also add explicit parentheses to the
language - note that these do not correspond to any AST constructor.

Specifically, our grammar now looks like this (excluding unchanged
rules from above):

```
Atom ::= var
       | int
       | bool
       | "(" Exp ")"

Exp ::= Atom
      | Exp "+" Exp
      | Exp "-" Exp
      | Exp "*" Exp
      | Exp "/" Exp
```

First you must transform the grammar to eliminate left recursion. Use
the approach explained in the course material. Assume all operators
are left-associative. This will give you a new definition of `Exp`
that recognises the same language.

<details>
<summary>Open this to see the left-factorised grammar</summary>

```
Atom ::= var
       | int
       | bool
       | "(" Exp ")"

Exp0' ::=            (* empty *)
        | "+" Atom Exp0'
        | "-" Atom Exp0'
        | "*" Atom Exp0'
        | "/" Atom Exp0'

Exp0 ::= Atom Exp0'

Exp  ::= Exp0
```

</details>

Finish the parser `pExp` using your transformed grammar. Consider
using the helper function

```
lString :: String -> Parser ()
lString s = lexeme $ void $ chunk s
```

to parse parentheses and operator symbols.

<details>
<summary>Open this to see the implementation</summary>

```Haskell
pAtom :: Parser Exp
pAtom =
  choice
    [ CstInt <$> lInteger,
      CstBool <$> lBool,
      Var <$> lVName,
      lString "(" *> pExp <* lString ")"
    ]

pExp0 :: Parser Exp
pExp0 = pAtom >>= chain
  where
    chain x =
      choice
        [ do
            lString "+"
            y <- pAtom
            chain $ Add x y,
          do
            lString "-"
            y <- pAtom
            chain $ Sub x y,
          do
            lString "*"
            y <- pAtom
            chain $ Mul x y,
          do
            lString "/"
            y <- pAtom
            chain $ Div x y,
          pure x
        ]

pExp :: Parser Exp
pExp = pExp0

```

</details>

#### Examples

```
> parseTest pExp "x+y*z"
Mul (Add (Var "x") (Var "y")) (Var "z")
> parseTest pExp "x+(y*z)"
Add (Var "x") (Mul (Var "y") (Var "z"))
```

### Running the interpreter

You have now implemented enough of the parser for the interpreter to
work for simple programs.

Use `cabal run apl PROG` to run the interpreter program `apl` on a
file `PROG`, which must contain a valid APL expression. The resulting
value (or an error message) will be printed to standard output and
standard error, respectively. You can also do `cabal install` to
install the `apl` executable in `$HOME/.local/bin`, after which you
can simply do `apl PROG` to run programs.

There is no task here. Congratulate yourself on having written a full
Haskell program and continue on.

### Implement operator precedence

Starting from the left-factorised grammar, also separate the parser
rules into levels corresponding to operator priorities. We have only
two levels: `+` and `-` have the same priority, and `*` and `/` have
the same priority, with the latter being the highest.

<details>
<summary>Open this to see the transformed grammar</summary>

```
Atom ::= var
       | int
       | bool
       | "(" Exp ")"

Exp1' ::=            (* empty *)
        | "*" Atom Exp1'
        | "/" Atom Exp1'

Exp1 ::= Atom Exp1'

Exp0' ::=            (* empty *)
        | "+" Exp1 Exp0'
        | "-" Exp1 Exp0'

Exp0 ::= Exp1 Exp0'

Exp  ::= Exp0
```

</details>

Now implement the corresponding parser.

<details>
<summary>Open this to see the implementation</summary>

```Haskell
pAtom :: Parser Exp
pAtom =
  choice
    [ CstInt <$> lInteger,
      CstBool <$> lBool,
      Var <$> lVName,
      lString "(" *> pExp <* lString ")"
    ]

pExp1 :: Parser Exp
pExp1 = pAtom >>= chain
  where
    chain x =
      choice
        [ do
            lString "*"
            y <- pAtom
            chain $ Mul x y,
          do
            lString "/"
            y <- pAtom
            chain $ Div x y,
          pure x
        ]

pExp0 :: Parser Exp
pExp0 = pExp1 >>= chain
  where
    chain x =
      choice
        [ do
            lString "+"
            y <- pExp1
            chain $ Add x y,
          do
            lString "-"
            y <- pExp1
            chain $ Sub x y,
          pure x
        ]

pExp :: Parser Exp
pExp = pExp0
```

</details>

#### Examples

```
> parseTest pExp "x+y*z"
Add (Var "x") (Mul (Var "y") (Var "z"))
```

### Implement `if-then-else`

We extend the grammar to be the following:

```
Atom ::= var
       | int
       | bool
       | "(" Exp ")"

LExp ::= "if" Exp "then" Exp "else" Exp
       | Atom

Exp ::= LExp
      | Exp "+" Exp
      | Exp "-" Exp
      | Exp "*" Exp
      | Exp "/" Exp
```

An `if` expression extends as far as possible, meaning `if x then x else x + x`
is understood as `if x then x else (x + x)`, rather than as `(if x then x else
x) + x`.

Implement this grammar. Remember to handle the new keywords `if`, `then`, and `else`.

#### Solution

<details>
<summary>Open this to see the implementation</summary>

```Haskell
pAtom :: Parser Exp
pAtom =
  choice
    [ CstInt <$> lInteger,
      CstBool <$> lBool,
      Var <$> lVName,
      lString "(" *> pExp <* lString ")"
    ]

pLExp :: Parser Exp
pLExp =
  choice
    [ If
        <$> (lKeyword "if" *> pExp0)
        <*> (lKeyword "then" *> pExp0)
        <*> (lKeyword "else" *> pExp0),
      pAtom
    ]

pExp1 :: Parser Exp
pExp1 = pLExp >>= chain
  where
    chain x =
      choice
        [ do
            lString "*"
            y <- pLExp
            chain $ Mul x y,
          do
            lString "/"
            y <- pLExp
            chain $ Div x y,
          pure x
        ]

pExp0 :: Parser Exp
pExp0 = pExp1 >>= chain
  where
    chain x =
      choice
        [ do
            lString "+"
            y <- pExp1
            chain $ Add x y,
          do
            lString "-"
            y <- pExp1
            chain $ Sub x y,
          pure x
        ]

pExp :: Parser Exp
pExp = pExp0
```

</details>
