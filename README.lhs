# herbrand

A Haskell library for extensible logical and algebraic structures, built on
one encoding: an open variant over a type-level list of symbols.

The name comes from the Herbrand universe, the set of all terms over a
signature.

## A signature is a list of symbols

A symbol is a plain data type. Each of its type arguments is a hole that a
term fills. `S` marks a symbol in the list, whatever its kind.

```haskell
{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Map.Strict qualified as Map

import Herbrand (S, Signature, Term (Term), eval, type (:<))
import Herbrand.Propositionals

type Core = '[S Var, S Neg, S (:&:)]
```

`Term Core` is any tree built from those three symbols. Each symbol comes
with a pattern of the same name that builds and matches at the term level,
constrained only by the symbol it uses. The same constructors therefore
build a term in `Core` and a term in the full propositional signature.

```haskell
pAndNotQ :: Term Core
pAndNotQ = Var "p" :&: Neg (Var "q")

rule :: Term Propositionals
rule = Var "p" :&: Neg (Var "q") :->: Var "r"
```

## Partially filled symbols

A symbol in the list may already have some of its holes filled. The term
then fills only the rest. `(:&:) Literal` is a conjunction whose left child
is fixed to a literal, a term over `Var` alone, and whose right child is the
term being defined.

```haskell
type Literal = Term '[S Var]

type Clause = '[S Var, S ((:&:) Literal)]
```

A `Term Clause` is a variable, or a literal conjoined with another clause: a
right-nested conjunction of variables and nothing else. The constructor for
the filled symbol takes a `Literal` on the left.

```haskell
(&:) :: (Literal :&: Term s) :< Signature s => Literal -> Term s -> Term s
lit &: rest = Term (MkConj lit rest)

infixr 3 &:

clause :: Term Clause
clause = Var "p" &: Var "q" &: Var "r"
```

A conjunction on the left, or a `Neg` anywhere, does not typecheck, because
`Clause` has no symbol for it at that position. Every interpretation defined
on the symbols still applies, so `eval` and `show` work on `clause` as they
do on `rule`.

## Evaluation

`eval` interprets a term under a context. For propositional terms the context
maps variables to booleans. An association list or a `Map` both work, and the
result type selects the interpretation. The keys are the raw `Var` symbols,
built with `MkVar`, not terms.

```haskell
truth :: Map.Map Var Bool
truth = Map.fromList [(MkVar "p", True), (MkVar "q", False), (MkVar "r", True)]

holds :: Bool
holds = eval truth rule
```

## Printing and parsing

`Show` prints infix syntax with the usual precedences, and `Read` parses that
syntax back to the same tree.

```haskell
printed :: String
printed = show rule

parsed :: Term Propositionals
parsed = read "p & ~q -> r"
```

`main` prints the formulas, then `True` twice.

```haskell
main :: IO ()
main = do
  print pAndNotQ
  putStrLn printed
  print clause
  print holds
  print $ parsed == rule
```
