# herbrand

A Haskell library for extensible logical and algebraic structures, built on
one encoding, an open variant over a type-level list of symbols.

The name comes from the Herbrand universe, the set of all terms over a
signature.

## A signature is a list of symbols

A symbol is a plain data type. Each of its type arguments is a hole that a
term fills. `S` marks a symbol in the list, whatever its kind.

```haskell
{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map

import Herbrand (Eval (..), S, Term (Term))
import Herbrand.Propositionals

type Core = '[S Var, S Neg, S (:&:)]
```

`Term Core` is any tree built from those three symbols. Each symbol has a
pattern of the same name that builds and matches at the term level,
constrained only by that symbol. The same constructors build a term in
`Core` and a term in the full propositional signature.

```haskell
pAndNotQ :: Term Core
pAndNotQ = Var "p" :&: Neg (Var "q")

rule :: Term Propositionals
rule = Var "p" :&: Neg (Var "q") :->: Var "r"
```

## Partially filled symbols

A symbol in the list may already have some of its holes filled. The term
fills only the rest. `(:&:) (Literal a)` is a conjunction whose left child is
a literal and whose right child is the clause being defined, so a `Clause a`
is a right-nested conjunction of literals and nothing else.

```haskell
clause :: Clause '[S Var]
clause = Clause (Positive (Var "p") :| [Negative (Var "q"), Positive (Var "r")])
```

## Normal forms

`dnf` and `cnf` convert a term to these types, so the result type certifies
the form.

```haskell
both :: Term Propositionals
both = (Var "p" :|: Var "q") :&: Var "r"

disjunctive :: DNF '[S Var]
disjunctive = dnf both

conjunctive :: CNF '[S Var]
conjunctive = cnf both
```

Any symbol that is not a connective is an atom and must be in the result's
atom signature.

```haskell
newtype Const = Const Bool

instance Show Const where
  showsPrec _ (Const b) = shows b

instance Eval ctx Const Bool where
  eval _ (Const b) = b

type Extended = S Const : Propositionals

mixed :: Term Extended
mixed = Term (Const True) :&: Var "p" :->: Var "q"

mixedDNF :: DNF '[S Const, S Var]
mixedDNF = dnf mixed
```

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
  print disjunctive
  print conjunctive
  print mixedDNF
  print holds
  print $ parsed == rule
```
