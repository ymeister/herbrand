herbrand
========

A Haskell library for extensible logical and algebraic structures, built on
one encoding: an open variant over a type-level list of symbols.

The name comes from the Herbrand universe, the set of all terms over a
signature.

> {-# LANGUAGE GHC2024 #-}
> {-# LANGUAGE OverloadedStrings #-}
>
> module Main (main) where
>
> import Data.Map.Strict qualified as Map
>
> import Herbrand (S, Term, eval)
> import Herbrand.Propositionals

A signature is a list of symbols
--------------------------------

A symbol is a plain data type. Each of its type arguments is a hole that a
term fills. `S` marks a symbol in the list, whatever its kind.

> type Core = '[S Var, S Neg, S (:&:)]

`Term Core` is any tree built from those three symbols. Each symbol comes
with a pattern of the same name that builds and matches at the term level,
constrained only by the symbol it uses. The same constructors therefore
build a term in `Core` and a term in the full propositional signature.

> pAndNotQ :: Term Core
> pAndNotQ = Var "p" :&: Neg (Var "q")
>
> rule :: Term Propositionals
> rule = Var "p" :&: Neg (Var "q") :->: Var "r"

Evaluation
----------

`eval` interprets a term under a context. For propositional terms the context
maps variables to booleans. An association list or a `Map` both work, and the
result type selects the interpretation. The keys are the raw `Var` symbols,
built with `MkVar`, not terms.

> truth :: Map.Map Var Bool
> truth = Map.fromList [(MkVar "p", True), (MkVar "q", False), (MkVar "r", True)]
>
> holds :: Bool
> holds = eval truth rule

Printing and parsing
--------------------

`Show` prints infix syntax with the usual precedences, and `Read` parses that
syntax back to the same tree.

> printed :: String
> printed = show rule
>
> parsed :: Term Propositionals
> parsed = read "p & ~q -> r"

`main` prints both formulas, then `True` twice.

> main :: IO ()
> main = do
>   print pAndNotQ
>   putStrLn printed
>   print holds
>   print $ parsed == rule
