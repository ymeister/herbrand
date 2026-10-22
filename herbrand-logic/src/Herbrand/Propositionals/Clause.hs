{-# LANGUAGE UndecidableInstances #-}

module Herbrand.Propositionals.Clause
  ( Clause
  , pattern Clause
  , ClauseSymbols
  , ClausesOf (..)
  ) where

import Data.Kind (Constraint, Type)
import Data.List.NonEmpty (NonEmpty (..), (<|))

import Herbrand (ReduceVariant, S, Signature, Term (Term), reduceTerm)
import Herbrand.Propositionals.Conjunction ((:&:) (..))
import Herbrand.Propositionals.Disjunction ((:|:) (..))
import Herbrand.Propositionals.Literal (Literal, LiteralsOf (..))

--------------------------------------------------------------------------------
-- Clause
--------------------------------------------------------------------------------

-- | A right-nested conjunction of literals over the atoms @a@.
--
-- Example:
--   Clause (Positive (Var "p") :| [Negative (Var "q")]) :: Clause '[S Var] => p & ~q
type Clause :: [Type] -> Type
type Clause a = Term (ClauseSymbols a)

{-# COMPLETE Clause #-}
pattern Clause :: NonEmpty (Literal a) -> Clause a
pattern Clause ls <- (literalsOf -> ls)
  where
    Clause ls = toClause ls

type ClauseSymbols :: [Type] -> [Type]
type ClauseSymbols a = '[S (Literal a), S ((:&:) (Literal a))]

-- | The clauses of one symbol, in order.
type ClausesOf :: [Type] -> Type -> Constraint
class ClausesOf a x where
  clausesOf :: x -> NonEmpty (Clause a)

instance {-# OVERLAPPING #-} ClausesOf a (Clause a) where
  {-# INLINABLE clausesOf #-}
  clausesOf c = c :| []

instance ReduceVariant (ClausesOf a) (Signature s) => ClausesOf a (Term s) where
  {-# INLINABLE clausesOf #-}
  clausesOf = reduceTerm @(ClausesOf a) clausesOf

instance ClausesOf a rest => ClausesOf a (Clause a :|: rest) where
  {-# INLINABLE clausesOf #-}
  clausesOf (MkDisj c rest) = c <| clausesOf rest

--------------------------------------------------------------------------------
-- Conversion
--------------------------------------------------------------------------------

toClause :: NonEmpty (Literal a) -> Clause a
toClause (l :| []) = Term l
toClause (l :| (l' : ls)) = Term $ MkConj l $ toClause $ l' :| ls
