{-# LANGUAGE UndecidableInstances #-}

module Herbrand.Propositionals.Disjunct
  ( Disjunct
  , pattern Disjunct
  , DisjunctSymbols
  , DisjunctsOf (..)
  ) where

import Data.Kind (Constraint, Type)
import Data.List.NonEmpty (NonEmpty (..), (<|))

import Herbrand (ReduceVariant, S, Signature, Term (Term), reduceTerm)
import Herbrand.Propositionals.Conjunction ((:&:) (..))
import Herbrand.Propositionals.Disjunction ((:|:) (..))
import Herbrand.Propositionals.Literal (Literal, LiteralsOf (..))

--------------------------------------------------------------------------------
-- Disjunct
--------------------------------------------------------------------------------

-- | A right-nested disjunction of literals over the atoms @a@.
--
-- Example:
--   Disjunct (Positive (Var "p") :| [Negative (Var "q")]) :: Disjunct '[S Var] => p | ~q
type Disjunct :: [Type] -> Type
type Disjunct a = Term (DisjunctSymbols a)

{-# COMPLETE Disjunct #-}
pattern Disjunct :: NonEmpty (Literal a) -> Disjunct a
pattern Disjunct ls <- (literalsOf -> ls)
  where
    Disjunct ls = toDisjunct ls

type DisjunctSymbols :: [Type] -> [Type]
type DisjunctSymbols a = '[S (Literal a), S ((:|:) (Literal a))]

-- | The disjuncts of one symbol, in order.
type DisjunctsOf :: [Type] -> Type -> Constraint
class DisjunctsOf a x where
  disjunctsOf :: x -> NonEmpty (Disjunct a)

instance {-# OVERLAPPING #-} DisjunctsOf a (Disjunct a) where
  {-# INLINABLE disjunctsOf #-}
  disjunctsOf d = d :| []

instance ReduceVariant (DisjunctsOf a) (Signature s) => DisjunctsOf a (Term s) where
  {-# INLINABLE disjunctsOf #-}
  disjunctsOf = reduceTerm @(DisjunctsOf a) disjunctsOf

instance DisjunctsOf a rest => DisjunctsOf a (Disjunct a :&: rest) where
  {-# INLINABLE disjunctsOf #-}
  disjunctsOf (MkConj d rest) = d <| disjunctsOf rest

--------------------------------------------------------------------------------
-- Conversion
--------------------------------------------------------------------------------

toDisjunct :: NonEmpty (Literal a) -> Disjunct a
toDisjunct (l :| []) = Term l
toDisjunct (l :| (l' : ls)) = Term $ MkDisj l $ toDisjunct $ l' :| ls
