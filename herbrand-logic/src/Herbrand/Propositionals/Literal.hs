{-# LANGUAGE UndecidableInstances #-}

module Herbrand.Propositionals.Literal
  ( Literal
  , pattern Literal
  , pattern Positive
  , pattern Negative
  , LiteralSymbols
  , LiteralsOf (..)
  , negated
  ) where

import Data.Kind (Constraint, Type)
import Data.List.NonEmpty (NonEmpty (..), (<|))

import Herbrand (ReduceVariant, S, Signature, Term (Term), reduceTerm, type (:<))
import Herbrand.Propositionals.Conjunction ((:&:) (..))
import Herbrand.Propositionals.Disjunction ((:|:) (..))
import Herbrand.Propositionals.Negation (Neg (..))

--------------------------------------------------------------------------------
-- Literal
--------------------------------------------------------------------------------

-- | An atom term or its negation.
--
-- Example:
--   Literal (MkNeg (Var "p")) :: Literal '[S Var] => ~p
type Literal :: [Type] -> Type
type Literal a = Term (LiteralSymbols a)

pattern Literal :: e :< Signature (LiteralSymbols a) => e -> Literal a
pattern Literal e = Term e

{-# COMPLETE Positive, Negative #-}
pattern Positive :: Term a -> Literal a
pattern Positive t = Literal t

pattern Negative :: Term a -> Literal a
pattern Negative t = Literal (MkNeg t)

type LiteralSymbols :: [Type] -> [Type]
type LiteralSymbols a = '[S (Term a), S (Neg (Term a))]

-- | The literals of one symbol, in order.
type LiteralsOf :: [Type] -> Type -> Constraint
class LiteralsOf a x where
  literalsOf :: x -> NonEmpty (Literal a)

instance {-# OVERLAPPING #-} LiteralsOf a (Literal a) where
  {-# INLINABLE literalsOf #-}
  literalsOf l = l :| []

instance ReduceVariant (LiteralsOf a) (Signature s) => LiteralsOf a (Term s) where
  {-# INLINABLE literalsOf #-}
  literalsOf = reduceTerm @(LiteralsOf a) literalsOf

instance LiteralsOf a rest => LiteralsOf a (Literal a :&: rest) where
  {-# INLINABLE literalsOf #-}
  literalsOf (MkConj l rest) = l <| literalsOf rest

instance LiteralsOf a rest => LiteralsOf a (Literal a :|: rest) where
  {-# INLINABLE literalsOf #-}
  literalsOf (MkDisj l rest) = l <| literalsOf rest

--------------------------------------------------------------------------------
-- Negation
--------------------------------------------------------------------------------

-- Example:
--   negated (Positive (Var "p")) => ~p
--   negated (Negative (Var "p")) => p
negated :: Literal a -> Literal a
negated (Positive t) = Negative t
negated (Negative t) = Positive t
