{-# LANGUAGE UndecidableInstances #-}

module Herbrand.Propositionals.DNF
  ( DNF
  , pattern DNF
  , DNFSymbols
  , Polar (..)
  , ToDNF (..)
  , dnf
  ) where

import Data.Kind (Constraint, Type)
import Data.List.NonEmpty (NonEmpty (..))

import Herbrand (ReduceVariant, S, Signature, Term (Term), reduceTerm, type (:<))
import Herbrand.Propositionals.Clause (Clause, pattern Clause, ClausesOf (..))
import Herbrand.Propositionals.Conjunction ((:&:) (..))
import Herbrand.Propositionals.Disjunction ((:|:) (..))
import Herbrand.Propositionals.Equivalence ((:=:) (..))
import Herbrand.Propositionals.Implication ((:->:) (..))
import Herbrand.Propositionals.Literal (Literal, pattern Negative, pattern Positive)
import Herbrand.Propositionals.Negation (Neg (..))

--------------------------------------------------------------------------------
-- Normal form
--------------------------------------------------------------------------------

-- | A right-nested disjunction of clauses over the atoms @a@.
--
-- Example:
--   dnf (Var "p" :&: Neg (Var "q") :->: Var "r" :: Term Propositionals) :: DNF '[S Var]
--   => ~p | q | r
type DNF :: [Type] -> Type
type DNF a = Term (DNFSymbols a)

{-# COMPLETE DNF #-}
pattern DNF :: NonEmpty (Clause a) -> DNF a
pattern DNF cs <- (clausesOf -> cs)
  where
    DNF cs = toDNF cs

type DNFSymbols :: [Type] -> [Type]
type DNFSymbols a = '[S (Clause a), S ((:|:) (Clause a))]

-- | The clauses of a formula's normal form and of its negation's, so that a
-- connective combines both without a separate negation pass.
--
-- Example:
--   polar (Var "p" :: Term Propositionals) :: Polar '[S Var]
--   => Polar { pos = (p :| []) :| [], neg = (~p :| []) :| [] }
type Polar :: [Type] -> Type
data Polar a = Polar
  { pos :: NonEmpty (NonEmpty (Literal a))
  , neg :: NonEmpty (NonEmpty (Literal a))
  }

-- | Conversion to normal form over the atoms @a@. The five connectives have
-- their own instances. Every other symbol is an atom and must be in @a@.
type ToDNF :: [Type] -> Type -> Constraint
class ToDNF a x where
  polar :: x -> Polar a

instance {-# OVERLAPPABLE #-} x :< Signature a => ToDNF a x where
  {-# INLINABLE polar #-}
  polar x = Polar
    { pos = (Positive (Term x) :| []) :| []
    , neg = (Negative (Term x) :| []) :| []
    }

instance ReduceVariant (ToDNF a) (Signature s) => ToDNF a (Term s) where
  {-# INLINABLE polar #-}
  polar = reduceTerm @(ToDNF a) polar

instance ToDNF a e => ToDNF a (Neg e) where
  {-# INLINABLE polar #-}
  polar (MkNeg e) = Polar { pos = p.neg, neg = p.pos }
    where
      p :: Polar a
      p = polar e

instance (ToDNF a e1, ToDNF a e2) => ToDNF a (e1 :&: e2) where
  {-# INLINABLE polar #-}
  polar (MkConj e1 e2) = Polar
    { pos = distribute l.pos r.pos
    , neg = l.neg <> r.neg
    }
    where
      l :: Polar a
      l = polar e1

      r :: Polar a
      r = polar e2

instance (ToDNF a e1, ToDNF a e2) => ToDNF a (e1 :|: e2) where
  {-# INLINABLE polar #-}
  polar (MkDisj e1 e2) = Polar
    { pos = l.pos <> r.pos
    , neg = distribute l.neg r.neg
    }
    where
      l :: Polar a
      l = polar e1

      r :: Polar a
      r = polar e2

instance (ToDNF a e1, ToDNF a e2) => ToDNF a (e1 :->: e2) where
  {-# INLINABLE polar #-}
  polar (MkImpl e1 e2) = Polar
    { pos = l.neg <> r.pos
    , neg = distribute l.pos r.neg
    }
    where
      l :: Polar a
      l = polar e1

      r :: Polar a
      r = polar e2

instance (ToDNF a e1, ToDNF a e2) => ToDNF a (e1 :=: e2) where
  {-# INLINABLE polar #-}
  polar (MkEquiv e1 e2) = Polar
    { pos = distribute l.pos r.pos <> distribute l.neg r.neg
    , neg = distribute l.pos r.neg <> distribute l.neg r.pos
    }
    where
      l :: Polar a
      l = polar e1

      r :: Polar a
      r = polar e2

--------------------------------------------------------------------------------
-- Conversion
--------------------------------------------------------------------------------

dnf :: forall a x. ToDNF a x => x -> DNF a
dnf x = DNF $ Clause <$> p.pos
  where
    p :: Polar a
    p = polar x

toDNF :: NonEmpty (Clause a) -> DNF a
toDNF (c :| []) = Term c
toDNF (c :| (c' : cs)) = Term $ MkDisj c $ toDNF $ c' :| cs

-- | Every clause of the left with every clause of the right.
distribute
  :: NonEmpty (NonEmpty (Literal a))
  -> NonEmpty (NonEmpty (Literal a))
  -> NonEmpty (NonEmpty (Literal a))
distribute ls rs = (<>) <$> ls <*> rs
