module Herbrand.Propositionals.CNF
  ( CNF
  , pattern CNF
  , CNFSymbols
  , cnf
  ) where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))

import Herbrand (S, Term (Term))
import Herbrand.Propositionals.Conjunction ((:&:) (..))
import Herbrand.Propositionals.Disjunct (Disjunct, pattern Disjunct, DisjunctsOf (..))
import Herbrand.Propositionals.DNF (Polar (..), ToDNF (..))
import Herbrand.Propositionals.Literal (negated)

--------------------------------------------------------------------------------
-- Normal form
--------------------------------------------------------------------------------

-- | A right-nested conjunction of disjuncts over the atoms @a@.
--
-- Example:
--   cnf (Var "p" :&: Neg (Var "q") :->: Var "r" :: Term Propositionals) :: CNF '[S Var]
--   => (~p | q | r)
type CNF :: [Type] -> Type
type CNF a = Term (CNFSymbols a)

{-# COMPLETE CNF #-}
pattern CNF :: NonEmpty (Disjunct a) -> CNF a
pattern CNF ds <- (disjunctsOf -> ds)
  where
    CNF ds = toCNF ds

type CNFSymbols :: [Type] -> [Type]
type CNFSymbols a = '[S (Disjunct a), S ((:&:) (Disjunct a))]

--------------------------------------------------------------------------------
-- Conversion
--------------------------------------------------------------------------------

-- | The De Morgan dual of the negation's disjunctive normal form.
cnf :: forall a x. ToDNF a x => x -> CNF a
cnf x = CNF $ Disjunct . fmap negated <$> p.neg
  where
    p :: Polar a
    p = polar x

toCNF :: NonEmpty (Disjunct a) -> CNF a
toCNF (d :| []) = Term d
toCNF (d :| (d' : ds)) = Term $ MkConj d $ toCNF $ d' :| ds
