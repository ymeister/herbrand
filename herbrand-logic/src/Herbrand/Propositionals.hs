module Herbrand.Propositionals
  ( module Herbrand.Propositionals.Variable
  , module Herbrand.Propositionals.Negation
  , module Herbrand.Propositionals.Conjunction
  , module Herbrand.Propositionals.Disjunction
  , module Herbrand.Propositionals.Implication
  , module Herbrand.Propositionals.Equivalence
  , Propositionals
  ) where

import Data.Kind (Type)

import Herbrand.Propositionals.Conjunction
import Herbrand.Propositionals.Disjunction
import Herbrand.Propositionals.Equivalence
import Herbrand.Propositionals.Implication
import Herbrand.Propositionals.Negation
import Herbrand.Propositionals.Variable
import Herbrand.Term (S)

--------------------------------------------------------------------------------
-- Signature
--------------------------------------------------------------------------------

-- | Example:
--   Var "p" :&: Neg (Var "q") :: Term Propositionals
type Propositionals :: [Type]
type Propositionals = '[S Var, S Neg, S (:&:), S (:|:), S (:->:), S (:=:)]
