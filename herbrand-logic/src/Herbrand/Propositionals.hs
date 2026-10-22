module Herbrand.Propositionals
  ( module Herbrand.Propositionals.Variable
  , module Herbrand.Propositionals.Negation
  , module Herbrand.Propositionals.Conjunction
  , module Herbrand.Propositionals.Disjunction
  , module Herbrand.Propositionals.Implication
  , module Herbrand.Propositionals.Equivalence
  , module Herbrand.Propositionals.Literal
  , module Herbrand.Propositionals.Clause
  , module Herbrand.Propositionals.DNF
  , module Herbrand.Propositionals.Disjunct
  , module Herbrand.Propositionals.CNF
  , Propositionals
  ) where

import Data.Kind (Type)

import Herbrand.Propositionals.CNF
import Herbrand.Propositionals.Clause
import Herbrand.Propositionals.Conjunction
import Herbrand.Propositionals.DNF
import Herbrand.Propositionals.Disjunct
import Herbrand.Propositionals.Disjunction
import Herbrand.Propositionals.Equivalence
import Herbrand.Propositionals.Implication
import Herbrand.Propositionals.Literal
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
