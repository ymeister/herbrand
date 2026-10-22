module Herbrand.Propositionals.Conjunction
  ( (:&:) (..)
  , pattern (:&:)
  ) where

import Control.DeepSeq (NFData (..))
import Text.Read (Lexeme (Symbol), Read (..), lexP, parens, prec, readListPrecDefault, step)

import Herbrand.Eval (Eval (..))
import Herbrand.Term (Signature, Term (Term), type (:<))

--------------------------------------------------------------------------------
-- Conjunction
--------------------------------------------------------------------------------

-- | Example:
--   MkConj (MkVar "p") (MkVar "q")
data e1 :&: e2 = MkConj e1 e2
  deriving stock (Eq, Ord)

-- | Example:
--   Var "p" :&: Var "q" :: Term Propositionals
pattern (:&:) :: (Term s :&: Term s) :< Signature s => Term s -> Term s -> Term s
pattern e1 :&: e2 = Term (MkConj e1 e2)

infixr 3 :&:

instance (Show e1, Show e2) => Show (e1 :&: e2) where
  showsPrec d (MkConj e1 e2) =
    showParen (d > conjPrec) $ showsPrec leftPrec e1 . showString " & " . showsPrec conjPrec e2

instance (Read e1, Read e2) => Read (e1 :&: e2) where
  readPrec = parens $ prec conjPrec $ do
    e1 <- step readPrec
    Symbol "&" <- lexP
    MkConj e1 <$> readPrec
  readListPrec = readListPrecDefault

instance (NFData e1, NFData e2) => NFData (e1 :&: e2) where
  rnf (MkConj e1 e2) = rnf e1 `seq` rnf e2

instance (Eval ctx e1 Bool, Eval ctx e2 Bool) => Eval ctx (e1 :&: e2) Bool where
  {-# INLINABLE eval #-}
  eval ctx (MkConj e1 e2) = and [eval ctx e1, eval ctx e2]

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

conjPrec :: Int
conjPrec = 3

leftPrec :: Int
leftPrec = conjPrec + 1
