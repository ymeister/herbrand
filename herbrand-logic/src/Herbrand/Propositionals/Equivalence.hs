module Herbrand.Propositionals.Equivalence
  ( (:=:) (..)
  , pattern (:=:)
  ) where

import Control.DeepSeq (NFData (..))
import Text.Read (Lexeme (Punc), Read (..), lexP, parens, prec, readListPrecDefault, step)

import Herbrand.Eval (Eval (..))
import Herbrand.Term (Signature, Term (Term), type (:<))

--------------------------------------------------------------------------------
-- Equivalence
--------------------------------------------------------------------------------

-- | Example:
--   MkEquiv (MkVar "p") (MkVar "q")
data e1 :=: e2 = MkEquiv e1 e2
  deriving stock (Eq, Ord)

-- | Example:
--   Var "p" :=: Var "q" :: Term Propositionals
pattern (:=:) :: (Term s :=: Term s) :< Signature s => Term s -> Term s -> Term s
pattern e1 :=: e2 = Term (MkEquiv e1 e2)

infixr 0 :=:

instance (Show e1, Show e2) => Show (e1 :=: e2) where
  showsPrec d (MkEquiv e1 e2) =
    showParen (d > equivPrec) $ showsPrec leftPrec e1 . showString " = " . showsPrec equivPrec e2

instance (Read e1, Read e2) => Read (e1 :=: e2) where
  readPrec = parens $ prec equivPrec $ do
    e1 <- step readPrec
    Punc "=" <- lexP
    MkEquiv e1 <$> readPrec
  readListPrec = readListPrecDefault

instance (NFData e1, NFData e2) => NFData (e1 :=: e2) where
  rnf (MkEquiv e1 e2) = rnf e1 `seq` rnf e2

instance (Eval ctx e1 Bool, Eval ctx e2 Bool) => Eval ctx (e1 :=: e2) Bool where
  {-# INLINABLE eval #-}
  eval ctx (MkEquiv e1 e2) = (eval ctx e1 :: Bool) == (eval ctx e2 :: Bool)

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

equivPrec :: Int
equivPrec = 0

leftPrec :: Int
leftPrec = equivPrec + 1
