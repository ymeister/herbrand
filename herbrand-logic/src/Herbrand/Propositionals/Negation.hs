module Herbrand.Propositionals.Negation
  ( Neg (..)
  , pattern Neg
  ) where

import Control.DeepSeq (NFData)
import Text.Read (Lexeme (Punc), Read (..), lexP, parens, prec, readListPrecDefault, step)

import Herbrand.Eval (Eval (..))
import Herbrand.Term (Signature, Term (Term), type (:<))

--------------------------------------------------------------------------------
-- Negation
--------------------------------------------------------------------------------

-- | Example:
--   MkNeg (MkVar "p")
newtype Neg e = MkNeg e
  deriving newtype (Eq, Ord, NFData)

-- | Example:
--   Neg (Var "p") :: Term Propositionals
pattern Neg :: Neg (Term s) :< Signature s => Term s -> Term s
pattern Neg e = Term (MkNeg e)

instance Show e => Show (Neg e) where
  showsPrec d (MkNeg e) = showParen (d > negPrec) $ showChar '~' . showsPrec childPrec e

instance Read e => Read (Neg e) where
  readPrec = parens $ prec negPrec $ do
    Punc "~" <- lexP
    MkNeg <$> step readPrec
  readListPrec = readListPrecDefault

instance Eval ctx e Bool => Eval ctx (Neg e) Bool where
  {-# INLINABLE eval #-}
  eval ctx (MkNeg e) = not $ eval ctx e

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

negPrec :: Int
negPrec = 4

childPrec :: Int
childPrec = negPrec + 1
