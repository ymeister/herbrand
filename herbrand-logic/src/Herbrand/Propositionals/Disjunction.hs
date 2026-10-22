module Herbrand.Propositionals.Disjunction
  ( (:|:) (..)
  , pattern (:|:)
  ) where

import Control.DeepSeq (NFData (..))
import Text.Read (Lexeme (Punc), Read (..), lexP, parens, prec, readListPrecDefault, step)

import Herbrand.Eval (Eval (..))
import Herbrand.Term (Signature, Term (Term), type (:<))

--------------------------------------------------------------------------------
-- Disjunction
--------------------------------------------------------------------------------

-- | Example:
--   MkDisj (MkVar "p") (MkVar "q")
data e1 :|: e2 = MkDisj e1 e2
  deriving stock (Eq, Ord)

-- | Example:
--   Var "p" :|: Var "q" :: Term Propositionals
pattern (:|:) :: (Term s :|: Term s) :< Signature s => Term s -> Term s -> Term s
pattern e1 :|: e2 = Term (MkDisj e1 e2)

infixr 2 :|:

instance (Show e1, Show e2) => Show (e1 :|: e2) where
  showsPrec d (MkDisj e1 e2) =
    showParen (d > disjPrec) $ showsPrec leftPrec e1 . showString " | " . showsPrec disjPrec e2

instance (Read e1, Read e2) => Read (e1 :|: e2) where
  readPrec = parens $ prec disjPrec $ do
    e1 <- step readPrec
    Punc "|" <- lexP
    MkDisj e1 <$> readPrec
  readListPrec = readListPrecDefault

instance (NFData e1, NFData e2) => NFData (e1 :|: e2) where
  rnf (MkDisj e1 e2) = rnf e1 `seq` rnf e2

instance (Eval ctx e1 Bool, Eval ctx e2 Bool) => Eval ctx (e1 :|: e2) Bool where
  {-# INLINABLE eval #-}
  eval ctx (MkDisj e1 e2) = or [eval ctx e1, eval ctx e2]

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

disjPrec :: Int
disjPrec = 2

leftPrec :: Int
leftPrec = disjPrec + 1
