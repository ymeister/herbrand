module Herbrand.Propositionals.Variable
  ( Var (..)
  , pattern Var
  ) where

import Control.DeepSeq (NFData)
import Data.Char (isLower)
import Data.Foldable (toList)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Text.Read (Lexeme (Ident), Read (..), lexP, parens, pfail, readListPrecDefault)

import Herbrand.Eval (Eval (..))
import Herbrand.Term (Signature, Term (Term), type (:<))

--------------------------------------------------------------------------------
-- Variable
--------------------------------------------------------------------------------

-- | Example:
--   MkVar "p"
newtype Var = MkVar Text
  deriving newtype (Eq, Ord, NFData)

-- | Example:
--   Var "p" :: Term Propositionals
pattern Var :: Var :< Signature s => Text -> Term s
pattern Var v = Term (MkVar v)

instance Show Var where
  showsPrec _ (MkVar v) = showString $ Text.unpack v

instance Read Var where
  readPrec = parens $ do
    Ident v@(h : _) <- lexP
    if isLower h
      then pure $ MkVar $ Text.pack v
      else pfail
  readListPrec = readListPrecDefault

instance Foldable t => Eval (t (Var, Bool)) Var Bool where
  {-# INLINABLE eval #-}
  eval ctx v = fromMaybe False $ lookup v $ toList ctx

instance Foldable t => Eval (t (Var, dom)) Var (Maybe dom) where
  {-# INLINABLE eval #-}
  eval ctx v = lookup v $ toList ctx

instance Eval (Map.Map Var Bool) Var Bool where
  {-# INLINABLE eval #-}
  eval ctx v = fromMaybe False $ Map.lookup v ctx

instance Eval (Map.Map Var dom) Var (Maybe dom) where
  {-# INLINABLE eval #-}
  eval ctx v = Map.lookup v ctx
