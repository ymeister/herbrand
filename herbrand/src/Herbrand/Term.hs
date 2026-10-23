{-# LANGUAGE UndecidableInstances #-}

module Herbrand.Term
  ( Term (Term)
  , Signature
  , type (:<)
  , ReduceVariant
  , reduceTerm
  , ReadV (..)
  , Saturated
  , Saturate
  , S
  ) where

import Control.DeepSeq (NFData)
import Data.Kind (Constraint, Type)
import Data.Variant (ReduceVariant, V, pattern V, reduceVariant, toVariantHead, toVariantTail, type (:<))
import Text.Read (Read (..), ReadPrec, pfail, readListPrecDefault, (+++))

import Herbrand.Eval (Eval (..))

--------------------------------------------------------------------------------
-- Terms
--------------------------------------------------------------------------------

-- | A term over the symbols @s@, with the term itself in every hole of every
-- symbol.
--
-- Example:
--   Term (MkConj (Term (MkVar "p")) (Term (MkNeg (Term (MkVar "q"))))) :: Term '[S Var, S Neg, S (:&:)]
type Term :: [Type] -> Type
newtype Term s = MkTerm (V (Signature s))

pattern Term :: e :< Signature s => e -> Term s
pattern Term e = MkTerm (V e)

deriving newtype instance Show (V (Signature s)) => Show (Term s)
deriving newtype instance Eq (V (Signature s)) => Eq (Term s)
deriving newtype instance Ord (V (Signature s)) => Ord (Term s)
deriving newtype instance NFData (V (Signature s)) => NFData (Term s)

instance ReadV (Signature s) => Read (Term s) where
  readPrec = MkTerm <$> readPrecV
  readListPrec = readListPrecDefault

instance Eval ctx (V (Signature s)) y => Eval ctx (Term s) y where
  {-# INLINABLE eval #-}
  eval ctx (MkTerm v) = eval ctx v

-- | The symbols of @s@ with the term substituted into every hole.
--
-- Example:
--   Signature '[S Var, S (:&:)] => '[Var, Term '[S Var, S (:&:)] :&: Term '[S Var, S (:&:)]]
type Signature :: [Type] -> [Type]
type Signature s = Saturated s (Term s)

--------------------------------------------------------------------------------
-- Reading
--------------------------------------------------------------------------------

-- | 'Read' for a variant, trying each type of the list in turn.
type ReadV :: [Type] -> Constraint
class ReadV xs where
  readPrecV :: ReadPrec (V xs)

instance ReadV '[] where
  readPrecV = pfail

instance (Read x, ReadV xs) => ReadV (x : xs) where
  readPrecV = (toVariantHead <$> readPrec) +++ (toVariantTail <$> readPrecV)

--------------------------------------------------------------------------------
-- Saturation
--------------------------------------------------------------------------------

-- Example:
--   Saturated '[S Var, S (:&:)] t => '[Var, t :&: t]
--   Saturated '[] t => '[]
type Saturated :: [Type] -> Type -> [Type]
type family Saturated s x where
  Saturated '[] x = '[]
  Saturated (S f : fs) x = Saturate f x : Saturated fs x

-- Example:
--   Saturate (:&:) t => t :&: t
--   Saturate ((:&:) a) t => a :&: t
--   Saturate Var t => Var
type Saturate :: k -> Type -> Type
type family Saturate f x where
  Saturate (f :: Type) x = f
  Saturate (f :: Type -> k) x = Saturate (f x) x

-- | Marks a symbol in a symbol list, whatever the symbol's kind.
--
-- Example:
--   '[S Var, S (:&:), S ((:|:) (Clause s))] :: [Type]
type S :: k -> Type
data S f

--------------------------------------------------------------------------------
-- Folding
--------------------------------------------------------------------------------

-- | Applies one function, polymorphic over the constraint @c@, to whichever
-- symbol the term holds.
--
-- Example:
--   reduceTerm @Show show (Var "p" :&: Var "q" :: Term Propositionals) => "p & q"
reduceTerm
  :: forall c s r
   . ReduceVariant c (Signature s)
  => (forall x. c x => x -> r)
  -> Term s
  -> r
reduceTerm f (MkTerm v) = reduceVariant @c f v
