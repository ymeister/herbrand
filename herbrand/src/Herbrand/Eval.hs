{-# LANGUAGE UndecidableInstances #-}

module Herbrand.Eval
  ( Eval (..)
  , EvalIn
  ) where

import Data.Kind (Constraint, Type)
import Data.Variant (ReduceVariant, V, reduceVariant)

--------------------------------------------------------------------------------
-- Evaluation
--------------------------------------------------------------------------------

-- | Interprets @x@ as a @y@ under a context. One instance per symbol, with
-- each child's instance as a constraint, so a symbol recurses by hand.
--
-- Example:
--   instance (Eval ctx e1 Bool, Eval ctx e2 Bool) => Eval ctx (e1 :&: e2) Bool where
--     eval ctx (e1 :&: e2) = and [eval ctx e1, eval ctx e2]
type Eval :: Type -> Type -> Type -> Constraint
class Eval ctx x y where
  eval :: ctx -> x -> y

instance {-# OVERLAPPABLE #-} Eval ctx x x where
  {-# INLINABLE eval #-}
  eval _ = id

instance {-# OVERLAPPABLE #-} Eval ctx x (Maybe x) where
  {-# INLINABLE eval #-}
  eval _ = Just

instance ReduceVariant (EvalIn ctx y) xs => Eval ctx (V xs) y where
  {-# INLINABLE eval #-}
  eval ctx = reduceVariant @(EvalIn ctx y) (eval ctx)

type EvalIn :: Type -> Type -> Type -> Constraint
class Eval ctx x y => EvalIn ctx y x
instance Eval ctx x y => EvalIn ctx y x
