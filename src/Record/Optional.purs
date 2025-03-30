module Record.Optional
  ( Optional
  , type (?)
  , WithDefaults
  , type (-?->)
  ) where

import Prelude

import Prim.Row as R
import Prim.RowList as RL

data Optional :: (Type -> Type) -> Type -> Type
data Optional f a

type WithDefaults a b = forall r. AdequatelySpecifies r a => r -> b

infix 8 type Optional as ?
infixr 4 type WithDefaults as -?->

class AdequatelySpecifies x y

class CompatibleArgument x y

instance CompatibleArgument (Optional f a) (f a) else
instance CompatibleArgument a a
