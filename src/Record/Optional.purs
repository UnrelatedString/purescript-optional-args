module Record.Optional
  ( Optional
  , type (?)
  , WithDefaults
  , type (-?->)
  , class AdequatelySpecifies
  , class CompatibleArgument
  ) where

import Prelude

import Prim.Row as R
import Prim.RowList as RL
import Prim.TypeError as TE

data Optional :: (Type -> Type) -> Type -> Type
data Optional f a

type WithDefaults a b = forall r. AdequatelySpecifies r a => r -> b

infix 8 type Optional as ?
infixr 4 type WithDefaults as -?->

-- | 
class AdequatelySpecifies x y

-- | Constrains two types to be the same or the first to be an Optional of the second.
class CompatibleArgument x y

instance CompatibleArgument (Optional f a) (f a) else
instance CompatibleArgument a a else
instance TE.Fail
  ( TE.Text "Expected: "
  `TE.Beside`
    TE.Quote a
  `TE.Beside`
    TE.Text ", got: "
  `TE.Beside`
    TE.Quote b
  `TE.Beside`
    TE.Text "in a record spec with optional arguments"
  ) => CompatibleArgument a b
