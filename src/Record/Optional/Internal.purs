module Record.Optional
  ( Optional
  , type (?)
  , WithDefaults
  , type (-?->)
  , class AdequatelySpecifies
  , specify
  , class FullySpecified
  , class CompatibleArgument
  ) where

import Prelude

import Prim.Row as R
import Prim.RowList as RL
import Prim.TypeError as TE

infixr 8 type TE.Beside as |!

data Optional :: (Type -> Type) -> Type -> Type
data Optional f a

type WithDefaults a b = forall r. AdequatelySpecifies r a => r -> b

infix 8 type Optional as ?
infixr 4 type WithDefaults as -?->

-- | Allows a record spec with `Optional`s to match real arguments.
-- | Record specs are given as `Row`s (in parentheses) rather than `Record`s (in braces).
class AdequatelySpecifies arg spec where
  specify :: forall r. FullySpecified r spec => arg -> Record r

class FullySpecified r spec

-- | Constrains two types to be the same or the first to be an Optional of the second.
class CompatibleArgument x y | x -> y

instance CompatibleArgument (Optional f a) (f a) else
instance CompatibleArgument a a else
instance TE.Fail
  ( TE.Text "Expected: "
  |!
    TE.Quote a
  |!
    TE.Text ", got: "
  |!
    TE.Quote b
  |!
    TE.Text " in a record spec with optional arguments"
  ) => CompatibleArgument a b
