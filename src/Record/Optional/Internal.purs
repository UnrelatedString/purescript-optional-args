module Record.Optional.Internal
  ( Optional
  , type (?)
  , WithDefaults
  , type (-?->)
  , withDefaults
  , class AdequatelySpecifies
  , specify
  , class FullySpecified
  , class FullySpecified'
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

withDefaults :: forall spec a b. FullySpecified spec a => (Record a -> b) -> spec -?-> b
withDefaults f s = f $ specify s

-- | Allows a record spec with `Optional`s to match real arguments.
-- | Record specs are given as `Row`s (in parentheses) rather than `Record`s (in braces).
class AdequatelySpecifies :: Type -> Row Type -> Constraint
class AdequatelySpecifies arg spec where
  specify ::
    forall r.
    FullySpecified spec r =>
    arg -> Record r

class FullySpecified spec r
instance (RL.RowToList spec list, FullySpecified' list r) => FullySpecified spec r

-- | Removes all `Optional`s from a spec, given as a RowList.
class FullySpecified' :: RL.RowList Type -> Row Type -> Constraint
class FullySpecified' list r

instance (RL.RowToList r list) => FullySpecified' list r else
instance
  ( R.Cons sym rtype rtail r
  , CompatibleArgument ltype rtype
  , FullySpecified' ltail rtail
  ) =>
  FullySpecified' (RL.Cons sym ltype ltail) r

-- | Constrains two types to be the same or the first to be an Optional of the second.
class CompatibleArgument :: Type -> Type -> Constraint
class CompatibleArgument x y | x -> y

-- | All cases are covered by this instance chain;
-- | it is impossible to define additional behavior.
instance CompatibleArgument (Optional f a) (f a) else
instance CompatibleArgument a a else
-- | The final and most general case forces a
-- | hopefully somewhat readable error.
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
