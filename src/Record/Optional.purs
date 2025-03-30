module Record.Optional
  ( Optional
  , type (?)
  , WithDefaults
  , type (-?->)
  ) where

data Optional
data WithDefaults

infix 8 type Optional as ?
infixr 4 type WithDefaults as -?->
