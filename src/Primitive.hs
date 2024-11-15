module Primitive where

import Serial (Serial (BooleanLeaf, NullLeaf, NumberLeaf, StringLeaf), Serializable (serialize))

data Primitive
  = Null
  | Boolean Bool
  | Number Double
  | String String
  deriving (Eq, Show)

instance Serializable Primitive where
  serialize Null = NullLeaf
  serialize (Boolean bool) = BooleanLeaf bool
  serialize (Number double) = NumberLeaf double
  serialize (String characters) = StringLeaf characters
