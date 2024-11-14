module Primitive where

import Formatable (Formatable (format), Tree (Atom))

data Primitive
  = Null
  | Boolean Bool
  | Number Float
  | String String
  deriving (Eq, Show)

instance Formatable Primitive where
  format Null = Atom "#n"
  format (Boolean True) = Atom "#t"
  format (Boolean False) = Atom "#f"
  format (Number float) = Atom $ show float
  format (String characters) = Atom $ show characters
