{-# LANGUAGE InstanceSigs #-}

module Primitive where

import Format (Format (format), Tree (Atom))

data Primitive
  = Null
  | Boolean Bool
  | Number Float
  | String String
  deriving (Eq, Show)

instance Format Primitive where
  format :: Primitive -> Tree
  format Null = Atom "#n"
  format (Boolean True) = Atom "#t"
  format (Boolean False) = Atom "#f"
  format (Number float) = Atom $ show float
  format (String characters) = Atom $ show characters
