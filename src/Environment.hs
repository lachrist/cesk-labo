{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Environment where

import Data.Map (Map, toAscList)
import Expression (Variable)
import Format (Format (..), indent)

newtype Environment v = Environment (Map Variable v)

formatBinding :: (Format v) => Int -> (Variable, v) -> String
formatBinding level (key, val) = indent level ++ key ++ ": " ++ format level val

instance (Format v) => Format (Environment v) where
  format :: (Format v) => Int -> Environment v -> String
  format level (Environment env) =
    "{"
      ++ concatMap (formatBinding (level + 1)) (Data.Map.toAscList env)
      ++ "}"