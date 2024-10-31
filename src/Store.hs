{-# LANGUAGE InstanceSigs #-}

module Store where

import Data.Map (Map, toAscList)
import Data.Word (Word64)
import Format (Format (..), indent)

type Address = Word64

newtype Store x = Store (Map Address x)

formatBinding :: (Format x) => Int -> (Address, x) -> String
formatBinding level (key, val) = indent level ++ show key ++ " -> " ++ format level val

instance (Format x) => Format (Store x) where
  format :: (Format x) => Int -> Store x -> String
  format level (Store mapping) =
    "["
      ++ concatMap (formatBinding (level + 1)) (Data.Map.toAscList mapping)
      ++ "]"
