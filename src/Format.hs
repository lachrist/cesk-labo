{-# LANGUAGE InstanceSigs #-}

module Format (Format (..), Tree (..), render) where

import Data.Map (Map, toAscList)

data Tree
  = Atom String
  | List [Tree]
  | Struct String [Tree]
  | Mapping [(String, Tree)]

newline :: Int -> String
newline level = '\n' : replicate (2 * level) ' '

renderItem :: Int -> Tree -> String
renderItem level tree = newline level ++ render level tree

renderPair :: Int -> (String, Tree) -> String
renderPair level (key, val) = newline level ++ key ++ " -> " ++ render level val

render :: Int -> Tree -> String
render _ (Atom leaf) =
  leaf
render _ (List []) =
  "[]"
render level (List [item]) =
  "[" ++ render level item ++ "]"
render level (List list) =
  "[" ++ concatMap (renderItem (level + 1)) list ++ "]"
render _ (Struct name []) =
  "(" ++ name ++ ")"
render level (Struct name [child]) =
  "(" ++ name ++ " " ++ render level child ++ ")"
render level (Struct name children) =
  "(" ++ name ++ concatMap (renderItem (level + 1)) children ++ ")"
render level (Mapping pairs) =
  "{" ++ concatMap (renderPair (level + 1)) pairs ++ "}"

class Format a where
  format :: a -> Tree

instance (Format a) => Format [a] where
  format :: (Format a) => [a] -> Tree
  format list = List $ map format list

formatBinding :: (Show k, Format v) => (k, v) -> (String, Tree)
formatBinding (key, val) = (show key, format val)

instance (Show k, Format v) => Format (Map k v) where
  format :: (Show k, Format v) => Map k v -> Tree
  format mapping = Mapping $ map formatBinding (toAscList mapping)
