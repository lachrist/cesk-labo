{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Formatable (Formatable (format), Tree (..), render) where

import Data.Foldable (toList)
import Data.Map (Map, toAscList)
import Data.Sequence (Seq)

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

class Formatable a where
  format :: a -> Tree

instance (Formatable a) => Formatable [a] where
  format = List . map format

instance (Formatable a) => Formatable (Seq a) where
  format = List . map format . Data.Foldable.toList

instance (Formatable a, Formatable b) => Formatable (Either a b) where
  format (Left x) = Struct "failure" [format x]
  format (Right y) = Struct "success" [format y]

instance (Formatable a, Formatable b) => Formatable (a, b) where
  format (x, y) = Struct "tuple" [format x, format y]

formatBinding :: (Show k, Formatable v) => (k, v) -> (String, Tree)
formatBinding (key, val) = (show key, format val)

instance (Show k, Formatable v) => Formatable (Map k v) where
  format mapping = Mapping $ map formatBinding (toAscList mapping)
