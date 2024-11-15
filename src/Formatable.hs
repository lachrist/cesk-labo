{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Formatable (Formatable (format), Tree (..), render) where

import Data.Map (Map, toAscList)

data Tree
  = Atom String
  | List [Tree]
  | Struct String [Tree]
  | Mapping [(String, Tree)]

type Level = Maybe Int

newline :: Level -> String
newline Nothing = " "
newline (Just indent) = '\n' : replicate (2 * indent) ' '

renderItem :: Level -> Tree -> String
renderItem level tree = newline level ++ render level tree

renderPair :: Level -> (String, Tree) -> String
renderPair level (key, val) = newline level ++ key ++ " -> " ++ render level val

render :: Level -> Tree -> String
render _ (Atom leaf) =
  leaf
render _ (List []) =
  "[]"
render level (List [item]) =
  "[" ++ render level item ++ "]"
render level (List list) =
  "[" ++ concatMap (renderItem (fmap (+ 1) level)) list ++ "]"
render _ (Struct name []) =
  "(" ++ name ++ ")"
render level (Struct name [child]) =
  "(" ++ name ++ (if name == "" then "" else " ") ++ render level child ++ ")"
render level (Struct name children) =
  "(" ++ name ++ concatMap (renderItem (fmap (+ 1) level)) children ++ ")"
render level (Mapping pairs) =
  "{" ++ concatMap (renderPair (fmap (+ 1) level)) pairs ++ "}"

class Formatable a where
  format :: a -> Tree

instance (Formatable a) => Formatable [a] where
  format = List . map format

instance (Formatable a, Formatable b) => Formatable (Either a b) where
  format (Left x) = Struct "failure" [format x]
  format (Right y) = Struct "success" [format y]

instance (Formatable a, Formatable b) => Formatable (a, b) where
  format (x, y) = Struct "tuple" [format x, format y]

formatBinding :: (Show k, Formatable v) => (k, v) -> (String, Tree)
formatBinding (key, val) = (show key, format val)

instance (Show k, Formatable v) => Formatable (Map k v) where
  format = Mapping . map formatBinding . toAscList
