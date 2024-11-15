{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Serial (Serializable (serialize), Serial (..), render) where

import Data.Map (Map, toAscList)

type Tag = String

data Serial
  = NullLeaf
  | BooleanLeaf Bool
  | IntegerLeaf Int
  | NumberLeaf Double
  | StringLeaf String
  | SymbolLeaf String
  | ArrayNode [Serial]
  | StructureNode Tag [Serial]
  | MappingNode [(String, Serial)]
  deriving (Eq, Show)

type Level = Int

escape :: Char -> String
escape '"' = "\\\""
escape '\\' = "\\\\"
escape '\b' = "\\b"
escape '\f' = "\\f"
escape '\n' = "\\n"
escape '\r' = "\\r"
escape '\t' = "\\t"
escape c = [c]

newline :: Level -> String
newline level = '\n' : replicate (2 * level) ' '

renderItem :: Level -> Serial -> String
renderItem level tree = newline level ++ render level tree

renderPair :: Level -> (String, Serial) -> String
renderPair level (key, val) = newline level ++ key ++ " -> " ++ render level val

cutoff :: Int
cutoff = 4

isLeaf :: Serial -> Bool
isLeaf NullLeaf = True
isLeaf (BooleanLeaf _) = True
isLeaf (IntegerLeaf _) = True
isLeaf (NumberLeaf _) = True
isLeaf (StringLeaf _) = True
isLeaf (SymbolLeaf _) = True
isLeaf _ = False

render :: Level -> Serial -> String
render _ NullLeaf = "#n"
render _ (BooleanLeaf True) = "#t"
render _ (BooleanLeaf False) = "#f"
render _ (IntegerLeaf leaf) = show leaf
render _ (NumberLeaf leaf) = show leaf
render _ (StringLeaf leaf) = "\"" ++ concatMap escape leaf ++ "\""
render _ (SymbolLeaf leaf) = leaf
render level (ArrayNode children) =
  if all isLeaf children && length children <= cutoff
    then "[" ++ unwords (map (render 0) children) ++ "]"
    else "[" ++ concatMap (renderItem (level + 1)) children ++ "]"
render level (StructureNode name children) =
  if all isLeaf children && length children <= cutoff
    then "(" ++ unwords (name : map (render 0) children) ++ ")"
    else "(" ++ name ++ concatMap (renderItem (level + 1)) children ++ ")"
render level (MappingNode pairs) =
  "{" ++ concatMap (renderPair (level + 1)) pairs ++ "}"

class Serializable a where
  serialize :: a -> Serial

instance (Serializable a) => Serializable [a] where
  serialize = ArrayNode . map serialize

serializeBinding :: (Show k, Serializable v) => (k, v) -> (String, Serial)
serializeBinding (key, val) = (show key, serialize val)

instance (Show k, Serializable v) => Serializable (Map k v) where
  serialize = MappingNode . map serializeBinding . toAscList
