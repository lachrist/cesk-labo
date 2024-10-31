{-# LANGUAGE InstanceSigs #-}

module Expression where

import Format (Format (format), Tree (Atom, Struct))
import Primitive (Primitive)

type Variable = String

data Location
  = Location String Int Int
  deriving (Eq, Show)

data Expression
  = Literal Primitive Location
  | Variable Variable Location
  | Condition Expression Expression Expression Location
  | Binding Variable Expression Expression Location
  | Lambda [Variable] Expression Location
  | Application Expression [Expression] Location
  deriving (Eq, Show)

printLocation :: Location -> String
printLocation (Location name line column) =
  name ++ ":" ++ show line ++ ":" ++ show column

instance Format Location where
  format :: Location -> Tree
  format = Atom . printLocation

instance Format Expression where
  format :: Expression -> Tree
  format (Literal primitive _) = format primitive
  format (Variable variable _) = Atom variable
  format (Condition test consequent alternate _) =
    Struct "if" [format test, format consequent, format alternate]
  format (Binding variable right body _) =
    Struct "let" [Atom variable, format right, format body]
  format (Lambda parameters body _) =
    Struct "let" [Atom "lambda", Struct "" $ map Atom parameters, format body]
  format (Application callee arguments _) =
    Struct "" $ format callee : map format arguments
