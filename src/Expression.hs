{-# LANGUAGE InstanceSigs #-}

module Expression where

import Format (Format (format), Tree (Atom, Struct))
import Primitive (Primitive)

type Variable = String

data Expression
  = Literal Primitive
  | Variable Variable
  | Condition Expression Expression Expression
  | Binding Variable Expression Expression
  | Lambda [Variable] Expression
  | Application Expression [Expression]
  deriving (Eq, Show)

instance Format Expression where
  format :: Expression -> Tree
  format (Literal primitive) = format primitive
  format (Variable variable) = Atom variable
  format (Condition test consequent alternate) =
    Struct "if" [format test, format consequent, format alternate]
  format (Binding variable right body) =
    Struct "let" [Atom variable, format right, format body]
  format (Lambda parameters body) =
    Struct "let" [Atom "lambda", Struct "" $ map Atom parameters, format body]
  format (Application callee arguments) =
    Struct "" $ format callee : map format arguments
