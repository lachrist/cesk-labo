{-# LANGUAGE InstanceSigs #-}

module Expression where

import Format (Format (..), formatAll, formatOne)
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
  format :: Int -> Expression -> String
  format level (Literal literal) = format level literal
  format _ (Variable variable) = variable
  format level (Condition test consequent alternate) =
    "(if"
      ++ formatOne level test
      ++ formatOne level consequent
      ++ formatOne level alternate
      ++ ")"
  format level (Binding variable right body) =
    "(let "
      ++ variable
      ++ formatOne level right
      ++ formatOne level body
      ++ ")"
  format level (Lambda parameters body) =
    "(lambda ("
      ++ unwords parameters
      ++ ")"
      ++ formatOne level body
      ++ ")"
  format level (Application callee arguments) =
    "("
      ++ formatAll level (callee : arguments)
      ++ ")"
