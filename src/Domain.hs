module Domain where

import Environment (Environment)
import Expression (Expression, Variable)
import Primitive (Primitive (Boolean))
import Serial (Serial (ArrayNode, StructureNode, SymbolLeaf), Serializable (serialize))

type BuiltinName = String

builtins :: [BuiltinName]
builtins =
  [ -- reflect --
    "apply",
    -- action --
    "trace",
    "read-line",
    "display",
    -- value --
    "begin",
    "eq?",
    "inspect",
    "cons",
    "list",
    "car",
    "cdr",
    "set-car!",
    "set-cdr!",
    -- data --
    "raise",
    "equal?",
    "any->string",
    "null?",
    "boolean?",
    "number?",
    "string?",
    "pair?",
    "procedure?",
    "||",
    "&&",
    "=",
    "!=",
    "<=",
    "+",
    "-",
    "*",
    "/",
    "expt",
    "sqrt",
    "number->string",
    "string->number",
    "string-length",
    "string-append",
    "substring"
  ]

data Domain v
  = Primitive Primitive
  | Builtin BuiltinName
  | Pair v v
  | Closure (Environment v) [Variable] Expression
  deriving (Eq, Show)

instance (Serializable v) => Serializable (Domain v) where
  serialize (Primitive primitive) = serialize primitive
  serialize (Builtin name) = SymbolLeaf $ '#' : name
  serialize (Pair first second) =
    StructureNode "pair" [serialize first, serialize second]
  serialize (Closure env params body) =
    StructureNode "pair" [serialize env, ArrayNode $ map SymbolLeaf params, serialize body]

isTruthy :: Domain v -> Bool
isTruthy (Primitive (Boolean bool)) = bool
isTruthy _ = True