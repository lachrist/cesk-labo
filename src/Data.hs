module Data where

import Environment (Environment)
import Expression (Expression, Variable)
import Formatable (Formatable (format), Tree (Atom, List, Struct))
import Primitive (Primitive (Boolean))

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

data Data v
  = Primitive Primitive
  | Builtin BuiltinName
  | Cons v v
  | Closure (Environment v) [Variable] Expression
  deriving (Eq)

instance (Show v) => Show (Data v) where
  show (Primitive primitive) = show primitive
  show (Builtin name) = '#' : name
  show (Cons car cdr) = "(" ++ show car ++ " . " ++ show cdr ++ ")"
  show (Closure _ params body) = "(lambda (" ++ unwords params ++ ") " ++ show body ++ ")"

instance (Formatable v) => Formatable (Data v) where
  format (Primitive primitive) = format primitive
  format (Builtin name) = Atom $ '#' : name
  format (Cons car cdr) =
    Struct "cons" [format car, format cdr]
  format (Closure env params body) =
    Struct "closure" [format env, List $ map Atom params, format body]

isTruthy :: Data v -> Bool
isTruthy (Primitive (Boolean bool)) = bool
isTruthy _ = True