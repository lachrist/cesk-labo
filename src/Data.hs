{-# LANGUAGE InstanceSigs #-}

module Data where

import Environment (Environment)
import Expression (Expression, Variable)
import Format (Format (format), Tree (Atom, List, Struct))
import Primitive (Primitive (Boolean))

type BuiltinName = String

builtins :: [BuiltinName]
builtins =
  [ -- reflect --
    "apply",
    -- action --
    "read-line",
    "print",
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
  deriving (Eq, Show)

instance (Format v) => Format (Data v) where
  format :: (Format v) => Data v -> Tree
  format (Primitive primitive) = format primitive
  format (Builtin name) = Atom $ '#' : name
  format (Cons car cdr) =
    Struct "cons" [format car, format cdr]
  format (Closure env params body) =
    Struct "closure" [format env, List $ map Atom params, format body]

isTruthy :: Data v -> Bool
isTruthy (Primitive (Boolean bool)) = bool
isTruthy _ = True