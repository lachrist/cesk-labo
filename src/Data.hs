{-# LANGUAGE InstanceSigs #-}

module Data where

import Environment (Environment)
import Expression (Expression, Variable)
import Format (Format (format), Tree (Atom, List, Struct))
import Primitive (Primitive)

type Builtin = String

builtins :: [Builtin]
builtins =
  [ -- applyBuiltin
    "apply",
    "read-line",
    "print",
    -- applyPure
    "begin",
    "eq?",
    "inspect",
    "cons",
    "list",
    "car",
    "cdr",
    "set-car!",
    "set-cdr!",
    -- applyData
    "raise",
    "equal?",
    "any->string",
    "null?",
    "boolean?",
    "number?",
    "string?",
    "pair?",
    "procedure?",
    -- applyAtomic
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
  | Builtin Builtin
  | Cons v v
  | Closure (Environment v) [Variable] Expression

instance (Format v) => Format (Data v) where
  format :: (Format v) => Data v -> Tree
  format (Primitive primitive) = format primitive
  format (Builtin name) = Atom $ '#' : name
  format (Cons car cdr) =
    Struct "cons" [format car, format cdr]
  format (Closure env params body) =
    Struct "closure" [format env, List $ map Atom params, format body]
