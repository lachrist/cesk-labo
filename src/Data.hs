{-# LANGUAGE InstanceSigs #-}

module Data where

import Environment (Environment)
import Expression (Expression, Variable)
import Format (Format (..), formatOne)
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

data Data value
  = Primitive Primitive
  | Builtin Builtin
  | Cons value value
  | Closure (Environment value) [Variable] Expression

instance (Format v) => Format (Data v) where
  format :: (Format v) => Int -> Data v -> String
  format level (Primitive literal) = format level literal
  format _ (Builtin name) = "<#" ++ name ++ ">"
  format level (Cons car cdr) =
    "(cons"
      ++ formatOne level car
      ++ formatOne level cdr
      ++ ")"
  format level (Closure environment parameters body) =
    "(closure ("
      ++ unwords parameters
      ++ ") "
      ++ formatOne level environment
      ++ formatOne level body
      ++ ")"
