{-# LANGUAGE InstanceSigs #-}

module Error where

import Expression (Location, Variable)
import Format (Format (format), Tree (Atom, Struct))

type Message = String

data Error v
  = ApplyError ApplyErrorCause v [v] Location
  | MissingError Variable Location

data ApplyErrorCause
  = BuiltinFailure Message
  | ArityMismatch Int
  | CannotApply

instance Format ApplyErrorCause where
  format :: ApplyErrorCause -> Tree
  format (BuiltinFailure message) = Atom message
  format (ArityMismatch arity) = Atom $ "expected " ++ show arity ++ " arguments"
  format CannotApply = Atom "cannot apply callee"

instance (Format v) => Format (Error v) where
  format :: (Format v) => Error v -> Tree
  format (ApplyError cause callee arguments location) =
    Struct
      "apply-error"
      [format cause, format callee, format arguments, format location]
  format (MissingError variable location) =
    Struct "missing-error" [Atom variable, format location]
