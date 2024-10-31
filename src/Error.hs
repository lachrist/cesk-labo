{-# LANGUAGE InstanceSigs #-}

module Error where

import Data (Data)
import Expression (Location, Variable)
import Format (Format (format), Tree (Atom, Struct))

type Message = String

data Error v
  = ApplyError Message (Data v) [v] Location
  | MissingError Variable Location

instance (Format v) => Format (Error v) where
  format :: (Format v) => Error v -> Tree
  format (ApplyError cause callee arguments location) =
    Struct
      "apply-error"
      [Atom cause, format callee, format arguments, format location]
  format (MissingError variable location) =
    Struct "missing-error" [Atom variable, format location]
