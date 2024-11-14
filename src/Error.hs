module Error where

import Data (BuiltinName, Data)
import Expression (Location, Variable)
import Formatable (Formatable (format), Tree (Atom, Struct))
import Parse (toLocation)
import Text.Parsec (ParseError, errorPos)

type Message = String

data Error v
  = BuiltinApplicationError Message BuiltinName [v] Location
  | ClosureApplicationError Message (Data v) [v] Location
  | RaiseError Message Location
  | MissingVariableError Variable Location
  | ParsingError Message Location
  deriving (Eq, Show)

data ErrorName
  = BuiltinApplicationErrorName
  | ClosureApplicationErrorName
  | RaiseErrorName
  | MissingVariableErrorName
  | ParsingErrorName
  deriving (Eq, Show)

getErrorName :: Error v -> ErrorName
getErrorName (BuiltinApplicationError {}) = BuiltinApplicationErrorName
getErrorName (ClosureApplicationError {}) = ClosureApplicationErrorName
getErrorName (RaiseError {}) = RaiseErrorName
getErrorName (MissingVariableError {}) = MissingVariableErrorName
getErrorName (ParsingError {}) = ParsingErrorName

fromParsecError :: ParseError -> Error v
fromParsecError err =
  ParsingError (show err) (toLocation $ errorPos err)

instance (Formatable v) => Formatable (Error v) where
  format (RaiseError message location) =
    Struct "raise-error" [Atom message, format location]
  format (BuiltinApplicationError cause name arguments location) =
    Struct
      "builtin-application-error"
      [Atom cause, Atom name, format arguments, format location]
  format (ClosureApplicationError cause callee arguments location) =
    Struct
      "closure-application-error"
      [Atom cause, format callee, format arguments, format location]
  format (MissingVariableError variable location) =
    Struct "missing-variable-error" [Atom variable, format location]
  format (ParsingError message location) =
    Struct "parsing-error" [Atom message, format location]
