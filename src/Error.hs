module Error where

import Expression (Location, Variable)
import Parse (toLocation)
import Serial (Serial (StringLeaf, StructureNode), Serializable (serialize))
import Text.Parsec (ParseError, errorPos)

type Message = String

data Error v
  = ApplicationError Message v [v] Location
  | MissingVariableError Variable Location
  | ParsingError Message Location
  deriving (Eq, Show)

data ErrorName
  = ApplicationErrorName
  | MissingVariableErrorName
  | ParsingErrorName
  deriving (Eq, Show)

getErrorName :: Error v -> ErrorName
getErrorName (ApplicationError {}) = ApplicationErrorName
getErrorName (MissingVariableError {}) = MissingVariableErrorName
getErrorName (ParsingError {}) = ParsingErrorName

fromParsecError :: ParseError -> Error v
fromParsecError err =
  ParsingError (show err) (toLocation $ errorPos err)

instance (Serializable v) => Serializable (Error v) where
  serialize (ApplicationError message function arguments location) =
    StructureNode
      "application-error"
      [StringLeaf message, serialize function, serialize arguments, serialize location]
  serialize (MissingVariableError variable location) =
    StructureNode "missing-variable-error" [StringLeaf variable, serialize location]
  serialize (ParsingError message location) =
    StructureNode "parsing-error" [StringLeaf message, serialize location]
