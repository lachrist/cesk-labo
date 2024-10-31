module Error where

data Error
  = ArityError
  | ApplyError
  | BuiltinError String String
