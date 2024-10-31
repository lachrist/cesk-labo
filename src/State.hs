{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module State where

import Continuation (Continuation)
import Environment (Environment)
import Expression (Expression)
import Format (Format (format), Tree (Atom, Struct))
import Store (Store)

data State v x
  = Ongoing Expression (Environment v) (Store x) (Continuation v)
  | Success (Store x) v
  | Failure (Store x) (Continuation v) (String, String)

instance (Format v, Format x) => Format (State v x) where
  format :: (Format v, Format x) => State v x -> Tree
  format (Ongoing expr env store kont) =
    Struct
      "ongoing"
      [format expr, format env, format store, format kont]
  format (Success store result) =
    Struct
      "success"
      [format result, format store]
  format (Failure store kont (name, message)) =
    Struct
      "failure"
      [Atom name, Atom message, format store, format kont]
