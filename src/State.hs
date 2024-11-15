{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module State where

import Continuation (Continuation)
import Environment (Environment)
import Error (Error)
import Expression (Expression)
import Serial (Serial (StructureNode), Serializable (serialize))
import Store (Store)

data Outcome x v
  = Success (Store x) v
  | Failure (Store x) (Error v)
  deriving (Eq, Show)

data State x v
  = Ongoing Expression (Environment v) (Store x) (Continuation v)
  | Final (Outcome x v)
  deriving (Eq, Show)

instance (Serializable x, Serializable v) => Serializable (Outcome x v) where
  serialize (Success mem val) = StructureNode "success" [serialize mem, serialize val]
  serialize (Failure mem err) = StructureNode "failure" [serialize mem, serialize err]

instance (Serializable x, Serializable v) => Serializable (State x v) where
  serialize (Ongoing cur env mem nxt) =
    StructureNode "ongoing" [serialize cur, serialize env, serialize mem, serialize nxt]
  serialize (Final out) = serialize out
