{-# LANGUAGE FlexibleInstances #-}

module Continuation where

import Environment (Environment)
import Expression (Expression, Location, Variable)
import Serial (Serial (StructureNode, SymbolLeaf), Serializable (serialize))

data Continuation v
  = Bind (Environment v) Variable Expression (Continuation v)
  | Apply (Environment v) [Expression] [v] (Continuation v) Location
  | Branch (Environment v) Expression Expression (Continuation v)
  | Finish
  deriving (Eq, Show)

instance (Serializable v) => Serializable (Continuation v) where
  serialize (Bind env var res nxt) =
    StructureNode "bind" [SymbolLeaf var, serialize env, serialize res, serialize nxt]
  serialize (Apply env todo done nxt _) =
    StructureNode "apply" [serialize env, serialize todo, serialize done, serialize nxt]
  serialize (Branch env pos neg nxt) =
    StructureNode "branch" [serialize env, serialize pos, serialize neg, serialize nxt]
  serialize Finish =
    StructureNode "finish" []
