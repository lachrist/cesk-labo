{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Continuation where

import Environment (Environment)
import Expression (Expression, Location, Variable)
import Format (Format (format), Tree (Atom, Struct))

data Continuation v
  = Bind (Environment v) Variable Expression (Continuation v)
  | Apply (Environment v) [Expression] [v] (Continuation v) Location
  | Branch (Environment v) Expression Expression (Continuation v)
  | Finish

instance (Format v) => Format (Continuation v) where
  format :: (Format v) => Continuation v -> Tree
  format (Bind env var res nxt) =
    Struct "bind" [Atom var, format env, format res, format nxt]
  format (Apply env todo done nxt _) =
    Struct "apply" [format env, format todo, format done, format nxt]
  format (Branch env pos neg nxt) =
    Struct "branch" [format env, format pos, format neg, format nxt]
  format Finish =
    Struct "finish" []
