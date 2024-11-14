{-# LANGUAGE FlexibleInstances #-}

module Continuation where

import Environment (Environment)
import Expression (Expression, Location, Variable)
import Formatable (Formatable (format), Tree (Atom, Struct))

data Continuation v
  = Bind (Environment v) Variable Expression (Continuation v)
  | Apply (Environment v) [Expression] [v] (Continuation v) Location
  | Branch (Environment v) Expression Expression (Continuation v)
  | Finish

instance (Formatable v) => Formatable (Continuation v) where
  format (Bind env var res nxt) =
    Struct "bind" [Atom var, format env, format res, format nxt]
  format (Apply env todo done nxt _) =
    Struct "apply" [format env, format todo, format done, format nxt]
  format (Branch env pos neg nxt) =
    Struct "branch" [format env, format pos, format neg, format nxt]
  format Finish =
    Struct "finish" []
