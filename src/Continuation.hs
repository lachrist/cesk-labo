{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Continuation where

import Environment (Environment)
import Expression (Expression, Variable)
import Format (Format (format), Tree (Atom, Struct))

data Continuation v
  = Bind (Environment v) Variable Expression (Continuation v)
  | Apply (Environment v) [Expression] [v] (Continuation v)
  | Branch (Environment v) Expression Expression (Continuation v)
  | Finish

instance (Format v) => Format (Continuation v) where
  format :: (Format v) => Continuation v -> Tree
  format (Bind env var body kont) =
    Struct "bind" [Atom var, format env, format body, format kont]
  format (Apply env todo done kont) =
    Struct "apply" [format env, format todo, format done, format kont]
  format (Branch env cons alt kont) =
    Struct "branch" [format env, format cons, format alt, format kont]
  format Finish =
    Struct "finish" []
