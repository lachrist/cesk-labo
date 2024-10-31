{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module State where

import Continuation (Continuation)
import Environment (Environment)
import Error (Error)
import Expression (Expression)
import Format (Format (format), Tree (Struct))

data State s v
  = Ongoing Expression (Environment v) s (Continuation v)
  | Success v s
  | Failure (Error v) s

instance (Format s, Format v) => Format (State s v) where
  format (Ongoing cur env mem nxt) =
    Struct "ongoing" [format cur, format env, format mem, format nxt]
  format (Success res mem) =
    Struct "success" [format res, format mem]
  format (Failure err mem) =
    Struct "failure" [format err, format mem]
