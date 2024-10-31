{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module State where

import Continuation (Continuation)
import Environment (Environment)
import Error (Error)
import Expression (Expression)
import Format (Format (format), Tree (Struct))
import Store (Store)

data State v x
  = Ongoing Expression (Environment v) (Store x) (Continuation v)
  | Success v (Store x)
  | Failure (Error v) (Store x) (Continuation v)

instance (Format v, Format x) => Format (State v x) where
  format :: (Format v, Format x) => State v x -> Tree
  format (Ongoing expr env store kont) =
    Struct
      "ongoing"
      [format expr, format env, format store, format kont]
  format (Success result store) =
    Struct
      "success"
      [format result, format store]
  format (Failure error_ store kont) =
    Struct
      "failure"
      [format error_, format store, format kont]
