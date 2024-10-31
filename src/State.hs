{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module State where

import Continuation (Continuation)
import Environment (Environment)
import Expression (Expression)
import Format (Format (..), formatOne)
import Store (Store)

data State v x
  = Ongoing Expression (Environment v) (Store x) (Continuation v)
  | Success (Store x) v
  | Failure (Store x) (Continuation v) (String, String)

instance (Format v, Format x) => Format (State v x) where
  format :: (Format v, Format x) => Int -> State v x -> String
  format level (Ongoing control environment store kontinuation) =
    "("
      ++ "ongoing"
      ++ formatOne level control
      ++ formatOne level environment
      ++ formatOne level store
      ++ formatOne level kontinuation
      ++ ")"
  format level (Success store value) =
    "("
      ++ "success"
      ++ formatOne level value
      ++ formatOne level store
      ++ ")"
  format level (Failure store kontinuation (name, message)) =
    "("
      ++ ("failure " ++ name ++ " >> " ++ message)
      ++ formatOne level store
      ++ formatOne level kontinuation
      ++ ")"
