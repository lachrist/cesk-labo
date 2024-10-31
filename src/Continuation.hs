{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Continuation where

import Environment (Environment)
import Expression (Expression, Variable)
import Format (Format (..), formatAllBracket, formatOne)

data Continuation v
  = Bind (Environment v) Variable Expression (Continuation v)
  | Apply (Environment v) [Expression] [v] (Continuation v)
  | Branch (Environment v) Expression Expression (Continuation v)
  | Finish

instance (Format v) => Format (Continuation v) where
  format :: (Format v) => Int -> Continuation v -> String
  format level (Bind env var body kont) =
    "("
      ++ ("bind " ++ var)
      ++ formatOne level env
      ++ formatOne level body
      ++ formatOne level kont
      ++ ")"
  format level (Apply env todo done kont) =
    "("
      ++ "apply"
      ++ formatOne level env
      ++ formatAllBracket level todo
      ++ formatAllBracket level done
      ++ formatOne level kont
      ++ ")"
  format level (Branch env cons alt kont) =
    "("
      ++ "branch"
      ++ formatOne level env
      ++ formatOne level cons
      ++ formatOne level alt
      ++ formatOne level kont
      ++ ")"
  format _ Finish = "(finish)"
