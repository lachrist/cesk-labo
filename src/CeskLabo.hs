{-# OPTIONS_GHC -Wno-name-shadowing #-}

module CeskLabo
  ( run,
    render,
    Serial (..),
    StorageSystem (..),
  )
where

import Error (Error, fromParsecError)
import Evaluate (eval)
import Expression (Expression (..))
import Parse (parseExpression)
import Serial (Serial (..), Serializable (serialize), render)
import State (Outcome (Failure, Success))
import Storage
import Store (initial)
import qualified Text.Parsec (parse)

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left (f x)
mapLeft _ (Right y) = Right y

type Path = String

type Content = String

type File = (Path, Content)

serializeOutcome :: (Serializable x, Serializable v, Storage x v) => Outcome x v -> (Serial, Either Serial (Serial, Serial))
serializeOutcome (Failure mem err) = (serialize mem, Left (serialize err))
serializeOutcome (Success mem val) = (serialize mem, Right (serialize val, serialize $ get mem val))

parse :: File -> Either (Error v) Expression
parse (path, content) = mapLeft fromParsecError (Text.Parsec.parse parseExpression path content)

exec :: (Eq v, Serializable v, Storage x v) => File -> IO (Outcome x v)
exec file = case parse file of
  Left error -> return $ Failure initial error
  Right root -> eval root

run :: StorageSystem -> File -> IO (Serial, Either Serial (Serial, Serial))
run NoStorage file = fmap serializeOutcome (exec file :: IO (Outcome VoidItem InlineValue))
run HybridStorage file = fmap serializeOutcome (exec file :: IO (Outcome HybridItem HybridValue))
run CompleteStorage file = fmap serializeOutcome (exec file :: IO (Outcome ComprehensiveItem ReferenceValue))
run ReuseCompleteStorage file = fmap serializeOutcome (exec file :: IO (Outcome ReuseComprehensiveItem ReuseReferenceValue))
