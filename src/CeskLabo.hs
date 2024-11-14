module CeskLabo
  ( Error (..),
    ErrorName (..),
    getErrorName,
    exec,
    run,
    parse,
    weave,
    render,
    Primitive (..),
    Data (..),
    Expression (..),
    module Storage,
  )
where

import Data (Data (..))
import Error (Error (..), ErrorName (..), fromParsecError, getErrorName)
import Evaluate (eval)
import Expression (Expression (..))
import Formatable (Formatable (format), Tree, render)
import Parse (parseExpression)
import Primitive (Primitive (..))
import Storage
import qualified Text.Parsec (parse)
import Weave (weave)

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left (f x)
mapLeft _ (Right y) = Right y

parse :: (String, String) -> Either (Error v) Expression
parse (loc, txt) = mapLeft fromParsecError (Text.Parsec.parse parseExpression loc txt)

exec :: (Eq v, Show v, Storage s v) => s -> (String, String) -> IO (s, Either (Error v) v)
exec mem (loc, txt) = case parse (loc, txt) of
  Left err -> return (mem, Left err)
  Right expr -> eval mem expr

run :: String -> (String, String) -> IO (Maybe Tree)
run "full" top = fmap (Just . format) (exec initialFullStore top)
run "reuse-full" top = fmap (Just . format) (exec initialReuseFullStore top)
run "void" top = fmap (Just . format) (exec initialVoidStore top)
run "hybrid" top = fmap (Just . format) (exec initialHybridStore top)
run _ _ = return Nothing
