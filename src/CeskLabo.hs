module CeskLabo
  ( Error (..),
    ErrorName (..),
    getErrorName,
    exec,
    parse,
    weave,
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

exec :: (Eq v, Show v, Storage s v) => (String, String) -> s -> IO (s, Either (Error v) v)
exec (loc, txt) mem = case parse (loc, txt) of
  Left err -> return (mem, Left err)
  Right expr -> eval mem expr
