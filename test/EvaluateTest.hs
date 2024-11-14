module EvaluateTest (tests) where

import Data (Data (Primitive))
import Error (Error, ErrorName, getErrorName)
import Evaluate (exec)
import Primitive (Primitive (..))
import Storage
import Test.HUnit (Test (TestCase, TestList), assertEqual, assertFailure)

assert :: (Storage s v, Show v) => String -> Either ErrorName Primitive -> (s, Either (Error v) v) -> IO ()
assert msg (Left tag) (_, Left err) = assertEqual msg tag (getErrorName err)
assert msg (Right prm1) (mem, Right val) = case get mem val of
  Primitive prm2 -> assertEqual msg prm1 prm2
  _ -> assertFailure $ "expected primitive, got " ++ show val
assert msg (Left _) (_, Right val) = assertFailure $ msg ++ " >> expected failure, got " ++ show val
assert msg (Right _) (_, Left err) = assertFailure $ msg ++ " >> expected success, got " ++ show err

test :: (Eq v, Show v, Storage s v) => s -> (String, String) -> Either ErrorName Primitive -> IO ()
test mem (loc, txt) res = exec (loc, txt) mem >>= assert loc res

tests :: Test
tests =
  TestList
    [ TestCase $
        test empty1 ("number", "123") (Right $ Number 123)
    ]
