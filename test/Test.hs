import CeskLabo
import Test.HUnit (Counts, Test (TestCase, TestList), assertEqual, assertFailure, runTestTT)

assert :: (Storage s v, Show v) => String -> Either ErrorName Primitive -> (s, Either (Error v) v) -> IO ()
assert msg (Left tag) (_, Left err) = assertEqual msg tag (getErrorName err)
assert msg (Right prm1) (mem, Right val) = case get mem val of
  Primitive prm2 -> assertEqual msg prm1 prm2
  _ -> assertFailure $ "expected primitive, got " ++ show val
assert msg (Left _) (_, Right val) = assertFailure $ msg ++ " >> expected failure, got " ++ show val
assert msg (Right _) (_, Left err) = assertFailure $ msg ++ " >> expected success, got " ++ show err

test :: (Eq v, Show v, Storage s v) => s -> (String, String) -> Either ErrorName Primitive -> IO ()
test mem (loc, txt) res = exec (loc, txt) mem >>= assert loc res

testPrimitive :: Test
testPrimitive =
  TestList
    [ TestCase $ test initialFullStore ("null", "#n") (Right Null),
      TestCase $ test initialFullStore ("false", "#f") (Right $ Boolean False),
      TestCase $ test initialFullStore ("true", "#t") (Right $ Boolean True),
      TestCase $ test initialFullStore ("pos-int", "123") (Right $ Number 123),
      TestCase $ test initialFullStore ("neg-int", "-123") (Right $ Number (-123)),
      TestCase $ test initialFullStore ("pas-float", "123.456") (Right $ Number 123.456),
      TestCase $ test initialFullStore ("neg-float", "-123.456") (Right $ Number (-123.456)),
      TestCase $ test initialFullStore ("basic-string", "\"foo\"") (Right $ String "foo"),
      TestCase $ test initialFullStore ("escaped-string", "\" \\\" \\\\ \\t \\n \\r \"") (Right $ String " \" \\ \t \n \r ")
    ]

testCondition :: Test
testCondition =
  TestList
    [ TestCase $ test initialFullStore ("if-true", "(if #t 123 456)") (Right $ Number 123),
      TestCase $ test initialFullStore ("if-false", "(if #f 123 456)") (Right $ Number 456)
    ]

testVariable :: Test
testVariable =
  TestCase $ test initialFullStore ("let", "(let x 3 (* x x))") (Right $ Number 9)

testClosure :: Test
testClosure =
  TestCase $ test initialFullStore ("lambda", "((lambda (x y) (string-append x y)) \"foo\" \"bar\")") (Right $ String "foobar")

main :: IO Counts
main = runTestTT $ TestList [testPrimitive, testCondition, testVariable, testClosure]
