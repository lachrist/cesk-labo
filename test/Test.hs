{-# OPTIONS_GHC -Wno-name-shadowing #-}

import CeskLabo
import Test.HUnit (Counts, Test (TestCase, TestList), assertEqual, assertFailure, runTestTT)

test :: StorageSystem -> (String, String) -> Serial -> IO ()
test storage file serial =
  run storage file >>= \(_, result) -> case result of
    Left error -> assertFailure $ "expected success, got " ++ show error
    Right (_, datum) -> assertEqual (fst file) serial datum

testPrimitive :: Test
testPrimitive =
  TestList
    [ TestCase $ test NoStorage ("null", "#n") NullLeaf,
      TestCase $ test NoStorage ("false", "#f") (BooleanLeaf False),
      TestCase $ test NoStorage ("true", "#t") (BooleanLeaf True),
      TestCase $ test NoStorage ("pos-int", "123") (NumberLeaf 123),
      TestCase $ test NoStorage ("neg-int", "-123") (NumberLeaf (-123)),
      TestCase $ test NoStorage ("pas-float", "123.456") (NumberLeaf 123.456),
      TestCase $ test NoStorage ("neg-float", "-123.456") (NumberLeaf (-123.456)),
      TestCase $ test NoStorage ("basic-string", "\"foo\"") (StringLeaf "foo"),
      TestCase $ test NoStorage ("escaped-string", "\" \\\" \\\\ \\t \\n \\r \"") (StringLeaf " \" \\ \t \n \r ")
    ]

testCondition :: Test
testCondition =
  TestList
    [ TestCase $ test NoStorage ("if-true", "(if #t 123 456)") (NumberLeaf 123),
      TestCase $ test NoStorage ("if-false", "(if #f 123 456)") (NumberLeaf 456)
    ]

testVariable :: Test
testVariable =
  TestCase $ test NoStorage ("let", "(let x 3 (* x x))") (NumberLeaf 9)

testClosure :: Test
testClosure =
  TestCase $ test NoStorage ("lambda", "((lambda (x y) (string-append x y)) \"foo\" \"bar\")") (StringLeaf "foobar")

testIdentity :: Test
testIdentity =
  TestList
    [ TestCase $ test NoStorage ("eq-void-number", "(eq? 123 123)") (BooleanLeaf True),
      TestCase $ test NoStorage ("eq-void-cons", "(eq? (cons 123 456) (cons 123 456))") (BooleanLeaf True),
      TestCase $ test ComprehensiveStorage ("eq-full-number", "(eq? 123 123)") (BooleanLeaf False),
      TestCase $ test ComprehensiveStorage ("eq-full-cons", "(eq? (cons 123 456) (cons 123 456))") (BooleanLeaf False),
      TestCase $ test ReuseComprehensiveStorage ("eq-reuse-full-number", "(eq? 123 123)") (BooleanLeaf True),
      TestCase $ test ReuseComprehensiveStorage ("eq-reuse-full-cons", "(eq? (cons 123 456) (cons 123 456))") (BooleanLeaf False),
      TestCase $ test HybridStorage ("eq-hybrid-number", "(eq? 123 123)") (BooleanLeaf True),
      TestCase $ test HybridStorage ("eq-hybrid-cons", "(eq? (cons 123 456) (cons 123 456))") (BooleanLeaf False)
    ]

main :: IO Counts
main =
  runTestTT $
    TestList
      [ testPrimitive,
        testCondition,
        testVariable,
        testClosure,
        testIdentity
      ]
