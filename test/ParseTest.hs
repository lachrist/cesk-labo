import Expression
import Format ()
import Parse (parseExpression)
import Primitive (Primitive (Boolean, Null, Number, String))
import Test.HUnit
  ( Counts,
    Test (TestCase, TestList),
    assertEqual,
    runTestTT,
  )
import Text.Parsec (ParseError, parse)

parseBasic :: String -> Either ParseError Expression
parseBasic = parse parseExpression "text.scm"

loc :: Location
loc = Location "dummy" 0 0

tests :: Test
tests =
  TestList
    [ TestCase $
        assertEqual
          "true"
          (Right (Literal (Boolean True) loc))
          (parseBasic "#t"),
      TestCase $
        assertEqual
          "false"
          (Right (Literal (Boolean False) loc))
          (parseBasic "#f"),
      TestCase $
        assertEqual
          "null"
          (Right (Literal Null loc))
          (parseBasic "#n"),
      TestCase $
        assertEqual
          "integer"
          (Right (Literal (Number 123) loc))
          (parseBasic "123"),
      TestCase $
        assertEqual
          "decimal"
          (Right (Literal (Number 123.456) loc))
          (parseBasic "123.456"),
      TestCase $
        assertEqual
          "string"
          (Right (Literal (String "foo") loc))
          (parseBasic "\"foo\""),
      TestCase $
        assertEqual
          "string-escape"
          (Right (Literal (String " \" \\ \t \n \r ") loc))
          (parseBasic "\" \\\" \\\\ \\t \\n \\r \""),
      TestCase $
        assertEqual
          "variable"
          (Right (Variable "foo" loc))
          (parseBasic "foo"),
      TestCase $
        assertEqual
          "if"
          ( Right
              ( Condition
                  (Literal (Number 123) loc)
                  (Literal (Number 456) loc)
                  (Literal (Number 789) loc)
                  loc
              )
          )
          (parseBasic "(if 123 456 789)"),
      TestCase $
        assertEqual
          "let"
          ( Right
              ( Binding
                  "var"
                  (Literal (Number 123) loc)
                  (Literal (Number 456) loc)
                  loc
              )
          )
          (parseBasic "(let var 123 456)"),
      TestCase $
        assertEqual
          "lambda"
          ( Right
              ( Lambda
                  ["foo", "bar"]
                  (Literal (Number 123) loc)
                  loc
              )
          )
          (parseBasic "(lambda (foo bar) 123)"),
      TestCase $
        assertEqual
          "application"
          ( Right
              ( Application
                  (Literal (Number 123) loc)
                  [ Literal (Number 456) loc,
                    Literal (Number 789) loc
                  ]
                  loc
              )
          )
          (parseBasic "(123 456 789)"),
      TestCase $
        assertEqual
          "comment"
          (Right (Literal (Number 123) loc))
          (parseBasic " ; comment\n 123")
    ]

main :: IO Counts
main = runTestTT tests