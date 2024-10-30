import Control
import Format
import Parse
import Primitive
import Test.HUnit
import Text.Parsec

tagIdentity :: SourcePos -> Expression -> Expression
tagIdentity _ expression = expression

parseBasic :: String -> Either ParseError Expression
parseBasic code = parse (parseExpression tagIdentity) "test.scm" code

tests :: Test
tests =
  TestList
    [ TestCase $
        assertEqual
          "true"
          (Right (Literal (Boolean True)))
          (parseBasic "#t"),
      TestCase $
        assertEqual
          "false"
          (Right (Literal (Boolean False)))
          (parseBasic "#f"),
      TestCase $
        assertEqual
          "null"
          (Right (Literal Null))
          (parseBasic "#n"),
      TestCase $
        assertEqual
          "integer"
          (Right (Literal (Number 123)))
          (parseBasic "123"),
      TestCase $
        assertEqual
          "decimal"
          (Right (Literal (Number 123.456)))
          (parseBasic "123.456"),
      TestCase $
        assertEqual
          "string"
          (Right (Literal (String "foo")))
          (parseBasic "\"foo\""),
      TestCase $
        assertEqual
          "string-escape"
          (Right (Literal (String " \" \\ \t \n \r ")))
          (parseBasic "\" \\\" \\\\ \\t \\n \\r \""),
      TestCase $
        assertEqual
          "variable"
          (Right (Variable "foo"))
          (parseBasic "foo"),
      TestCase $
        assertEqual
          "if"
          ( Right
              ( Condition
                  (Literal (Number 123))
                  (Literal (Number 456))
                  (Literal (Number 789))
              )
          )
          (parseBasic "(if 123 456 789)"),
      TestCase $
        assertEqual
          "let"
          ( Right
              ( Binding
                  "var"
                  (Literal (Number 123))
                  (Literal (Number 456))
              )
          )
          (parseBasic "(let var 123 456)"),
      TestCase $
        assertEqual
          "lambda"
          ( Right
              ( Lambda
                  ["foo", "bar"]
                  (Literal (Number 123))
              )
          )
          (parseBasic "(lambda (foo bar) 123)"),
      TestCase $
        assertEqual
          "application"
          ( Right
              ( Application
                  (Literal (Number 123))
                  [ Literal (Number 456),
                    Literal (Number 789)
                  ]
              )
          )
          (parseBasic "(123 456 789)"),
      TestCase $
        assertEqual
          "comment"
          (Right (Literal (Number 123)))
          (parseBasic " ; comment\n 123")
    ]

main :: IO Counts
main = runTestTT tests