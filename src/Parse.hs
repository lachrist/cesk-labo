module Parse (parseExpression) where

import Control
import Control.Monad
import Data.Functor
import Data.Functor.Identity
import Primitive
import Text.Parsec
import Text.Read

type Tag = SourcePos -> Expression -> Expression

-----------
-- Token --
-----------

separator :: [Char]
separator = " \t\r\n();"

parseComment :: ParsecT String u Identity ()
parseComment = char ';' >> many (noneOf "\n") >> return ()

parseSpace :: ParsecT String u Identity ()
parseSpace = many ((oneOf " \t\r\n" $> ()) <|> parseComment) $> ()

parseToken :: ParsecT String u Identity String
parseToken = many1 (noneOf separator)

readEscape :: Char -> Char
readEscape 'n' = '\n'
readEscape 't' = '\t'
readEscape 'r' = '\r'
readEscape c = c

parseStringChar :: ParsecT String u Identity Char
parseStringChar = noneOf "\\\"\n\t\r" <|> (char '\\' >> fmap readEscape anyChar)

parseString :: ParsecT String u Identity String
parseString = char '"' >> many parseStringChar <* char '"'

parseKeyword :: String -> ParsecT String u Identity ()
parseKeyword keyword =
  try (string keyword >> oneOf separator) $> ()

----------
-- Atom --
----------

readToken :: String -> Expression
readToken "#n" = Literal Primitive.Null
readToken "#t" = Literal (Primitive.Boolean True)
readToken "#f" = Literal (Primitive.Boolean False)
readToken input =
  maybe
    (Variable input)
    (Literal . Primitive.Number)
    (readMaybe input)

parseAtom :: Tag -> ParsecT String u Identity Expression
parseAtom tag =
  liftM2
    tag
    getPosition
    ( fmap (Literal . Primitive.String) parseString
        <|> fmap readToken parseToken
    )

--------------
-- Compound --
--------------

parseBinding :: Tag -> ParsecT String u Identity Expression
parseBinding tag =
  parseKeyword "let"
    >> liftM2
      tag
      getPosition
      ( liftM3
          Control.Binding
          (parseSpace >> parseToken)
          (parseExpression tag)
          (parseExpression tag)
      )

parseCondition :: Tag -> Text.Parsec.Parsec String u Expression
parseCondition tag =
  parseKeyword "if"
    >> liftM2
      tag
      Text.Parsec.getPosition
      ( liftM3
          Control.Condition
          (parseExpression tag)
          (parseExpression tag)
          (parseExpression tag)
      )

parseLambda :: Tag -> Text.Parsec.Parsec String u Expression
parseLambda tag =
  parseKeyword "lambda"
    >> liftM2
      tag
      Text.Parsec.getPosition
      ( liftM2
          Control.Lambda
          ( (parseSpace >> char '(')
              >> many (parseSpace >> parseToken)
                <* (parseSpace >> char ')')
          )
          (parseExpression tag)
      )

parseApplication :: Tag -> Text.Parsec.Parsec String u Expression
parseApplication tag =
  liftM2
    tag
    Text.Parsec.getPosition
    ( liftM2
        Control.Application
        (parseExpression tag)
        (many (parseExpression tag))
    )

parseCompound :: Tag -> Text.Parsec.Parsec String u Expression
parseCompound tag =
  char '('
    >> ( parseBinding tag
           <|> parseCondition tag
           <|> parseLambda tag
           <|> parseApplication tag
       )
      <* (parseSpace >> char ')')

----------------
-- Expression --
----------------

parseExpression :: Tag -> Text.Parsec.Parsec String u Expression
parseExpression tag = parseSpace >> (parseCompound tag <|> parseAtom tag)
