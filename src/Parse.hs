module Parse (parseExpression) where

import Control.Monad (liftM2, liftM3)
import Data.Functor (($>))
import Data.Functor.Identity (Identity)
import Expression (Expression (..), Location)
import GHC.Base (liftM4)
import Primitive (Primitive (Boolean, Null, Number, String))
import Text.Parsec
  ( Parsec,
    ParsecT,
    SourcePos,
    anyChar,
    char,
    getPosition,
    many,
    many1,
    noneOf,
    oneOf,
    sourceColumn,
    sourceLine,
    sourceName,
    string,
    try,
    (<|>),
  )
import Text.Read (readMaybe)

toLocation :: SourcePos -> Location
toLocation position =
  ( sourceName position,
    sourceLine position,
    sourceColumn position
  )

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

readToken :: String -> Location -> Expression
readToken "#n" = Literal Primitive.Null
readToken "#t" = Literal (Primitive.Boolean True)
readToken "#f" = Literal (Primitive.Boolean False)
readToken input =
  maybe
    (Variable input)
    (Literal . Primitive.Number)
    (readMaybe input)

readString :: String -> Location -> Expression
readString content = Literal (Primitive.String content)

parseAtom :: ParsecT String u Identity Expression
parseAtom = do
  location <- fmap toLocation getPosition
  liftM2
    readString
    parseString
    (return location)
    <|> liftM2
      readToken
      parseToken
      (return location)

--------------
-- Compound --
--------------

parseBinding :: Location -> ParsecT String u Identity Expression
parseBinding location =
  parseKeyword "let"
    >> liftM4
      Binding
      (parseSpace >> parseToken)
      parseExpression
      parseExpression
      (return location)

parseCondition :: Location -> Text.Parsec.Parsec String u Expression
parseCondition location =
  parseKeyword "if"
    >> liftM4
      Condition
      parseExpression
      parseExpression
      parseExpression
      (return location)

parseLambda :: Location -> Text.Parsec.Parsec String u Expression
parseLambda location =
  parseKeyword "lambda"
    >> liftM3
      Lambda
      ( do
          parseSpace
          _ <- char '('
          params <- many (parseSpace >> parseToken)
          parseSpace
          _ <- char ')'
          return params
      )
      parseExpression
      (return location)

parseApplication :: Location -> Text.Parsec.Parsec String u Expression
parseApplication location =
  liftM3
    Application
    parseExpression
    (many parseExpression)
    (return location)

parseCompound :: Text.Parsec.Parsec String u Expression
parseCompound = do
  location <- fmap toLocation getPosition
  _ <- char '('
  expr <-
    parseBinding location
      <|> parseCondition location
      <|> parseLambda location
      <|> parseApplication location
  parseSpace
  _ <- char ')'
  return expr

----------------
-- Expression --
----------------

parseExpression :: Text.Parsec.Parsec String u Expression
parseExpression = parseSpace >> (parseCompound <|> parseAtom)
