module Main where

import Evaluate
import Format
import Parse
import System.Environment
import Text.Parsec.String

main :: IO ()
main = error "todo"

-- argv <- System.Environment.getArgs
-- case argv of
--   ["execute", path, system] -> Text.Parsec.String.parseFromFile (Parse.parser []) path >>= Interprete.top system >>= putStrLn
--   ("instrument" : path : pointcut) ->
--     Text.Parsec.String.parseFromFile (parseExpression pointcut) path >>= putStrLn . either show (showIndent 0)
--   _ -> do
--     putStrLn "Usage:"
--     putStrLn "  cesk execute program.scm (reference|value|mixed|reference-interning)"
--     putStrLn "  cesk instrument program.scm [literal] [variable] [binding] [condition] [abstraction] [application]"
