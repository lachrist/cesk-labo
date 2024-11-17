{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import CeskLabo (Serial, StorageSystem (..), render, run)
import System.Environment (getArgs)

usage :: String
usage = "usage: cesk (full-storage|void-storage|hybrid-storage|reuse-full-storage) path/to/main.scm"

toStorageSystem :: String -> Maybe StorageSystem
toStorageSystem "no-storage" = Just NoStorage
toStorageSystem "hybrid-storage" = Just HybridStorage
toStorageSystem "comprehensive-storage" = Just ComprehensiveStorage
toStorageSystem "reuse-comprehensive-storage" = Just ReuseComprehensiveStorage
toStorageSystem _ = Nothing

showResult :: (Serial, Either Serial (Serial, Serial)) -> String
showResult (memory, Left error) =
  "FAILURE\n"
    ++ ("  memory >> " ++ render 1 memory ++ "\n")
    ++ ("  error >> " ++ render 1 error ++ "\n")
showResult (memory, Right (value, datum)) =
  "SUCCESS\n"
    ++ ("  memory >> " ++ render 1 memory ++ "\n")
    ++ ("  value >> " ++ render 1 value ++ "\n")
    ++ ("  datum >> " ++ render 1 datum ++ "\n")

main :: IO ()
main =
  getArgs >>= \case
    [storage, path] -> case toStorageSystem storage of
      Just system -> do
        content <- readFile path
        result <- run system (path, content)
        putStr $ showResult result
      Nothing -> do
        putStrLn $ "invalid storage system >> " ++ storage
        putStrLn usage
    argv -> do
      putStrLn $ "expected exactly two arguments but got >> " ++ show argv
      putStrLn usage
