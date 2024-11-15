module Main where

import CeskLabo (render, run)
import System.Environment (getArgs)

usage :: String
usage = "usage: cesk (full-storage|void-storage|hybrid-storage|reuse-full-storage) path/to/main.scm"

top :: [String] -> IO ()
top [sto, loc] = do
  txt <- readFile loc
  res <- run sto (loc, txt)
  case res of
    Just tree -> putStrLn $ render (Just 0) tree
    Nothing -> do
      putStrLn $ "invalid storage >> " ++ sto
      putStrLn usage
top argv = do
  putStrLn $ "expected two arguments >> " ++ show argv
  putStrLn usage

main :: IO ()
main = getArgs >>= top
