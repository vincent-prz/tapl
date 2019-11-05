module Main where

import Arith.Evaluator
import Arith.Parser

repl :: IO ()
repl = do
  input <- getLine
  let parseResult = fullParser input
  case parseResult of
    Left err -> print err
    Right t -> print $ eval t
  repl

main :: IO ()
main = do
  putStrLn "Arith REPL"
  repl
