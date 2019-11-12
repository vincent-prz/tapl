module Main where

import Untyped.Parser

repl :: IO ()
repl = do
  input <- getLine
  let parseResult = Untyped.Parser.fullParser input
  case parseResult of
    Left err -> print err
    Right t -> print t
    -- Right t -> print $ eval t
  repl

main :: IO ()
main = do
  putStrLn "Untyped lambda calculus REPL"
  repl
