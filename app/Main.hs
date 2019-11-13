module Main where

import System.Console.Haskeline
import Untyped.Parser

processInput :: String -> String
processInput input =
  let parseResult = Untyped.Parser.fullParser input
   in case parseResult of
        Left err -> show err
        Right t -> show t

main :: IO ()
main = putStrLn "Untyped lambda calculus REPL" >> runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "> "
      case minput of
        Nothing -> return ()
        Just "quit" -> return ()
        Just input -> do
          outputStrLn $ processInput input
          loop
