module Main where

import System.Console.Haskeline
import Untyped.Evaluator
import Untyped.Parser

processInput :: EvaluationStrategy -> String -> String
processInput evalStrat input =
  let parseResult = Untyped.Parser.fullParser input
   in case parseResult of
        Left err -> show err
        Right t -> reduceTerm evalStrat t

reduceTerm :: EvaluationStrategy -> Term -> String
reduceTerm evalStrat t =
  case evalWithStrategy evalStrat t of
    Left err -> show err
    Right t' -> show t'

main :: IO ()
main =
  putStrLn "Untyped lambda calculus REPL" >>
  runInputT defaultSettings (loop CallByValue)
  where
    loop :: EvaluationStrategy -> InputT IO ()
    loop evalStrat = do
      minput <- getInputLine "> "
      case minput of
        Nothing -> return ()
        Just "quit" -> return ()
        Just ":cbv" -> do
          outputStrLn "Switching to call by value evaluation mode"
          loop CallByValue
        Just ":beta" -> do
          outputStrLn "Switching to full beta reduction evaluation mode"
          loop FullBeta
        Just input -> do
          outputStrLn $ processInput evalStrat input
          loop evalStrat
