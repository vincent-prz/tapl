module Main where

import SimplyTyped.Evaluator
import SimplyTyped.Parser
import SimplyTyped.Desugar
import SimplyTyped.TypeChecker
import System.Console.Haskeline

processInput :: String -> String
processInput input =
  let parseResult = fullParser input
   in case parseResult of
        Left err -> err
        Right terms -> processTerm (desugar terms)

processTerm :: Term -> String
processTerm term =
  case typecheck term of
    Left err -> show err
    Right typ -> show (evalTerm term) ++ " : " ++ show typ

main :: IO ()
main =
  putStrLn "Simply typed lambda calculus REPL" >> runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "> "
      case minput of
        Nothing -> return ()
        Just ":quit" -> return ()
        Just input -> do
          let output = processInput input
          outputStrLn output
          loop
