module Main where

import SimplyTyped.Definitions
import SimplyTyped.Desugar
import SimplyTyped.Evaluator
import SimplyTyped.Parser
import SimplyTyped.TypeChecker
import SimplyTyped.Unsequence
import System.Console.Haskeline

processInput :: String -> String
processInput input =
  let parseResult = fullParser input
   in case parseResult of
        Left err -> err
        Right terms -> processTerm (unsequence terms)

processTerm :: Term -> String
processTerm term =
  case typecheck term of
    Left err -> show err
    Right typ -> show (evalTerm (desugar term)) ++ " : " ++ show typ

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
