module Main where

import Lib.Lib
import SimplyTyped.Evaluator
import SimplyTyped.Parser
import SimplyTyped.TypeChecker
import System.Console.Haskeline

-- TODO: deal with parsing errors
processInput :: String -> String
processInput input =
  let term = fullParser input
      tcheck = typecheck term
   in case tcheck of
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
