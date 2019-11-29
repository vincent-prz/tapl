module Main where

import Data.List (intercalate)
import System.Console.Haskeline
import Untyped.Evaluator
import Untyped.Parser

data Options = Options
  { verbose :: Bool
  , strategy :: EvaluationStrategy
  }

defaultOptions :: Options
defaultOptions = Options False CallByValue

processInput :: Options -> String -> String
processInput opts input =
  let parseResult = Untyped.Parser.fullParser input
   in case parseResult of
        Left err -> show err
        Right t -> reduceTerm opts t

reduceTerm :: Options -> Term -> String
reduceTerm opts t =
  if verbose opts
    then case verboseEvalWithStrategy (strategy opts) t of
           Left err -> show err
           Right ts -> intercalate "\n" (map show ts)
    else case evalWithStrategy (strategy opts) t of
           Left err -> show err
           Right t' -> show t'

main :: IO ()
main =
  putStrLn "Untyped lambda calculus REPL" >>
  runInputT defaultSettings (loop defaultOptions)
  where
    loop :: Options -> InputT IO ()
    loop opts = do
      minput <- getInputLine "> "
      case minput of
        Nothing -> return ()
        Just ":quit" -> return ()
        Just ":cbv" -> do
          outputStrLn "Switching to call by value evaluation mode."
          loop (Options (verbose opts) CallByValue)
        Just ":beta" -> do
          outputStrLn "Switching to full beta reduction evaluation mode."
          loop (Options (verbose opts) FullBeta)
        Just ":verbose on" -> do
          outputStrLn "Enabling verbose mode."
          loop (Options True (strategy opts))
        Just ":verbose off" -> do
          outputStrLn "Disabling verbose mode."
          loop (Options False (strategy opts))
        Just input -> do
          outputStrLn $ processInput opts input
          loop opts
