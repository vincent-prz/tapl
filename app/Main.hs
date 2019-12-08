module Main where

import Control.Monad.State
import Data.List (intercalate)
import qualified Data.Map as Map
import Lib.Lib
import System.Console.Haskeline
import Untyped.Evaluator
import Untyped.Parser

data EvalOpts = EvalOpts
  { verbose :: Bool
  , strategy :: EvaluationStrategy
  }

defaultOpts :: EvalOpts
defaultOpts = EvalOpts False CallByValue

processInput :: EvalOpts -> String -> State Context String
processInput opts input =
  let parseResult = fullParser input
   in case parseResult of
        Left err -> return $ show err
        Right program -> runProgram opts program

runProgram :: EvalOpts -> Program -> State Context String
runProgram opts program = do
  result <- evalProgramWithContext (strategy opts) program
  case result of
    Left err -> return $ show err
    Right ts ->
      return $
      if verbose opts
        then intercalate "\n" (map show ts)
        else maybe "oops, something went wrong" show (lastMay ts)

main :: IO ()
main =
  putStrLn "Untyped lambda calculus REPL" >>
  runInputT defaultSettings (loop defaultOpts Map.empty)
  where
    loop :: EvalOpts -> Context -> InputT IO ()
    loop opts c = do
      minput <- getInputLine "> "
      case minput of
        Nothing -> return ()
        Just ":quit" -> return ()
        Just ":cbv" -> do
          outputStrLn "Switching to call by value evaluation mode."
          loop (opts {strategy = CallByValue}) c
        Just ":beta" -> do
          outputStrLn "Switching to full beta reduction evaluation mode."
          loop (opts {strategy = FullBeta}) c
        Just ":verbose on" -> do
          outputStrLn "Enabling verbose mode."
          loop (opts {verbose = True}) c
        Just ":verbose off" -> do
          outputStrLn "Disabling verbose mode."
          loop (opts {verbose = False}) c
        Just input -> do
          let (output, newC) = runState (processInput opts input) c
          outputStrLn output
          loop opts newC
