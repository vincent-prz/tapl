module Main where

import Control.Monad.State
import Data.List (intercalate)
import qualified Data.Map as Map
import System.Console.Haskeline
import Untyped.Evaluator
import Untyped.Parser

data Options = Options
  { verbose :: Bool
  , strategy :: EvaluationStrategy
  }

defaultOptions :: Options
defaultOptions = Options False CallByValue

processInput :: Options -> String -> State Context String
processInput opts input =
  let parseResult = Untyped.Parser.fullParser input
   in case parseResult of
        Left err -> return $ show err
        Right program -> runProgram opts program

runProgram :: Options -> Program -> State Context String
runProgram opts program = do
  result <- evalProgramWithContext (strategy opts) program
  case result of
    Left err -> return $ show err
    Right t -> return $ show t

main :: IO ()
main =
  putStrLn "Untyped lambda calculus REPL" >>
  runInputT defaultSettings (loop defaultOptions Map.empty)
  where
    loop :: Options -> Context -> InputT IO ()
    loop opts c = do
      minput <- getInputLine "> "
      case minput of
        Nothing -> return ()
        Just ":quit" -> return ()
        Just ":cbv" -> do
          outputStrLn "Switching to call by value evaluation mode."
          loop (Options (verbose opts) CallByValue) c
        Just ":beta" -> do
          outputStrLn "Switching to full beta reduction evaluation mode."
          loop (Options (verbose opts) FullBeta) c
        Just ":verbose on" -> do
          outputStrLn "Enabling verbose mode."
          loop (Options True (strategy opts)) c
        Just ":verbose off" -> do
          outputStrLn "Disabling verbose mode."
          loop (Options False (strategy opts)) c
        Just input -> do
          let (output, newC) = runState (processInput opts input) c
          outputStrLn output
          loop opts newC
