module Main where

import Control.Monad.State (MonadState (get, put), State, StateT (runStateT), runState)
import qualified Data.Map as Map
import SimplyTyped.Definitions
import SimplyTyped.Desugar
import SimplyTyped.Evaluator
import SimplyTyped.Parser
import SimplyTyped.TypeChecker
import SimplyTyped.Unsequence
import System.Console.Haskeline

processInput :: String -> State (Context, TypeContext) String
processInput input =
  let parseResult = fullParser input
   in case parseResult of
        Left err -> return err
        Right terms -> processTerm (unsequence terms)

processTerm :: Term -> State (Context, TypeContext) String
processTerm term = do
  (varContext, typContext) <- get
  let typResult = runStateT (typecheckWithContext term) typContext
  case typResult of
    Left err -> return $ show err
    Right (typ, newTypContext) -> do
      let (result, newVarContext) = runState (evalTermWithContext (desugar term)) varContext
      put (newVarContext, newTypContext)
      return (show result ++ " : " ++ show typ)

main :: IO ()
main =
  putStrLn "Simply typed lambda calculus REPL" >> runInputT defaultSettings (loop (Map.empty, Map.empty))
  where
    loop :: (Context, TypeContext) -> InputT IO ()
    loop context = do
      minput <- getInputLine "> "
      case minput of
        Nothing -> return ()
        Just ":quit" -> return ()
        Just input -> do
          let (output, newContext) = runState (processInput input) context
          outputStrLn output
          loop newContext
