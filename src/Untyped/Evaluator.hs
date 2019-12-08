module Untyped.Evaluator where

import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Lib.Lib
import Untyped.Parser (Program(..), Statement(..), Term(..))

data RuntimeError
  = ParsingError
  | UnboundVariable String
  deriving (Eq, Show)

data EvaluationStrategy
  = CallByValue
  | FullBeta
  deriving (Eq, Ord)

type Context = Map.Map String Term

getFreeVars :: Term -> [String]
getFreeVars = g []
  where
    g :: [String] -> Term -> [String]
    g boundVars (T_VAR s)
      | s `elem` boundVars = []
      | otherwise = [s]
    g boundVars (T_APP t1 t2) = g boundVars t1 ++ g boundVars t2
    g boundVars (T_ABS (T_VAR s) t) = g (s : boundVars) t

pickFreshName :: String -> [String] -> String
pickFreshName s l
  | s `notElem` l = s
pickFreshName s l = g 1 s l
  where
    g :: Int -> String -> [String] -> String
    g n name existingNames
      | name ++ show n `elem` existingNames = g (n + 1) name existingNames
      | otherwise = name ++ show n

substitution :: String -> Term -> Term -> Term
substitution x s (T_VAR y)
  | x == y = s
  | otherwise = T_VAR y
substitution x s (T_APP t1 t2) =
  T_APP (substitution x s t1) (substitution x s t2)
substitution x _ t@(T_ABS (T_VAR y) _)
  | x == y = t
substitution x s (T_ABS (T_VAR y) t1) =
  let fv = getFreeVars s
   in if y `notElem` fv
        then T_ABS (T_VAR y) (substitution x s t1)
        else T_ABS (T_VAR (pickFreshName y fv)) (substitution x s t1)

checkVarsAreBound :: Context -> Term -> Either String ()
checkVarsAreBound c = g (Map.keys c)
  where
    g :: [String] -> Term -> Either String ()
    g boundVars (T_VAR x)
      | x `elem` boundVars = Right ()
      | otherwise = Left x
    g boundVars (T_APP t1 t2) = g boundVars t1 >> g boundVars t2
    g boundVars (T_ABS (T_VAR x) t) = g (x : boundVars) t

getEvalFromStrategy :: EvaluationStrategy -> (Term -> Term)
getEvalFromStrategy CallByValue = eval1StepCallByValue
getEvalFromStrategy FullBeta = evalBeta1Step

-- only keep final reduction
evalProgramFinalResult ::
     EvaluationStrategy -> Program -> Either RuntimeError Term
evalProgramFinalResult strategy p =
  fromMaybe T_UNIT <$> (lastMay <$> evalProgram strategy p)

evalProgram :: EvaluationStrategy -> Program -> Either RuntimeError [Term]
evalProgram strategy program =
  evalState (evalProgramWithContext strategy program) Map.empty

evalProgramWithContext ::
     EvaluationStrategy -> Program -> State Context (Either RuntimeError [Term])
evalProgramWithContext _ (Program []) = return $ Right [T_UNIT]
evalProgramWithContext strategy (Program [Run t]) = do
  c <- get
  return $ evalTerm strategy (replaceDeclaredVariables c t)
-- run not in last position -> just discard it
evalProgramWithContext strategy (Program (Run _:stmts)) =
  evalProgramWithContext strategy (Program stmts)
evalProgramWithContext strategy (Program (Assign name term:stmts)) = do
  c <- get
  case checkVarsAreBound c term of
    Left x -> return $ Left (UnboundVariable x)
    Right _ -> do
      put (Map.insert name (replaceDeclaredVariables c term) c)
      evalProgramWithContext strategy (Program stmts)

evalTerm :: EvaluationStrategy -> Term -> Either RuntimeError [Term]
evalTerm strategy term =
  case checkVarsAreBound Map.empty term of
    Left x -> Left (UnboundVariable x)
    Right _ -> Right (actualEval term)
  where
    actualEval :: Term -> [Term]
    actualEval t =
      let evalFunc = getEvalFromStrategy strategy
          newTerm = evalFunc t
       in if newTerm == t
            then [newTerm]
            else t : actualEval newTerm

-- replace declared vars with their actual content
replaceDeclaredVariables :: Context -> Term -> Term
replaceDeclaredVariables c t@(T_VAR x) =
  case Map.lookup x c of
    Nothing -> t
    Just t1 -> replaceDeclaredVariables (Map.delete x c) t1
replaceDeclaredVariables c (T_APP t1 t2) =
  T_APP (replaceDeclaredVariables c t1) (replaceDeclaredVariables c t2)
replaceDeclaredVariables c (T_ABS (T_VAR x) t) =
  T_ABS (T_VAR x) (replaceDeclaredVariables (Map.delete x c) t)

-- call by value
eval1StepCallByValue :: Term -> Term
eval1StepCallByValue (T_APP (T_ABS (T_VAR x) t12) v2@(T_ABS _ _)) =
  substitution x v2 t12
eval1StepCallByValue (T_APP v1@(T_ABS _ _) t2) =
  T_APP v1 (eval1StepCallByValue t2)
eval1StepCallByValue (T_APP t1 t2) = T_APP (eval1StepCallByValue t1) t2
eval1StepCallByValue t = t

-- beta evaluation
evalBeta1Step :: Term -> Term
evalBeta1Step (T_APP (T_ABS (T_VAR x) t12) t2) = substitution x t2 t12
evalBeta1Step (T_APP t1 t2) = T_APP (evalBeta1Step t1) (evalBeta1Step t2)
evalBeta1Step (T_ABS (T_VAR x) t) = T_ABS (T_VAR x) (evalBeta1Step t)
evalBeta1Step t = t
