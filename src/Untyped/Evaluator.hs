module Untyped.Evaluator where

import qualified Data.Map as Map
import Debug.Trace
import Untyped.Parser (Program(..), Statement(..), Term(..))

data RuntimeError
  = ParsingError
  | UnboundVariable String
  deriving (Eq, Show)

data EvaluationStrategy
  = CallByValue
  | FullBeta

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

getEvalFromStrategy :: EvaluationStrategy -> (Context -> Term -> Term)
getEvalFromStrategy CallByValue = eval1Step
getEvalFromStrategy FullBeta = evalBeta1Step

-- TODO: make notation more consistent, eg rename eval in evalCallbyValue,
-- and evalWithStrategy in eval
evalProgram :: EvaluationStrategy -> Program -> Either RuntimeError Term
evalProgram strategy program = evalProgramWithContext strategy Map.empty program

evalProgramWithContext ::
     EvaluationStrategy -> Context -> Program -> Either RuntimeError Term
evalProgramWithContext strategy c (Program []) = Right T_UNIT
evalProgramWithContext strategy c (Program [Run t]) =
  evalWithStrategy strategy c t
evalProgramWithContext strategy c (Program (Run _:stmts)) =
  evalProgramWithContext strategy c (Program stmts)
evalProgramWithContext strategy c (Program (Assign name term:stmts)) =
  evalProgramWithContext strategy (Map.insert name term c) (Program stmts)

evalWithStrategy ::
     EvaluationStrategy -> Context -> Term -> Either RuntimeError Term
evalWithStrategy strategy c term =
  case checkVarsAreBound c term of
    Left x -> Left (UnboundVariable x)
    Right _ -> Right (actualEval c term)
  where
    actualEval :: Context -> Term -> Term
    actualEval c t =
      let evalFunc = trace ("\n" ++ show t) getEvalFromStrategy strategy
          newTerm = evalFunc c t
       in if newTerm == t
            then newTerm
            else actualEval c newTerm

-- also returns intermediary steps
verboseEvalWithStrategy ::
     EvaluationStrategy -> Context -> Term -> Either RuntimeError [Term]
verboseEvalWithStrategy strategy c term =
  case checkVarsAreBound Map.empty term of
    Left x -> Left (UnboundVariable x)
    Right _ -> Right (actualEval c term)
  where
    actualEval :: Context -> Term -> [Term]
    actualEval c t =
      let evalFunc = getEvalFromStrategy strategy
          newTerm = evalFunc c t
       in if newTerm == t
            then [newTerm]
            else t : actualEval c newTerm

-- call by value
eval :: Context -> Term -> Either RuntimeError Term
eval = evalWithStrategy CallByValue

eval1Step :: Context -> Term -> Term
eval1Step c (T_APP (T_ABS (T_VAR x) t12) v2@(T_ABS _ _)) = substitution x v2 t12
eval1Step c (T_APP v1@(T_ABS _ _) t2) = T_APP v1 (eval1Step c t2)
eval1Step c (T_APP t1 t2) = T_APP (eval1Step c t1) t2
eval1Step c t = t

-- beta evaluation
evalBeta :: Context -> Term -> Either RuntimeError Term
evalBeta = evalWithStrategy FullBeta

evalBeta1Step :: Context -> Term -> Term
evalBeta1Step c (T_APP (T_ABS (T_VAR x) t12) t2) = substitution x t2 t12
evalBeta1Step c (T_APP t1 t2) = T_APP (evalBeta1Step c t1) (evalBeta1Step c t2)
evalBeta1Step c (T_ABS (T_VAR x) t) = T_ABS (T_VAR x) (evalBeta1Step c t)
evalBeta1Step c t@(T_VAR x) =
  case Map.lookup x c of
    Just t1 -> t1
    Nothing -> t
evalBeta1Step c t = t
