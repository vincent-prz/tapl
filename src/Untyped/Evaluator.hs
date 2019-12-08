module Untyped.Evaluator where

import Control.Monad.State
import qualified Data.Map as Map
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

checkVarsAreBound :: Term -> Either String ()
checkVarsAreBound = g []
  where
    g :: [String] -> Term -> Either String ()
    g boundVars (T_VAR x)
      | x `elem` boundVars = Right ()
      | otherwise = Left x
    g boundVars (T_APP t1 t2) = g boundVars t1 >> g boundVars t2
    g boundVars (T_ABS (T_VAR x) t) = g (x : boundVars) t

getEvalFromStrategy :: EvaluationStrategy -> (Term -> Term)
getEvalFromStrategy CallByValue = eval1Step
getEvalFromStrategy FullBeta = evalBeta1Step

-- TODO: make notation more consistent, eg rename eval in evalCallbyValue,
-- and evalWithStrategy in eval
evalProgram :: EvaluationStrategy -> Program -> Either RuntimeError Term
evalProgram strategy program =
  evalState (evalProgramWithContext strategy program) Map.empty

evalProgramWithContext ::
     EvaluationStrategy -> Program -> State Context (Either RuntimeError Term)
evalProgramWithContext _ (Program []) = return (Right T_UNIT)
evalProgramWithContext strategy (Program [Run t]) = do
  c <- get
  return $ evalWithStrategy strategy (replaceDeclaredVariables c t)
evalProgramWithContext strategy (Program (Run _:stmts)) =
  evalProgramWithContext strategy (Program stmts)
evalProgramWithContext stategy (Program (Assign name term:stmts)) = do
  c <- get
  put (Map.insert name term c)
  evalProgramWithContext stategy (Program stmts)

evalWithStrategy :: EvaluationStrategy -> Term -> Either RuntimeError Term
evalWithStrategy strategy term =
  case checkVarsAreBound term of
    Left x -> Left (UnboundVariable x)
    Right _ -> Right (actualEval term)
  where
    actualEval :: Term -> Term
    actualEval t =
      let evalFunc = getEvalFromStrategy strategy
          newTerm = evalFunc t
       in if newTerm == t
            then newTerm
            else actualEval newTerm

-- also returns intermediary steps
verboseEvalWithStrategy ::
     EvaluationStrategy -> Term -> Either RuntimeError [Term]
verboseEvalWithStrategy strategy term =
  case checkVarsAreBound term of
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

-- replace declared vars with their actial content
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
eval :: Term -> Either RuntimeError Term
eval = evalWithStrategy CallByValue

eval1Step :: Term -> Term
eval1Step (T_APP (T_ABS (T_VAR x) t12) v2@(T_ABS _ _)) = substitution x v2 t12
eval1Step (T_APP v1@(T_ABS _ _) t2) = T_APP v1 (eval1Step t2)
eval1Step (T_APP t1 t2) = T_APP (eval1Step t1) t2
eval1Step t = t

-- beta evaluation
evalBeta :: Term -> Either RuntimeError Term
evalBeta = evalWithStrategy FullBeta

evalBeta1Step :: Term -> Term
evalBeta1Step (T_APP (T_ABS (T_VAR x) t12) t2) = substitution x t2 t12
evalBeta1Step (T_APP t1 t2) = T_APP (evalBeta1Step t1) (evalBeta1Step t2)
evalBeta1Step (T_ABS (T_VAR x) t) = T_ABS (T_VAR x) (evalBeta1Step t)
evalBeta1Step t = t
