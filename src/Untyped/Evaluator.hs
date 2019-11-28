module Untyped.Evaluator where

import Untyped.Parser (Term(..))

data RuntimeError
  = ParsingError -- FIXME:only here for tests, remove this
  | UnboundVariable String
  deriving (Eq, Show)

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
    g n s l
      | s ++ show n `elem` l = g (n + 1) s l
      | otherwise = s ++ show n

substitution :: String -> Term -> Term -> Term
substitution x s (T_VAR y)
  | x == y = s
  | otherwise = T_VAR y
substitution x s (T_APP t1 t2) =
  T_APP (substitution x s t1) (substitution x s t2)
substitution x s t@(T_ABS (T_VAR y) t1)
  | x == y = t
substitution x s t@(T_ABS (T_VAR y) t1) =
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

eval :: Term -> Either RuntimeError Term
eval term =
  case checkVarsAreBound term of
    Left x -> Left (UnboundVariable x)
    Right _ -> Right (actualEval term)
  where
    actualEval term =
      let newTerm = eval1Step term
       in if newTerm == term
            then newTerm
            else actualEval newTerm

eval1Step :: Term -> Term
eval1Step (T_APP (T_ABS (T_VAR x) t12) v2@(T_ABS _ _)) = substitution x v2 t12
eval1Step (T_APP v1@(T_ABS _ _) t2) = T_APP v1 (eval1Step t2)
eval1Step (T_APP t1 t2) = T_APP (eval1Step t1) t2
eval1Step t = t

-- beta evaluation
-- TODO: refactor this with `Strategy` pattern
evalBeta :: Term -> Either RuntimeError Term
evalBeta term =
  case checkVarsAreBound term of
    Left x -> Left (UnboundVariable x)
    Right _ -> Right (actualEval term)
  where
    actualEval term =
      let newTerm = evalBeta1Step term
       in if newTerm == term
            then newTerm
            else actualEval newTerm

evalBeta1Step :: Term -> Term
evalBeta1Step (T_APP (T_ABS (T_VAR x) t12) t2) = substitution x t2 t12
evalBeta1Step (T_APP t1 t2) = T_APP (evalBeta1Step t1) (evalBeta1Step t2)
evalBeta1Step (T_ABS (T_VAR x) t) = T_ABS (T_VAR x) (evalBeta1Step t)
evalBeta1Step t = t
