module SimplyTyped.Evaluator where

import qualified Data.Map as Map
import SimplyTyped.Parser (Term(..))

type Context = Map.Map String Term

getFreeVars :: Term -> [String]
getFreeVars = g []
  where
    g :: [String] -> Term -> [String]
    g boundVars (Var s)
      | s `elem` boundVars = []
      | otherwise = [s]
    g boundVars (App t1 t2) = g boundVars t1 ++ g boundVars t2
    g boundVars (Abs s _ t) = g (s : boundVars) t
    g _ ConstTrue = []
    g _ ConstFalse = []
    g boundVars (IfThenElse t1 t2 t3) =
      g boundVars t1 ++ g boundVars t2 ++ g boundVars t3

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
substitution x s (Var y)
  | x == y = s
  | otherwise = Var y
substitution x s (App t1 t2) = App (substitution x s t1) (substitution x s t2)
substitution x _ t@(Abs y _ _)
  | x == y = t
substitution x s (Abs y typ t1) =
  let fv = getFreeVars s
   in if y `notElem` fv
        then Abs y typ (substitution x s t1)
        else let freshName = pickFreshName y fv
                 t1' = substitution y (Var freshName) t1
              in Abs freshName typ (substitution x s t1')
substitution _ _ ConstTrue = ConstTrue
substitution _ _ ConstFalse = ConstFalse
substitution x s (IfThenElse t1 t2 t3) =
  IfThenElse (substitution x s t1) (substitution x s t2) (substitution x s t3)
substitution _ _ ConstZero = ConstZero
substitution x s (Succ t) = Succ (substitution x s t)

-- assumption: the input Term has been typechecked
evalTerm :: Term -> Term
evalTerm term =
  let newTerm = eval1Step term
   in if newTerm == term
        then newTerm
        else evalTerm newTerm

isBoolValue :: Term -> Bool
isBoolValue ConstTrue = True
isBoolValue ConstFalse = True
isBoolValue _ = False

isNatValue :: Term -> Bool
isNatValue ConstZero = True
isNatValue (Succ t) = isNatValue t
isNatValue _ = False

isValue :: Term -> Bool
isValue Abs {} = True
isValue t = isBoolValue t || isNatValue t

-- call by value
eval1Step :: Term -> Term
eval1Step (App (Abs x _ t12) v2)
  | isValue v2 = substitution x v2 t12
eval1Step (App v1@Abs {} t2) = App v1 (eval1Step t2)
eval1Step (App t1 t2) = App (eval1Step t1) t2
eval1Step (IfThenElse ConstTrue t2 _) = t2
eval1Step (IfThenElse ConstFalse _ t3) = t3
eval1Step (IfThenElse t1 t2 t3) = IfThenElse (eval1Step t1) t2 t3
eval1Step (Succ t) = Succ (eval1Step t) -- TODO check this in the book
eval1Step t = t
