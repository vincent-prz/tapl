module SimplyTyped.Evaluator where

import qualified Data.Map as Map
import SimplyTyped.Definitions (CoreTerm(..))

type Context = Map.Map String CoreTerm

getFreeVars :: CoreTerm -> [String]
getFreeVars = g []
  where
    g :: [String] -> CoreTerm -> [String]
    g boundVars (CoVar s)
      | s `elem` boundVars = []
      | otherwise = [s]
    g boundVars (CoApp t1 t2) = g boundVars t1 ++ g boundVars t2
    g boundVars (CoAbs s _ t) = g (s : boundVars) t
    g _ CoConstTrue = []
    g _ CoConstFalse = []
    g _ CoConstZero = []
    g _ CoConstUnit = []
    g boundVars (CoIfThenElse t1 t2 t3) =
      g boundVars t1 ++ g boundVars t2 ++ g boundVars t3
    g boundVars (CoSucc t) = g boundVars t
    g boundVars (CoPred t) = g boundVars t
    g boundVars (CoIsZero t) = g boundVars t

pickFreshName :: String -> [String] -> String
pickFreshName s l
  | s `notElem` l = s
pickFreshName s l = g 1 s l
  where
    g :: Int -> String -> [String] -> String
    g n name existingNames
      | name ++ show n `elem` existingNames = g (n + 1) name existingNames
      | otherwise = name ++ show n

substitution :: String -> CoreTerm -> CoreTerm -> CoreTerm
substitution x s (CoVar y)
  | x == y = s
  | otherwise = CoVar y
substitution x s (CoApp t1 t2) =
  CoApp (substitution x s t1) (substitution x s t2)
substitution x _ t@(CoAbs y _ _)
  | x == y = t
substitution x s (CoAbs y typ t1) =
  let fv = getFreeVars s
   in if y `notElem` fv
        then CoAbs y typ (substitution x s t1)
        else let freshName = pickFreshName y fv
                 t1' = substitution y (CoVar freshName) t1
              in CoAbs freshName typ (substitution x s t1')
substitution _ _ CoConstTrue = CoConstTrue
substitution _ _ CoConstFalse = CoConstFalse
substitution x s (CoIfThenElse t1 t2 t3) =
  CoIfThenElse (substitution x s t1) (substitution x s t2) (substitution x s t3)
substitution _ _ CoConstZero = CoConstZero
substitution x s (CoSucc t) = CoSucc (substitution x s t)
substitution x s (CoPred t) = CoPred (substitution x s t)
substitution x s (CoIsZero t) = CoIsZero (substitution x s t)
substitution _ _ CoConstUnit = CoConstUnit

-- assumption: the input CoreTerm has been typechecked
evalTerm :: CoreTerm -> CoreTerm
evalTerm term =
  let newTerm = eval1Step term
   in if newTerm == term
        then newTerm
        else evalTerm newTerm

isBoolValue :: CoreTerm -> Bool
isBoolValue CoConstTrue = True
isBoolValue CoConstFalse = True
isBoolValue _ = False

isNatValue :: CoreTerm -> Bool
isNatValue CoConstZero = True
isNatValue (CoSucc t) = isNatValue t
isNatValue _ = False

isValue :: CoreTerm -> Bool
isValue CoAbs {} = True
isValue CoConstUnit = True
isValue t = isBoolValue t || isNatValue t

-- call by value
eval1Step :: CoreTerm -> CoreTerm
eval1Step (CoApp (CoAbs x _ t12) v2)
  | isValue v2 = substitution x v2 t12
eval1Step (CoApp v1@CoAbs {} t2) = CoApp v1 (eval1Step t2)
eval1Step (CoApp t1 t2) = CoApp (eval1Step t1) t2
eval1Step (CoIfThenElse CoConstTrue t2 _) = t2
eval1Step (CoIfThenElse CoConstFalse _ t3) = t3
eval1Step (CoIfThenElse t1 t2 t3) = CoIfThenElse (eval1Step t1) t2 t3
eval1Step (CoSucc t) = CoSucc (eval1Step t)
eval1Step (CoPred CoConstZero) = CoConstZero
eval1Step (CoPred (CoSucc t)) = eval1Step t
eval1Step (CoIsZero CoConstZero) = CoConstTrue
eval1Step (CoIsZero (CoSucc _)) = CoConstFalse
eval1Step (CoIsZero t) = CoIsZero (eval1Step t)
eval1Step t = t
