module SimplyTyped.Evaluator where

import qualified Data.Map as Map
import SimplyTyped.Definitions (CoreTerm (..))
import SimplyTyped.Variables (getFreeVars, pickFreshName)

type Context = Map.Map String CoreTerm

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
        else
          let freshName = pickFreshName y fv
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
substitution x s (CoLetExpr y t1 t2)
  | x /= y = CoLetExpr y (substitution x s t1) (substitution x s t2)
  | otherwise = CoLetExpr y (substitution x s t1) t2
substitution x s (CoPair t1 t2) = CoPair (substitution x s t1) (substitution x s t2)
substitution x s (CoProjection t n) = CoProjection (substitution x s t) n

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
isValue (CoPair t1 t2) = isValue t1 && isValue t2
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
eval1Step (CoLetExpr x t1 t2) =
  let t1Val = eval1Step t1
   in substitution x t1Val t2
eval1Step (CoProjection (CoPair t1 t2) 1) | isValue t1 && isValue t2 = t1
eval1Step (CoProjection (CoPair t1 t2) 2) | isValue t1 && isValue t2 = t2
eval1Step (CoProjection t n) | not (isValue t) && n `elem` [1, 2] = CoProjection (eval1Step t) n
eval1Step (CoPair t1 t2) | not (isValue t1) = CoPair (eval1Step t1) t2
eval1Step (CoPair t1 t2) | not (isValue t2) = CoPair t1 (eval1Step t2)
eval1Step t = t
