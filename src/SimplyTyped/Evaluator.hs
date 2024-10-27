module SimplyTyped.Evaluator (evalTerm) where

import Control.Monad.State (MonadState (get, put), State, evalState)
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
substitution x s (CoTuple ts) = CoTuple (map (substitution x s) ts)
substitution x s (CoProjection t n) = CoProjection (substitution x s t) n
substitution x s (CoAssign name t) = CoAssign name (substitution x s t)

-- assumption: the input CoreTerm has been typechecked
evalTerm :: CoreTerm -> CoreTerm
evalTerm term = evalState (evalTermWithContext term) Map.empty

evalTermWithContext :: CoreTerm -> State Context CoreTerm
evalTermWithContext term = do
  newTerm <- eval1Step term
  if newTerm == term
    then return newTerm
    else evalTermWithContext newTerm

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
isValue (CoTuple ts) = all isValue ts
isValue t = isBoolValue t || isNatValue t

-- call by value
eval1Step :: CoreTerm -> State Context CoreTerm
eval1Step (CoApp (CoAbs x _ t12) v2)
  | isValue v2 = return $ substitution x v2 t12
eval1Step (CoApp v1@CoAbs {} t2) = CoApp v1 <$> eval1Step t2
eval1Step (CoApp t1 t2) = CoApp <$> eval1Step t1 <*> pure t2
eval1Step (CoIfThenElse CoConstTrue t2 _) = return t2
eval1Step (CoIfThenElse CoConstFalse _ t3) = return t3
eval1Step (CoIfThenElse t1 t2 t3) = CoIfThenElse <$> eval1Step t1 <*> pure t2 <*> pure t3
eval1Step (CoSucc t) = CoSucc <$> eval1Step t
eval1Step (CoPred CoConstZero) = return CoConstZero
eval1Step (CoPred (CoSucc t)) = eval1Step t
eval1Step (CoIsZero CoConstZero) = return CoConstTrue
eval1Step (CoIsZero (CoSucc _)) = return CoConstFalse
eval1Step (CoIsZero t) = CoIsZero <$> eval1Step t
eval1Step (CoLetExpr x t1 t2) =
  let t1Val = eval1Step t1
   in substitution x <$> t1Val <*> pure t2
eval1Step (CoProjection tup@(CoTuple ts) n) | n `elem` [1 .. length ts] && isValue tup = return $ ts !! (n - 1)
eval1Step (CoProjection t n) | not (isValue t) = CoProjection <$> eval1Step t <*> pure n
-- NOTE: technically doing several steps at once here
eval1Step tup@(CoTuple ts)
  | not (isValue tup) =
    CoTuple <$> mapM (\t -> if not (isValue t) then eval1Step t else pure t) ts
eval1Step (CoAssign s t) = do
  ctx <- get
  put (Map.insert s t ctx)
  return CoConstUnit
eval1Step t@(CoVar s) = do
  ctx <- get
  case Map.lookup s ctx of
    Nothing -> return t
    Just t' -> return t'
eval1Step t = return t
