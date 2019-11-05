module Arith.Evaluator where

import Arith.Parser (Term(..))

eval :: Term -> Term
eval term =
  let newTerm = eval1Step term
   in if newTerm == term
        then newTerm
        else eval newTerm

eval1Step :: Term -> Term
eval1Step (T_IF_THEN_ELSE T_TRUE t2 _) = t2 -- E-IF-TRUE
eval1Step (T_IF_THEN_ELSE T_FALSE _ t3) = t3 -- E-IF-FALSE
eval1Step (T_IF_THEN_ELSE t t2 t3) = T_IF_THEN_ELSE (eval t) t2 t3 -- E-IF
eval1Step (T_SUCC t) = T_SUCC (eval1Step t) -- E-SUCC
eval1Step (T_PRED T_ZERO) = T_ZERO -- E-PREDZERO
eval1Step (T_PRED (T_SUCC t)) = eval1Step t -- E-PREDSUCC
eval1Step (T_PRED t) = T_PRED (eval1Step t) -- E-PRED
eval1Step (T_IS_ZERO T_ZERO) = T_TRUE -- E-ISZEROZERO
eval1Step (T_IS_ZERO (T_SUCC _)) = T_FALSE -- E-ISZEROSUCC
eval1Step (T_IS_ZERO t) = T_IS_ZERO (eval1Step t) -- E-ISZERO
eval1Step term = term -- already in normal form, nothing to do

-- everything below is only needed for the eval big step mode of evaluation
-- to activate the big step mode, just redefine `eval` as `eval = evalBigStep`
isNumericalValue :: Term -> Bool
isNumericalValue T_ZERO = True
isNumericalValue (T_SUCC t) = isNumericalValue t
isNumericalValue _ = False

isBooleanValue :: Term -> Bool
isBooleanValue T_TRUE = True
isBooleanValue T_FALSE = True
isBooleanValue _ = False

isValue :: Term -> Bool
isValue t = isNumericalValue t || isBooleanValue t

evalBigStep :: Term -> Term
evalBigStep t
  | isValue t = t -- B-Value
  -- | otherwise = t -- impossible to further evaluate
evalBigStep (T_IF_THEN_ELSE t1 t2 t3) =
  let cond = evalBigStep t1
   in if cond == T_TRUE
        then evalBigStep t2 -- B-IFTRUE
        else evalBigStep t3 -- B-IFFALSE
evalBigStep (T_SUCC t) = T_SUCC (evalBigStep t) --B-SUCC
evalBigStep (T_PRED t) =
  let value = evalBigStep t
   in case value of
        T_ZERO -> T_ZERO -- B-PREDZERO
        T_SUCC v -> v -- BPREDSUCC
        t' -> T_PRED t' -- impossible to further evaluate
evalBigStep (T_IS_ZERO t) =
  let value = evalBigStep t
   in case value of
        T_ZERO -> T_TRUE -- B-ISZEROZERO
        T_SUCC _ -> T_FALSE -- B-ISZEROSUCC
        t' -> T_IS_ZERO t' -- impossible to further evaluate
