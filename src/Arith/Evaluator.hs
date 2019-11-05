module Arith.Evaluator where

import Arith.Parser (Term(..))

eval :: Term -> Term
eval term =
  let newTerm = applyRule term
   in if newTerm == term
        then newTerm
        else eval newTerm

applyRule :: Term -> Term
applyRule (T_IF_THEN_ELSE T_TRUE t2 _) = t2 -- E-IF-TRUE
applyRule (T_IF_THEN_ELSE T_FALSE _ t3) = t3 -- E-IF-FALSE
applyRule (T_IF_THEN_ELSE t t2 t3) = T_IF_THEN_ELSE (eval t) t2 t3 -- E-IF
applyRule (T_SUCC t) = T_SUCC (applyRule t) -- E-SUCC
applyRule (T_PRED T_ZERO) = T_ZERO -- E-PREDZERO
applyRule (T_PRED (T_SUCC t)) = applyRule t -- E-PREDSUCC
applyRule (T_PRED t) = T_PRED (applyRule t) -- E-PRED
applyRule (T_IS_ZERO T_ZERO) = T_TRUE -- E-ISZEROZERO
applyRule (T_IS_ZERO (T_SUCC t)) = T_FALSE -- E-ISZEROSUCC
applyRule (T_IS_ZERO t) = T_IS_ZERO (applyRule t) -- E-ISZERO
applyRule term = term -- already in normal form, nothing to do
