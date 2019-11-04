module Arith.Evaluator where

import Arith.Parser (Term(..))

eval :: Term -> Term
eval (T_PRED T_ZERO) = T_ZERO -- E-PREDZERO
eval (T_PRED (T_SUCC t)) = t -- E-PREDSUCC
eval term = term -- already in normal form, nothing to do
