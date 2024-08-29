module SimplyTyped.Desugar (desugar) where

import Data.Maybe (fromMaybe)
import SimplyTyped.Definitions
import SimplyTyped.Variables (getFreshName)

-- assumption: the term is well typed
desugar :: Term -> CoreTerm
desugar (Var s) = CoVar s
desugar (Abs s t1 b) = CoAbs paramName t1 coreB where
  coreB = desugar b
  paramName = fromMaybe (getFreshName "_" coreB) s
desugar (App t1 t2) = CoApp (desugar t1) (desugar t2)
desugar ConstTrue = CoConstTrue
desugar ConstFalse = CoConstFalse
desugar (IfThenElse g t1 t2) =
  CoIfThenElse (desugar g) (desugar t1) (desugar t2)
desugar ConstZero = CoConstZero
desugar (Succ t1) = CoSucc (desugar t1)
desugar (Pred t1) = CoPred (desugar t1)
desugar (IsZero t1) = CoIsZero (desugar t1)
desugar ConstUnit = CoConstUnit
desugar (Ascription term _) = desugar term
