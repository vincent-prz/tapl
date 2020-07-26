module SimplyTyped.Desugar where

import SimplyTyped.Definitions

-- assumption: the term is well typed
desugar :: Term -> CoreTerm
desugar (Var s) = CoVar s
desugar (Abs s t1 b) = CoAbs s t1 (desugar b)
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
-- hack: we don't actually care about the type of the abstraction here,
-- so we just put undefined.
desugar (Let s t1 t2) = CoApp (CoAbs s undefined (desugar t2)) (desugar t1)
