module SimplyTyped.Unsequence where

import SimplyTyped.Definitions

unsequence :: [Term] -> Term
unsequence [] = ConstUnit
unsequence [t] = t
unsequence (t:ts) = App (Abs Nothing TUnit (unsequence ts)) t
