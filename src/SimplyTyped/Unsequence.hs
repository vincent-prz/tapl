module SimplyTyped.Unsequence where

import SimplyTyped.Definitions

-- FIXME - deal with fresh varname properly
unsequence :: [Term] -> Term
unsequence [] = ConstUnit
unsequence [t] = t
unsequence (t:ts) = App (Abs name TUnit (unsequence ts)) t
  where
    name = "fresh" ++ show (length (t : ts)) -- this is a hack
