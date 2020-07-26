module SimplyTyped.Desugar where

import SimplyTyped.Lexer
import SimplyTyped.Parser

-- FIXME - deal with fresh varname properly
desugar :: [Term] -> Term
desugar [] = ConstUnit
desugar [t] = t
desugar (t:ts) = App (Abs name TUnit (desugar ts)) t
  where
    name = "fresh" ++ show (length (t : ts)) -- this is a hack
