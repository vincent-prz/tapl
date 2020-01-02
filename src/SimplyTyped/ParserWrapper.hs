-- putting here customization of the Parser generated by Happy,
-- and which I didn't want to put in the Happy file.
module SimplyTyped.ParserWrapper where

import SimplyTyped.Lexer
import SimplyTyped.Parser

instance Show Term where
  show (Var s) = s
  show (Abs s t b) = "\\" ++ s ++ ":" ++ show t ++ "." ++ show b
  show (IfThenElse t1 t2 t3) =
    "if " ++ show t1 ++ " then " ++ show t2 ++ " else " ++ show t3
  show (App t1 t2) = showL t1 ++ " $ " ++ showR t2
    where
      showL (Var s) = s
      showL t@Abs {} = "(" ++ show t ++ ")"
      showL t@(App _ _) = show t
      showL t@IfThenElse {} = show t
      showR (Var s) = s
      showR t@Abs {} = show t
      showR t@(App _ _) = "(" ++ show t ++ ")"
      showR t@IfThenElse {} = show t

fullParser :: String -> Term
fullParser s =
  case lexer s of
    Left err -> error $ show err
    Right lexemes -> parseTerm lexemes
