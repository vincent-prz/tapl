{
module SimplyTyped.Parser where

import SimplyTyped.Lexer
}
%name parseTerm
%tokentype { Token }
%monad { Either String } { (>>=) } { return }
%error { parseError }

%token
        var { TOK_VAR $$ }        
        true { TOK_TRUE }
        false { TOK_FALSE }
        '0' { TOK_ZERO }
        succ { TOK_SUCC }
        pred { TOK_PRED }
        iszero { TOK_ISZERO }
        lambda { TOK_LAMBDA }        
        '.' { TOK_DOT }        
        '$' { TOK_DOLLAR }
        '(' { TOK_LEFT_PAREN }        
        ')' { TOK_RIGHT_PAREN }        
        if { TOK_IF }
        then { TOK_THEN }
        else { TOK_ELSE }
        ':' { TOK_COLON }        
        type { TOK_TYPE $$ }        

%nonassoc '.'
%nonassoc else
%nonassoc succ
%nonassoc pred
%nonassoc iszero
%left '$'
%%

Term    : var { Var $1 }
        | lambda var ':' type '.' Term { Abs $2 $4 $6 }
        | Term '$' Term { App $1 $3 }
        | '(' Term ')' { $2 }
        | true { ConstTrue }
        | false { ConstFalse }
        | if Term then Term else Term { IfThenElse $2 $4 $6 }
        | '0' { ConstZero }
        | succ Term { Succ $2 }
        | pred Term { Pred $2 }
        | iszero Term { IsZero $2 }
{
parseError :: [Token] -> Either String a
parseError _ = Left "Parse Error"

data Term
  = Var String
  | Abs String Type Term
  | App Term Term
  | ConstTrue
  | ConstFalse
  | IfThenElse Term Term Term
  | ConstZero
  | Succ Term
  | Pred Term
  | IsZero Term
  deriving (Eq)

instance Show Term where
  show (Var s) = s
  show (Abs s t b) = "\\" ++ s ++ ":" ++ show t ++ "." ++ show b
  show ConstTrue = "true"
  show ConstFalse = "false"
  show (IfThenElse t1 t2 t3) =
    "if " ++ show t1 ++ " then " ++ show t2 ++ " else " ++ show t3
  show ConstZero = "0"
  show (Succ t) = "succ " ++ show t
  show (Pred t) = "pred " ++ show t
  show (IsZero t) = "iszero " ++ show t
  show (App t1 t2) = showL t1 ++ " $ " ++ showR t2
    where
      showL (Var s) = s
      showL t@Abs {} = "(" ++ show t ++ ")"
      showL t@(App _ _) = show t
      showL t@IfThenElse {} = "(" ++ show t ++ ")"
      showL t@Succ {} = "(" ++ show t ++ ")"
      showL t@Pred {} = "(" ++ show t ++ ")"
      showL t@IsZero {} = "(" ++ show t ++ ")"
      showL t = show t
      showR t@(App _ _) = "(" ++ show t ++ ")"
      showR t = show t

fullParser :: String -> Either String Term
fullParser s =
  case lexer s of
    Left err -> Left $ show err
    Right lexemes -> parseTerm lexemes
}
