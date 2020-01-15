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
%left '$'
%%

Term    : var { Var $1 }
        | lambda var ':' type '.' Term { Abs $2 $4 $6 }
        | Term '$' Term { App $1 $3 }
        | '(' Term ')' { $2 }
        | true { ConstTrue }
        | false { ConstFalse }
        | if Term then Term else Term { IfThenElse $2 $4 $6 }
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
  deriving (Eq)

instance Show Term where
  show (Var s) = s
  show (Abs s t b) = "\\" ++ s ++ ":" ++ show t ++ "." ++ show b
  show ConstTrue = "true"
  show ConstFalse = "false"
  show (IfThenElse t1 t2 t3) =
    "if " ++ show t1 ++ " then " ++ show t2 ++ " else " ++ show t3
  show (App t1 t2) = showL t1 ++ " $ " ++ showR t2
    where
      showL (Var s) = s
      showL t@Abs {} = "(" ++ show t ++ ")"
      showL t@(App _ _) = show t
      showL ConstTrue = "true"
      showL ConstFalse = "false"
      showL t@IfThenElse {} = "(" ++ show t ++ ")"
      showR (Var s) = s
      showR t@Abs {} = show t
      showR t@(App _ _) = "(" ++ show t ++ ")"
      showR ConstTrue = "true"
      showR ConstFalse = "false"
      showR t@IfThenElse {} = show t

fullParser :: String -> Either String Term
fullParser s =
  case lexer s of
    Left err -> Left $ show err
    Right lexemes -> parseTerm lexemes
}
