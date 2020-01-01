{
module SimplyTyped.Parser where

import SimplyTyped.Lexer
}
%name parseTerm
%tokentype { Token }
%error { parseError }

%token
        var { TOK_VAR $$ }        
        lambda { TOK_LAMBDA }        
        '.' { TOK_DOT }        
        '(' { TOK_LEFT_PAREN }        
        ')' { TOK_RIGHT_PAREN }        
        ':' { TOK_COLON }        
        type { TOK_TYPE $$ }        
%%
Term    : var { Var $1 }
        | lambda var ':' type '.' Term { Abs $2 $4 $6 }
{
parseError :: [Token] -> a
parseError _ = error "Parse error"


data Term
  = Var String
  | Abs String Type Term
  | App Term Term
}
