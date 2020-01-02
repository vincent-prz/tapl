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
        '$' { TOK_DOLLAR }
        '(' { TOK_LEFT_PAREN }        
        ')' { TOK_RIGHT_PAREN }        
        if { TOK_IF }
        then { TOK_THEN }
        else { TOK_ELSE }
        ':' { TOK_COLON }        
        type { TOK_TYPE $$ }        

%left '$'
%%

Term    : var { Var $1 }
        | lambda var ':' type '.' Term { Abs $2 $4 $6 }
        | Term '$' Term { App $1 $3 }
        | '(' Term ')' { $2 }
        | if Term then Term else Term { IfThenElse $2 $4 $6 }
{
parseError :: [Token] -> a
parseError _ = error "Parse error"


data Term
  = Var String
  | Abs String Type Term
  | App Term Term
  | IfThenElse Term Term Term
}
