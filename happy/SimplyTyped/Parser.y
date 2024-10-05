{
module SimplyTyped.Parser where

import SimplyTyped.Lexer
import SimplyTyped.Definitions
}
%name parseTerms
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
        ';' { TOK_SEMICOLON }
        '_' { TOK_WILDCARD }
        type { TOK_TYPE $$ }
        as { TOK_AS }
        let { TOK_LET }
        '=' { TOK_EQUAL }
        'in' { TOK_IN }
        ',' { TOK_COMMA }
        number { TOK_NUMBER $$ }

%nonassoc '.'
%nonassoc else
%nonassoc succ
%nonassoc pred
%nonassoc iszero
%nonassoc as
%nonassoc 'in'
%left '$'
%%

Terms   : Term { [$1] }
        | Term ';' Terms { $1 : $3 }

Term    : var { Var $1 }
        | lambda var ':' type '.' Term { Abs (Just $2) $4 $6 }
        | lambda '_' ':' type '.' Term { Abs Nothing $4 $6 }
        | Term '$' Term { App $1 $3 }
        | '(' Term ')' { $2 }
        | true { ConstTrue }
        | false { ConstFalse }
        | if Term then Term else Term { IfThenElse $2 $4 $6 }
        | '0' { ConstZero }
        | succ Term { Succ $2 }
        | pred Term { Pred $2 }
        | iszero Term { IsZero $2 }
        | '('')' { ConstUnit }
        | Term as type { Ascription $1 $3 }
        | let var '=' Term 'in' Term { LetExpr $2 $4 $6 }
        | '(' TupleElems ')' { Tuple $2 }
        | Term '.' number { Projection $1 $3 }

TupleElems : Term ',' Term { [$1, $3] }
           | Term ',' TupleElems { $1 : $3 }

{
parseError :: [Token] -> Either String a
parseError _ = Left "Parse Error"

fullParser :: String -> Either String [Term]
fullParser s =
  case lexer s of
    Left err -> Left $ show err
    Right lexemes -> parseTerms lexemes
}
