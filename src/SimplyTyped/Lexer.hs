module SimplyTyped.Lexer
  ( Token (..),
    lexer,
  )
where

import Data.Char (digitToInt)
import Data.Functor
import SimplyTyped.Definitions
import Text.ParserCombinators.Parsec

-- FIXME: ambiguity in succ succ t
data Token
  = TOK_VAR String
  | TOK_TRUE
  | TOK_FALSE
  | TOK_ZERO
  | TOK_SUCC
  | TOK_PRED
  | TOK_ISZERO
  | TOK_BOOL
  | TOK_NAT
  | TOK_UNIT
  | TOK_LAMBDA
  | TOK_DOT
  | TOK_DOLLAR
  | TOK_LEFT_PAREN
  | TOK_RIGHT_PAREN
  | TOK_IF
  | TOK_THEN
  | TOK_ELSE
  | TOK_COLON
  | TOK_SEMICOLON
  | TOK_WILDCARD
  | TOK_AS
  | TOK_LET
  | TOK_EQUAL
  | TOK_IN
  | TOK_COMMA
  | TOK_ARROW
  | TOK_NUMBER Int
  deriving (Eq, Show)

parseVariable :: Parser Token
parseVariable = TOK_VAR <$> ((:) <$> lower <*> many alphaNum)

whitespace :: Parser String
whitespace = many (oneOf [' ', '\t'])

parseToken :: Parser Token
parseToken =
  choice
    [ try $ string "true" $> TOK_TRUE,
      try $ string "false" $> TOK_FALSE,
      try $ string "if" $> TOK_IF,
      try $ string "then" $> TOK_THEN,
      try $ string "else" $> TOK_ELSE,
      try $ char '0' $> TOK_ZERO,
      try $ string "succ" $> TOK_SUCC,
      try $ string "pred" $> TOK_PRED,
      try $ string "iszero" $> TOK_ISZERO,
      try $ string "Bool" $> TOK_BOOL,
      try $ string "Nat" $> TOK_NAT,
      try $ string "Unit" $> TOK_UNIT,
      try $ string "as" $> TOK_AS,
      try $ string "let" $> TOK_LET,
      try $ string "in" $> TOK_IN,
      try parseVariable,
      try $ char '$' $> TOK_DOLLAR,
      try $ char '\\' $> TOK_LAMBDA,
      try $ char '.' $> TOK_DOT,
      try $ char '(' $> TOK_LEFT_PAREN,
      try $ char ')' $> TOK_RIGHT_PAREN,
      try $ string "if" $> TOK_IF,
      try $ string "then" $> TOK_THEN,
      try $ string "else" $> TOK_ELSE,
      try $ char ':' $> TOK_COLON,
      try $ char ';' $> TOK_SEMICOLON,
      try $ char '_' $> TOK_WILDCARD,
      try $ char '=' $> TOK_EQUAL,
      try $ char ',' $> TOK_COMMA,
      try $ string "->" $> TOK_ARROW,
      try $ TOK_NUMBER . digitToInt <$> digit
    ]

parseTokens :: Parser [Token]
parseTokens = many (parseToken <* whitespace)

lexer :: String -> Either ParseError [Token]
lexer = parse (parseTokens <* eof) "Lexing Error"
