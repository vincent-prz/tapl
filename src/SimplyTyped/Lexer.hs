module SimplyTyped.Lexer(
  Token(..),
  lexer
) where

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
  | TOK_LAMBDA
  | TOK_DOT
  | TOK_DOLLAR
  | TOK_LEFT_PAREN
  | TOK_RIGHT_PAREN
  | TOK_IF
  | TOK_THEN
  | TOK_ELSE
  | TOK_COLON
  | TOK_TYPE Type
  | TOK_SEMICOLON
  | TOK_WILDCARD
  | TOK_AS
  deriving (Eq, Show)

parseVariable :: Parser Token
parseVariable = TOK_VAR <$> ((:) <$> lower <*> many alphaNum)

parseSingleType :: Parser Type
parseSingleType =
  string "Bool" $> TBool <|> string "Nat" $> TNat <|> string "Unit" $> TUnit

parseArrowType :: Parser Type
parseArrowType =
  try (Arrow <$> parseSingleType <*> (string "->" *> parseArrowType)) <|>
  parseSingleType

parseType :: Parser Token
parseType = TOK_TYPE <$> parseArrowType

whitespace :: Parser String
whitespace = many (oneOf [' ', '\t'])

parseToken :: Parser Token
parseToken =
  choice
    [ try $ string "true" $> TOK_TRUE
    , try $ string "false" $> TOK_FALSE
    , try $ string "if" $> TOK_IF
    , try $ string "then" $> TOK_THEN
    , try $ string "else" $> TOK_ELSE
    , try $ char '0' $> TOK_ZERO
    , try $ string "succ" $> TOK_SUCC
    , try $ string "pred" $> TOK_PRED
    , try $ string "iszero" $> TOK_ISZERO
    , try $ string "as" $> TOK_AS
    , try parseVariable
    , try parseType
    , try $ char '$' $> TOK_DOLLAR
    , try $ char '\\' $> TOK_LAMBDA
    , try $ char '.' $> TOK_DOT
    , try $ char '(' $> TOK_LEFT_PAREN
    , try $ char ')' $> TOK_RIGHT_PAREN
    , try $ string "if" $> TOK_IF
    , try $ string "then" $> TOK_THEN
    , try $ string "else" $> TOK_ELSE
    , try $ char ':' $> TOK_COLON
    , try $ char ';' $> TOK_SEMICOLON
    , try $ char '_' $> TOK_WILDCARD
    ]

parseTokens :: Parser [Token]
parseTokens = many (parseToken <* whitespace)

lexer :: String -> Either ParseError [Token]
lexer = parse (parseTokens <* eof) "Lexing Error"
