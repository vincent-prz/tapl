module SimplyTyped.Lexer where

import Data.Functor
import Text.ParserCombinators.Parsec

data Type
  = TBool
  | Arrow Type
          Type
  deriving (Eq)

instance Show Type where
  show TBool = "Bool"
  show (Arrow t1 t2) = show t1 ++ "->" ++ show t2

data Token
  = TOK_VAR String
  | TOK_LAMBDA
  | TOK_DOT
  | TOK_WHITESPACE
  | TOK_LEFT_PAREN
  | TOK_RIGHT_PAREN
  | TOK_IF
  | TOK_THEN
  | TOK_ELSE
  | TOK_COLON
  | TOK_TYPE Type
  deriving (Eq, Show)

parseVariable :: Parser Token
parseVariable = TOK_VAR <$> ((:) <$> lower <*> many alphaNum)

parseSingleType :: Parser Type
parseSingleType = string "Bool" $> TBool

parseArrowType :: Parser Type
parseArrowType =
  try (Arrow <$> parseSingleType <*> (string "->" *> parseArrowType)) <|>
  parseSingleType

parseType :: Parser Token
parseType = TOK_TYPE <$> parseArrowType

parseWhitespace :: Parser Token
parseWhitespace = many1 (oneOf [' ', '\t']) $> TOK_WHITESPACE

parseToken :: Parser Token
parseToken =
  choice
    [ try $ string "if" $> TOK_IF
    , try $ string "then" $> TOK_THEN
    , try $ string "else" $> TOK_ELSE
    , try parseVariable
    , try parseType
    , try parseWhitespace
    , try $ char '\\' $> TOK_LAMBDA
    , try $ char '.' $> TOK_DOT
    , try $ char '(' $> TOK_LEFT_PAREN
    , try $ char ')' $> TOK_RIGHT_PAREN
    , try $ string "if" $> TOK_IF
    , try $ string "then" $> TOK_THEN
    , try $ string "else" $> TOK_ELSE
    , try $ char ':' $> TOK_COLON
    ]

parseTokens :: Parser [Token]
parseTokens = many parseToken

lexer :: String -> Either ParseError [Token]
lexer input = parse (parseTokens <* eof) "lexing error" input
