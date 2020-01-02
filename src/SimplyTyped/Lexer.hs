module SimplyTyped.Lexer where

import Data.Functor
import Text.ParserCombinators.Parsec

-- TODO: put this and `Term` in a separate file.
data Type
  = TBool
  | Arrow Type
          Type
  deriving (Eq)

instance Show Type where
  show TBool = "Bool"
  show (Arrow t1 t2) = showL t1 ++ "->" ++ show t2
    where
      showL arr@(Arrow _ _) = "(" ++ show arr ++ ")"
      showL t = show t

data Token
  = TOK_VAR String
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

whitespace :: Parser String
whitespace = many (oneOf [' ', '\t'])

parseToken :: Parser Token
parseToken =
  choice
    [ try $ string "if" $> TOK_IF
    , try $ string "then" $> TOK_THEN
    , try $ string "else" $> TOK_ELSE
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
    ]

parseTokens :: Parser [Token]
parseTokens = many (parseToken <* whitespace)

lexer :: String -> Either ParseError [Token]
lexer input = parse (parseTokens <* eof) "lexing error" input
