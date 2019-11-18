module Untyped.Parser where

import Control.Monad
import Data.Functor
import Text.Parsec.Prim hiding (try)
import Text.ParserCombinators.Parsec

-- Lexing
data Token
  = TOK_VAR String
  | TOK_LAMBDA
  | TOK_DOT
  | TOK_LEFT_PAREN
  | TOK_RIGHT_PAREN
  deriving (Eq, Show)

isVariable :: Token -> Bool
isVariable (TOK_VAR _) = True
isVariable _ = False

parseVariable :: Parser Token
parseVariable = TOK_VAR <$> many1 alphaNum

parseToken :: Parser Token
parseToken =
  choice
    [ try parseVariable
    , try $ char '\\' $> TOK_LAMBDA
    , try $ char '.' $> TOK_DOT
    , try $ char '(' $> TOK_LEFT_PAREN
    , try $ char ')' $> TOK_RIGHT_PAREN
    ]

whitespace :: Parser String
whitespace = many (oneOf [' ', '\t', '\n'])

parseTokens :: Parser [Token]
parseTokens = many (parseToken <* whitespace)

lexer :: String -> Either ParseError [Token]
lexer input = parse (parseTokens <* eof) "lexing error" input

-- parse
data Term
  = T_VAR String
  | T_ABS Term
          Term
  | T_APP Term
          Term
  deriving (Eq)

instance Show Term where
  show (T_VAR s) = s
  show (T_ABS t1 t2) = "\\" ++ show t1 ++ "." ++ show t2
  show (T_APP t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"

type ParserTok a = Parsec [Token] () a

parseExpression :: ParserTok Term
parseExpression =
  parseAbstraction <|> parseApplication <|> parseTokVar <|> parseParens

parseAbstraction :: ParserTok Term
parseAbstraction =
  try $ do
    p1 <- (== TOK_LAMBDA) <$> anyToken
    boundVariable <- parseTokVar
    p2 <- (== TOK_DOT) <$> anyToken
    guard p1
    guard p2
    body <- parseExpression
    return $ T_ABS boundVariable body

-- remove left recursion and keep associativity at the same time.
-- See https://craftinginterpreters.com/parsing-expressions.html
parseApplication :: ParserTok Term
parseApplication =
  try $ do
    operand <- primary
    args <- many primary
    return $ foldl T_APP operand args

primary = parseTokVar <|> parseParens <|> parseAbstraction

parseTokVar :: ParserTok Term
parseTokVar =
  try $ do
    tok <- anyToken
    case tok of
      TOK_VAR s -> return (T_VAR s)
      _ -> fail $ "attempted parsing variable, but got " ++ show tok

parseParens :: ParserTok Term
parseParens =
  try $ do
    p1 <- (== TOK_LEFT_PAREN) <$> anyToken
    expr <- parseExpression
    p2 <- (== TOK_RIGHT_PAREN) <$> anyToken
    guard p1
    guard p2
    return expr

parseAST :: ParserTok Term
parseAST = parseExpression <* eof

-- parsing + lexing
fullParser :: String -> Either ParseError Term
fullParser s = do
  lexemes <- lexer s
  parse parseAST "parsing error" lexemes
