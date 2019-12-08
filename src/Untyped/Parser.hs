module Untyped.Parser where

import Control.Monad
import Data.Functor
import Data.List (intercalate)
import Text.Parsec.Prim hiding (try)
import Text.ParserCombinators.Parsec

-- Lexing
data Token
  = TOK_VAR String
  | TOK_LAMBDA
  | TOK_DOT
  | TOK_LEFT_PAREN
  | TOK_RIGHT_PAREN
  | TOK_ASSIGN
  | TOK_NEWLINE
  deriving (Eq, Show)

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
    , try $ char '=' $> TOK_ASSIGN
    , try $ char '\n' $> TOK_NEWLINE
    ]

whitespace :: Parser String
whitespace = many (oneOf [' ', '\t'])

parseTokens :: Parser [Token]
parseTokens = many (parseToken <* whitespace)

lexer :: String -> Either ParseError [Token]
lexer input = parse (parseTokens <* eof) "lexing error" input

-- parse
newtype Program =
  Program [Statement]

instance Show Program where
  show (Program stmts) = intercalate "\n" (map show stmts)

data Statement
  = Assign String
           Term
  | Run Term

instance Show Statement where
  show (Run s) = show s
  show (Assign x t) = x ++ " = " ++ show t

-- TODO: T_ABS should be String -> Term -> Term
data Term
  = T_VAR String
  | T_ABS Term
          Term
  | T_APP Term
          Term
  | T_UNIT
  deriving (Eq)

instance Show Term where
  show (T_VAR s) = s
  show (T_ABS t1 t2) = "\\" ++ show t1 ++ "." ++ show t2
  show T_UNIT = "()"
  show (T_APP t1 t2) = showL t1 ++ " " ++ showR t2
    where
      showL (T_VAR s) = s
      showL t@(T_ABS _ _) = "(" ++ show t ++ ")"
      showL t@(T_APP _ _) = show t
      showR (T_VAR s) = s
      showR t@(T_ABS _ _) = show t
      showR t@(T_APP _ _) = "(" ++ show t ++ ")"

type ParserTok a = Parsec [Token] () a

parseTerm :: ParserTok Term
parseTerm =
  parseAbstraction <|> parseApplication <|> parseTokVar <|> parseParens

parseAbstraction :: ParserTok Term
parseAbstraction =
  try $ do
    p1 <- (== TOK_LAMBDA) <$> anyToken
    boundVariable <- parseTokVar
    p2 <- (== TOK_DOT) <$> anyToken
    guard p1
    guard p2
    body <- parseTerm
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
    expr <- parseTerm
    p2 <- (== TOK_RIGHT_PAREN) <$> anyToken
    guard p1
    guard p2
    return expr

parseAssign :: ParserTok Statement
parseAssign =
  try $ do
    tok <- anyToken
    p1 <- (== TOK_ASSIGN) <$> anyToken
    term <- parseTerm
    guard p1
    case tok of
      TOK_VAR s -> return (Assign s term)
      _ -> fail $ "attempted parsing variable, but got " ++ show tok

parseStatement :: ParserTok Statement
parseStatement = parseAssign <|> (Run <$> parseTerm)

parseNewLine :: ParserTok ()
parseNewLine =
  try $ do
    p1 <- (== TOK_NEWLINE) <$> anyToken
    guard p1

parseProgram :: ParserTok Program
parseProgram = (Program <$> many (parseStatement <* many parseNewLine)) <* eof

-- parsing + lexing
fullParser :: String -> Either ParseError Program
fullParser s = do
  lexemes <- lexer s
  parse parseProgram "parsing error" lexemes
