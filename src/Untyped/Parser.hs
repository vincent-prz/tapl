module Untyped.Parser where

import Control.Monad
import Data.Functor
import Text.Parsec.Prim hiding (try)
import Text.ParserCombinators.Parsec

-- Lexing
data Token
  = TOK_VARIABLE String
  | TOK_LAMBDA
  | TOK_DOT
  | TOK_LEFT_PAREN
  | TOK_RIGHT_PAREN
  deriving (Eq, Show)

isVariable :: Token -> Bool
isVariable (TOK_VARIABLE _) = True
isVariable _ = False

parseVariable :: Parser Token
parseVariable = TOK_VARIABLE <$> many1 alphaNum

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
lexer input = parse (parseTokens) "lexing error" input

-- parse
data Term
  = T_VARIABLE String
  | T_ABSTRACTION Term
                  Term
  | T_APPLICATION Term
                  Term
  deriving (Eq)

instance Show Term where
  show (T_VARIABLE s) = s
  show (T_ABSTRACTION t1 t2) = "\\" ++ show t1 ++ "." ++ show t2
  show (T_APPLICATION t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"

type ParserTok a = Parsec [Token] () a

--- LEFT RECURSION ELIMINATION
parseExpression :: ParserTok Term
parseExpression = parseAbstraction

parseAbstraction :: ParserTok Term
parseAbstraction =
  try $ do
    boundedVars <- many parseBoundedVariable
    body <- parseApplication
    return $ foldr T_ABSTRACTION body boundedVars

parseBoundedVariable :: ParserTok Term
parseBoundedVariable =
  try $ do
    p1 <- (== TOK_LAMBDA) <$> anyToken
    boundVariable <- parseTokVariable
    p2 <- (== TOK_DOT) <$> anyToken
    guard p1
    guard p2
    return boundVariable

parseApplication :: ParserTok Term
parseApplication =
  try $ do
    operand <- primary
    args <- many primary
    return $ foldl T_APPLICATION operand args

primary = parseTokVariable <|> parseParens

parseTokVariable =
  try $ do
    tok <- anyToken
    case tok of
      TOK_VARIABLE s -> return (T_VARIABLE s)
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

-- END LEFT RECURSION ELIMINATION
parseAST :: ParserTok Term
parseAST = parseExpression <* eof

-- parsing + lexing
fullParser :: String -> Either ParseError Term
fullParser s = do
  lexemes <- lexer s
  parse parseAST "parsing error" lexemes
