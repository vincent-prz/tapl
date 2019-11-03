module Arith.Arith where

import Control.Monad
import Data.Functor
import Data.Functor.Identity
import Text.Parsec.Prim hiding (try)
import Text.ParserCombinators.Parsec

-- Lexing
data Token
  = TOK_TRUE
  | TOK_FALSE
  | TOK_IF
  | TOK_THEN
  | TOK_ELSE
  | TOK_ZERO
  | TOK_SUCC
  | TOK_PRED
  | TOK_IS_ZERO
  deriving (Show, Eq)

-- TODO: how to avoid trying everywhere ?
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
    , try $ string "iszero" $> TOK_IS_ZERO
    ]

whitespace :: Parser String
whitespace = many (oneOf [' ', '\t', '\n'])

parseTokens :: Parser [Token]
parseTokens = many (parseToken <* whitespace)

lexer :: String -> Either ParseError [Token]
lexer input = parse (parseTokens <* eof) "" input

-- parse
data Term
  = T_TRUE
  | T_FALSE
  | T_ZERO
  | T_SUCC Term
  | T_PRED Term
  | T_IS_ZERO Term
  | T_IF_THEN_ELSE Term
                   Term
                   Term
  deriving (Show, Eq)

type ParserTok a = ParsecT [Token] () Identity a

parseTerm :: Token -> Term -> ParserTok Term
parseTerm tok term = do
  tok' <- try anyToken
  if tok' == tok
    then pure term
    else fail $ "attempted parsing: " ++ show tok ++ " and got: " ++ show tok'

parseTrue :: ParserTok Term
parseTrue = try $ parseTerm TOK_TRUE T_TRUE

parseFalse :: ParserTok Term
parseFalse = try $ parseTerm TOK_FALSE T_FALSE

parseZero :: ParserTok Term
parseZero = try $ parseTerm TOK_ZERO T_ZERO

parseSucc :: ParserTok Term
parseSucc =
  try $ do
    p <- (== TOK_SUCC) <$> try anyToken
    n <- parseAST
    guard p
    pure $ T_SUCC n

parsePred :: ParserTok Term
parsePred =
  try $ do
    p <- (== TOK_PRED) <$> try anyToken
    n <- parseAST
    guard p
    pure $ T_PRED n

parseIsZero :: ParserTok Term
parseIsZero =
  try $ do
    p <- (== TOK_IS_ZERO) <$> try anyToken
    n <- parseAST
    guard p -- TODO: what is guard doing here ?
    pure $ T_IS_ZERO n

parseIfThenElse :: ParserTok Term
parseIfThenElse =
  try $ do
    p1 <- (== TOK_IF) <$> try anyToken
    t1 <- parseAST
    p2 <- (== TOK_THEN) <$> try anyToken
    t2 <- parseAST
    p3 <- (== TOK_ELSE) <$> try anyToken
    t3 <- parseAST
    guard p1
    guard p2
    guard p3
    pure $ T_IF_THEN_ELSE t1 t2 t3

parseAST :: ParserTok Term
parseAST =
  parseZero <|> parseTrue <|> parseFalse <|> parsePred <|> parseSucc <|>
  parseIsZero <|>
  parseIfThenElse

-- parsing + lexing
fullParser :: String -> Either ParseError Term
fullParser s = do
  lexemes <- lexer s
  parse parseAST "" lexemes

run :: IO ()
run = do
  input <- getLine
  print $ fullParser input
