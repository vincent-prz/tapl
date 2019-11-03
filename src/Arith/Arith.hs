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

parseToken :: Parser Token
parseToken =
  choice
    [ string "true" $> TOK_TRUE
    , string "false" $> TOK_FALSE
    , string "if" $> TOK_IF
    , string "then" $> TOK_THEN
    , string "else" $> TOK_ELSE
    , char '0' $> TOK_ZERO
    , string "succ" $> TOK_SUCC
    , string "pred" $> TOK_PRED
    , string "iszero" $> TOK_IS_ZERO
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
  let l = tok' == tok
  if l
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
    guard p
    pure $ T_IS_ZERO n

parseAST :: ParserTok Term
parseAST =
  parseZero <|> parseTrue <|> parseFalse <|> parsePred <|> parseSucc -- <|>
  --parseIsZero

-- parsing + lexing
fullParser :: String -> Either ParseError Term
fullParser s = do
  lexemes <- lexer s
  parse parseAST "" lexemes

run :: IO ()
run = do
  input <- getLine
  print $ fullParser input
