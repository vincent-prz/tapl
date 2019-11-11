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
lexer input = parse (parseTokens <* eof) "lexing error" input

-- parse
data Term
  = T_VARIABLE String
  | T_ABSTRACTION Term
  | T_APPLICATION Term
                  Term
  deriving (Eq, Show)

type ParserTok a = Parsec [Token] () a

parseTokVariable :: ParserTok Term
parseTokVariable =
  try $ do
    tok <- anyToken
    case tok of
      TOK_VARIABLE s -> pure (T_VARIABLE s)
      _ -> fail $ "attempted parsing variable, but got " ++ show tok

parseAbstraction :: ParserTok Term
parseAbstraction =
  try $ do
    p1 <- (== TOK_LAMBDA) <$> anyToken
    p2 <- isVariable <$> anyToken
    p3 <- (== TOK_DOT) <$> anyToken
    guard p1
    guard p2
    guard p3
    T_ABSTRACTION <$> parseAST

parseApplication :: ParserTok Term
parseApplication =
  let parseVarOrAbs = parseTokVariable <|> parseAbstraction
   in try $ T_APPLICATION <$> parseVarOrAbs <*> parseVarOrAbs

-- need to parseApplication first to prevent it from parsing the left term of
-- an application as variable or abstraction
parseAST :: ParserTok Term
parseAST = parseApplication <|> parseTokVariable <|> parseAbstraction

-- parsing + lexing
fullParser :: String -> Either ParseError Term
fullParser s = do
  lexemes <- lexer s
  parse parseAST "parsing error" lexemes
