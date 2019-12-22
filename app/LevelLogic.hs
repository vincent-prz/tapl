{-# LANGUAGE OverloadedStrings #-}

module LevelLogic where

import Data.Either
import Untyped.Evaluator
import Untyped.Parser

import Miso.String hiding (foldl, head, map, null)
import Text.ParserCombinators.Parsec

data Level = Level
  { lvlTitle :: MisoString
  , initialCode :: MisoString
  , lvlExcerpt :: MisoString
  , expectations :: [Expectation]
  }

data Expectation = Expectation
  { arguments :: [Term]
  , expectedResult :: Term
  }

data ExpectationResult = ExpectationResult
  { expectation :: Expectation
  , actual :: Term
  }

buildExpectation :: [String] -> String -> Either ParseError Expectation
buildExpectation args expected =
  Expectation <$> mapM singleTermParser args <*> singleTermParser expected

isSuccessful :: ExpectationResult -> Bool
isSuccessful er = expectedResult (expectation er) == actual er

processSubmission :: Level -> String -> Either String ()
processSubmission lvl input =
  case fullParser input of
    Left err -> Left $ show err
    Right p ->
      case evalProgramFinalResult FullBeta p of
        Left err -> Left $ show err
        Right T_UNIT -> Left "Error: submission must end with expression"
        Right t -> testSubmission t (expectations lvl)

testSubmission :: Term -> [Expectation] -> Either String ()
testSubmission t es =
  let (runtimeErrs, others) = partitionEithers $ map (testExpectation t) es
   in case runtimeErrs of
        [] -> aux others
        (err:_) -> Left (show err)
  where
    aux :: [ExpectationResult] -> Either String ()
    aux [] = Right ()
    aux (er:ers) =
      if isSuccessful er
        then aux ers
        else Left $
             "Expected " ++
             expected ++ ", but got " ++ got ++ " when applied on " ++ args
      where
        expected = show (expectedResult $ expectation er)
        got = show $ actual er
        args = show (arguments $ expectation er)

testExpectation :: Term -> Expectation -> Either RuntimeError ExpectationResult
testExpectation t e = do
  let application = Program [Run (foldl T_APP t (arguments e))]
  actualVal <- evalProgramFinalResult FullBeta application
  return $ ExpectationResult {expectation = e, actual = actualVal}
