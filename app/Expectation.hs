module Expectation where

import Data.Either
import Text.ParserCombinators.Parsec
import Untyped.Evaluator
import Untyped.Parser

data Expectation = Expectation
  { argument :: Term
  , expectedResult :: Term
  }

data ExpectationResult = ExpectationResult
  { expectation :: Expectation
  , actual :: Term
  }

buildExpectation :: String -> String -> Either ParseError Expectation
buildExpectation arg expected =
  Expectation <$> singleTermParser arg <*> singleTermParser expected

isSuccessful :: ExpectationResult -> Bool
isSuccessful er = expectedResult (expectation er) == actual er

testSubmission :: Term -> [Expectation] -> Either String ()
testSubmission t es =
  let (runtimeErrs, others) = partitionEithers $ map (testExpectation t) es
   in if not (null runtimeErrs)
        then Left $ show $ head runtimeErrs
        else aux others
  where
    aux :: [ExpectationResult] -> Either String ()
    aux [] = Right ()
    aux (er:ers) =
      if isSuccessful er
        then aux ers
        else Left $
             "Expected " ++
             expected ++ ", but got " ++ got ++ " when applied on " ++ arg
      where
        expected = show (expectedResult $ expectation er)
        got = show $ actual er
        arg = show (argument $ expectation er)

testExpectation :: Term -> Expectation -> Either RuntimeError ExpectationResult
testExpectation t e =
  let application = Program [Run $ T_APP t $ argument e]
      actualEval = evalProgramFinalResult FullBeta application
   in ExpectationResult e <$> actualEval
