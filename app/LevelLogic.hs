{-# LANGUAGE OverloadedStrings #-}

module LevelLogic where

import Data.Either
import qualified Data.Map as Map
import Untyped.Evaluator
import Untyped.Parser

import Miso.String hiding (head, map, null)
import Text.ParserCombinators.Parsec

data Level = Level
  { lvlTitle :: MisoString
  , initialCode :: MisoString
  , lvlExcerpt :: MisoString
  , expectations :: [Expectation]
  }

levels :: Map.Map Int Level
levels =
  Map.fromList
    [ ( 0
      , Level
          { lvlTitle = "the identity function"
          , initialCode = "\\x.x"
          , lvlExcerpt = "hello world"
          , expectations = []
          })
    , ( 1
      , Level
          { lvlTitle = "Church booleans"
          , initialCode = "true = \\t.\\f.t\nfalse = \\t.\\f.f"
          , lvlExcerpt = "write a `not` function."
          , expectations =
              rights
                [ buildExpectation "\\t.\\f.t" "\\t.\\f.f"
                , buildExpectation "\\t.\\f.f" "\\t.\\f.t"
                ]
          })
    , ( 2
      , Level
          { lvlTitle = "Church booleans: AND"
          , initialCode =
              "true = \\t.\\f.t\nfalse = \\t.\\f.f\nand = \\a.\\b.a b a\nand true false"
          , lvlExcerpt = "hello booleans 2"
          , expectations = []
          })
    , ( 3
      , Level
          { lvlTitle = "Church booleans: OR"
          , initialCode =
              "true = \\t.\\f.t\nfalse = \\t.\\f.f\nand = \\a.\\b.a b a\n"
          , lvlExcerpt = "can you implement OR? Define a `or` function"
          , expectations = []
          })
    ]

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

testSubmission :: Program -> [Expectation] -> Either String ()
testSubmission p es =
  let (runtimeErrs, others) = partitionEithers $ map (testExpectation p) es
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

testExpectation ::
     Program -> Expectation -> Either RuntimeError ExpectationResult
testExpectation p e = do
  submittedTerm <- evalProgramFinalResult FullBeta p
  let application = Program [Run (T_APP submittedTerm (argument e))]
  actualVal <- evalProgramFinalResult FullBeta application
  return $ ExpectationResult {expectation = e, actual = actualVal}