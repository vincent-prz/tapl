{-# LANGUAGE OverloadedStrings #-}

import Data.List (intercalate)
import Data.Map (Map, fromList)
import qualified Data.Text as T
import Reflex.Dom
import Untyped.Evaluator
import Untyped.Parser

data Options = Options
  { verbose :: Bool
  , strategy :: EvaluationStrategy
  }

defaultOptions :: Options
defaultOptions = Options True FullBeta

processInput :: Options -> String -> [String]
processInput opts input =
  let parseResult = Untyped.Parser.fullParser input
   in case parseResult of
        Left err -> [show err]
        Right t -> reduceTerm opts t

reduceTerm :: Options -> Term -> [String]
reduceTerm opts t =
  if verbose opts
    then case verboseEvalWithStrategy (strategy opts) t of
           Left err -> [show err]
           Right ts -> map show ts
    else case evalWithStrategy (strategy opts) t of
           Left err -> [show err]
           Right t' -> [show t']

quieter :: Options -> String -> [String]
quieter opts input =
  if null input
    then []
    else processInput opts input

evalStrategies :: Map EvaluationStrategy T.Text
evalStrategies =
  fromList [(FullBeta, "Full beta reduction"), (CallByValue, "Call by value")]

verbosityChoices :: Map Bool T.Text
verbosityChoices = fromList [(False, "No"), (True, "Yes")]

outputComponent ::
     (DomBuilder t m, PostBuild t m)
  => Dynamic t T.Text
  -> Dynamic t Bool
  -> Dynamic t EvaluationStrategy
  -> m ()
outputComponent text verbosity strategy =
  el "p" $
  dynText
    ((\input v strat
        --T.pack $ quieter (Options v strat) $ T.unpack input) <$>
       -> T.pack $ const "hello" $ T.unpack input) <$>
     text <*>
     verbosity <*>
     strategy)

main :: IO ()
main =
  mainWidget $
  el "div" $ do
    el "p" $
      text "Type in a lambda calculus expression. Example: (\\x.\\y.x y) \\x.x"
    el "p" $ text "Print all reduction steps:"
    dVerbose <-
      dropdown (verbose defaultOptions) (constDyn verbosityChoices) def
    el "p" $ text "Evaluation strategy:"
    dEvalStrat <-
      dropdown (strategy defaultOptions) (constDyn evalStrategies) def
    el "div" $ do
      t <- textArea def
      outputComponent
        (_textArea_value t)
        (_dropdown_value dVerbose)
        (_dropdown_value dEvalStrat)
      --el "p" $
      --  dynText
      --    ((\input verbosity strat ->
      --        T.pack $ quieter (Options verbosity strat) $ T.unpack input) <$>
      --     _textArea_value t <*>
      --     _dropdown_value dVerbose <*>
      --     _dropdown_value dEvalStrat)
