{-# LANGUAGE OverloadedStrings #-}

import Data.List (intercalate)
import qualified Data.Text as T
import Reflex.Dom
import Untyped.Evaluator
import Untyped.Parser

data Options = Options
  { verbose :: Bool
  , strategy :: EvaluationStrategy
  }

defaultOptions :: Options
defaultOptions = Options False CallByValue

processInput :: Options -> String -> String
processInput opts input =
  let parseResult = Untyped.Parser.fullParser input
   in case parseResult of
        Left err -> show err
        Right t -> reduceTerm opts t

reduceTerm :: Options -> Term -> String
reduceTerm opts t =
  if verbose opts
    then case verboseEvalWithStrategy (strategy opts) t of
           Left err -> show err
           Right ts -> intercalate "\n" (map show ts)
    else case evalWithStrategy (strategy opts) t of
           Left err -> show err
           Right t' -> show t'

quieter :: Options -> String -> String
quieter opts input =
  if null input
    then ""
    else processInput opts input

main :: IO ()
main =
  mainWidget $
  el "div" $ do
    el "p" $
      text "Type in a lambda calculus expression. Example: (\\x.\\y.x y) \\x.x"
    t <- textArea def
    el "div" $
      dynText (T.pack . quieter defaultOptions . T.unpack <$> _textArea_value t)
