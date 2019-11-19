{-# LANGUAGE OverloadedStrings #-}

import Reflex.Dom
import Untyped.Evaluator
import Untyped.Parser
import qualified Data.Text as T


processInput :: String -> String
processInput input =
  let parseResult = Untyped.Parser.fullParser input
   in case parseResult of
        Left err -> show err
        Right t -> reduceTerm t

quieter :: String -> String
quieter input = if null input then "" else processInput input

reduceTerm :: Term -> String
reduceTerm t =
  case eval t of
    Left err -> show err
    Right t' -> show t'

main :: IO ()
main = mainWidget $ el "div" $ do
  t <- textArea def
  el "div" $
    dynText $ (T.pack . quieter . T.unpack <$> _textArea_value t)
