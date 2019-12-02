{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Data.List (intercalate)
import Untyped.Evaluator
import Untyped.Parser

import Miso hiding (Options)
import Miso.String hiding (map, null)

data Options = Options
  { verbose :: Bool
  , strategy :: EvaluationStrategy
  } deriving (Eq)

data Model = Model
  { opts :: Options
  , input :: MisoString
  } deriving (Eq)

-- | Sum type for application events
data Action
  = ChangeInput MisoString
  | ChangeVerbose MisoString
  | ChangeEvalStrategy MisoString
  | NoOp
  deriving (Show, Eq)

defaultOpts :: Options
defaultOpts = Options True FullBeta

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

main :: IO ()
main = startApp App {..}
  where
    initialAction = NoOp
    model = Model {opts = defaultOpts, input = "(\\x.x) \\y.y"}
    update = updateModel -- update function
    view = viewModel -- view function
    events = defaultEvents -- default delegated events
    subs = [] -- empty subscription list
    mountPoint = Nothing -- mount point for application (Nothing defaults to 'body')

-- FIXME: partial functions + clunkyness with records
updateModel :: Action -> Model -> Effect Action Model
updateModel action m =
  case action of
    ChangeInput newInput -> noEff $ Model {opts = opts m, input = newInput}
    ChangeVerbose "0" ->
      noEff $ Model {opts = Options True (strategy (opts m)), input = input m}
    ChangeVerbose "1" ->
      noEff $ Model {opts = Options False (strategy (opts m)), input = input m}
    ChangeEvalStrategy "0" ->
      noEff $
      Model {opts = Options (verbose (opts m)) CallByValue, input = input m}
    ChangeEvalStrategy "1" ->
      noEff $
      Model {opts = Options (verbose (opts m)) FullBeta, input = input m}
    NoOp -> noEff m

viewSingleStep :: String -> View Action
viewSingleStep step = p_ [] [text $ toMisoString step]

viewModel :: Model -> View Action
viewModel m =
  div_
    []
    [ p_ [] ["Type in a lambda calculus expression."]
    , div_
        []
        [ p_ [] ["Print all steps ?"]
        , select_
            [onChange ChangeVerbose]
            [option_ [value_ "0"] ["Yes"], option_ [value_ "1"] ["No"]]
        ]
    , div_
        []
        [ p_ [] ["Evaluation mode:"]
        , select_
            [onChange ChangeEvalStrategy]
            [ option_ [value_ "0"] ["Call by value"]
            , option_ [value_ "1"] ["Full Beta reduction"]
            ]
        ]
    , textarea_ [onInput ChangeInput] []
    , div_ [] (map viewSingleStep (quieter (opts m) (fromMisoString $ input m)))
    ]
