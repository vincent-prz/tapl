{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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
  | ChangeVerbose Bool
  | ChangeEvalStrategy EvaluationStrategy
  | NoOp
  deriving (Eq)

defaultOpts :: Options
defaultOpts = Options True FullBeta

valueToEvaluationStrategy :: MisoString -> EvaluationStrategy
valueToEvaluationStrategy "0" = CallByValue
valueToEvaluationStrategy "1" = FullBeta
valueToEvaluationStrategy v =
  error $ "unrecognized value for evaluation strategy :" ++ fromMisoString v

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

-- FIXME: clunkyness with records
updateModel :: Action -> Model -> Effect Action Model
updateModel action m =
  case action of
    ChangeInput newInput -> noEff $ Model {opts = opts m, input = newInput}
    ChangeVerbose v ->
      noEff $ Model {opts = Options v (strategy (opts m)), input = input m}
    ChangeEvalStrategy strat ->
      noEff $ Model {opts = Options (verbose (opts m)) strat, input = input m}
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
            [ on "change" valueDecoder $
              ChangeVerbose . toEnum . read . fromMisoString
            ]
            [ option_ [selected_ $ verbose $ opts m, value_ "1"] ["Yes"]
            , option_ [selected_ $ not $ verbose $ opts m, value_ "0"] ["No"]
            ]
        ]
    , div_
        []
        [ p_ [] ["Evaluation mode:"]
        , select_
            [ on "change" valueDecoder $
              ChangeEvalStrategy . valueToEvaluationStrategy
            ]
            [ option_
                [selected_ (strategy (opts m) == CallByValue), value_ "0"]
                ["Call by value"]
            , option_
                [selected_ (strategy (opts m) == FullBeta), value_ "1"]
                ["Full Beta reduction"]
            ]
        ]
    , textarea_ [cols_ "50", defaultValue_ (input m), onInput ChangeInput] []
    , div_ [] (map viewSingleStep (quieter (opts m) (fromMisoString $ input m)))
    ]
