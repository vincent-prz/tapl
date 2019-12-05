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

data TutorialState
  = TutorialOff
  | Level Int
  deriving (Eq)

data Model = Model
  { opts :: Options
  , input :: MisoString
  , tutorialState :: TutorialState
  } deriving (Eq)

-- | Sum type for application events
data Action
  = ChangeInput MisoString
  | ChangeVerbose Bool
  | ChangeEvalStrategy EvaluationStrategy
  | ToggleTutorial
  | NextLevel
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
    model =
      Model
        { opts = defaultOpts
        , input = "(\\x.x) \\y.y"
        , tutorialState = TutorialOff
        }
    update = updateModel -- update function
    view = viewModel -- view function
    events = defaultEvents -- default delegated events
    subs = [] -- empty subscription list
    mountPoint = Nothing -- mount point for application (Nothing defaults to 'body')

-- FIXME: clunkyness with records
updateModel :: Action -> Model -> Effect Action Model
updateModel action m =
  case action of
    ChangeInput newInput -> noEff $ m {input = newInput}
    ChangeVerbose v -> noEff $ m {opts = Options v (strategy (opts m))}
    ChangeEvalStrategy strat ->
      noEff $ m {opts = Options (verbose (opts m)) strat}
    ToggleTutorial ->
      noEff $
      m
        { tutorialState = toggleTutorialState (tutorialState m)
        , input =
            if tutorialState m == TutorialOff
              then getTutorialInput (Level 0)
              else input m
        }
    NextLevel ->
      let nl = nextLevel (tutorialState m)
       in noEff $ m {tutorialState = nl, input = getTutorialInput nl}
    NoOp -> noEff m

nextLevel :: TutorialState -> TutorialState
nextLevel TutorialOff = TutorialOff
nextLevel (Level n) = Level (n + 1)

toggleTutorialState :: TutorialState -> TutorialState
toggleTutorialState TutorialOff = Level 0
toggleTutorialState _ = TutorialOff

tutorialButtonLabel :: TutorialState -> MisoString
tutorialButtonLabel TutorialOff = "Start Tutorial"
tutorialButtonLabel (Level _) = "Stop Tutorial"

isTutorialOn :: TutorialState -> Bool
isTutorialOn = (/=) TutorialOff

getTutorialInput :: TutorialState -> MisoString
getTutorialInput TutorialOff = ""
getTutorialInput (Level 0) = "\\x.x"
getTutorialInput (Level 1) = "\\x.x x"
getTutorialInput (Level 2) = "(\\x.x) (\\x.x x)"
getTutorialInput (Level 3) = "\\x.\\y.y"
getTutorialInput (Level 4) = "(\\x.\\y.y) (\\x.x) \\z.z"
getTutorialInput (Level 5) = "\\x.\\y.\\z.(x y) z"
getTutorialInput (Level 6) = "\\x.x \\y.y"

viewTutorialText :: TutorialState -> View Action
viewTutorialText TutorialOff = div_ [] []
viewTutorialText (Level 0) = div_ [] [p_ [] ["simplest value"]]
viewTutorialText (Level 1) = div_ [] [p_ [] ["another example"]]
viewTutorialText (Level 2) = div_ [] [p_ [] ["application"]]
viewTutorialText (Level 3) = div_ [] [p_ [] ["2 parameters"]]
viewTutorialText (Level 4) = div_ [] [p_ [] ["2 parameters applied"]]
viewTutorialText (Level 5) =
  div_ [] [p_ [] ["left associativity of abstractions"]]
viewTutorialText (Level 6) =
  div_ [] [p_ [] ["abstractions extend as far right as possible"]]

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
    , div_
        []
        [ button_
            [onClick ToggleTutorial]
            [text $ tutorialButtonLabel (tutorialState m)]
        , button_ [onClick NextLevel] ["Next"]
        ]
    , viewTutorialText (tutorialState m)
    , textarea_ [cols_ "50", defaultValue_ (input m), onInput ChangeInput] []
    , div_ [] (map viewSingleStep (quieter (opts m) (fromMisoString $ input m)))
    ]
