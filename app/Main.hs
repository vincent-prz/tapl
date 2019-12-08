{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import TutorialText
import Untyped.Evaluator
import Untyped.Parser

import qualified Data.Map as Map
import Miso
import Miso.String hiding (map, null)

data EvalOpts = EvalOpts
  { verbose :: Bool
  , strategy :: EvaluationStrategy
  } deriving (Eq)

data TutorialState
  = TutorialOff
  | Level Int
  deriving (Eq)

maxLevel :: Int
maxLevel = 4

data Model = Model
  { opts :: EvalOpts
  , input :: MisoString
  , tutorialState :: TutorialState
  } deriving (Eq)

data Action
  = ChangeInput MisoString
  | ChangeVerbose Bool
  | ChangeEvalStrategy EvaluationStrategy
  | ToggleTutorial
  | PreviousLevel
  | NextLevel
  | NoOp
  deriving (Eq)

defaultOpts :: EvalOpts
defaultOpts = EvalOpts True FullBeta

valueToEvaluationStrategy :: MisoString -> EvaluationStrategy
valueToEvaluationStrategy "0" = CallByValue
valueToEvaluationStrategy "1" = FullBeta
valueToEvaluationStrategy v =
  error $ "unrecognized value for evaluation strategy :" ++ fromMisoString v

processInput :: EvalOpts -> String -> [String]
processInput opts input =
  let parseResult = Untyped.Parser.fullParser input
   in case parseResult of
        Left err -> [show err]
        Right t -> reduceTerm opts t

reduceTerm :: EvalOpts -> Term -> [String]
reduceTerm opts t =
  if verbose opts
    then case verboseEvalWithStrategy (strategy opts) t of
           Left err -> [show err]
           Right ts -> map show ts
    else case evalWithStrategy (strategy opts) t of
           Left err -> [show err]
           Right t' -> [show t']

quieter :: EvalOpts -> String -> [String]
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

getTutorialInput :: TutorialState -> MisoString
getTutorialInput TutorialOff = ""
getTutorialInput (Level 0) = "\\x.x"
getTutorialInput (Level 1) = "\\x.x x"
getTutorialInput (Level 2) = "(\\x.x) \\x.x x"
getTutorialInput (Level 3) = "(\\x.x) \\x.x x"
getTutorialInput (Level 4) = "\\x.\\y.y"
getTutorialInput (Level 5) = "(\\x.\\y.y) (\\x.x) \\z.z"
getTutorialInput (Level 6) = "\\x.\\y.\\z.(x y) z"
getTutorialInput (Level 7) = "\\x.x \\y.y"
getTutorialInput (Level n) = error $ "input not configured for " ++ show n

-- FIXME: clunkyness with records
updateModel :: Action -> Model -> Effect Action Model
updateModel action m =
  case action of
    ChangeInput newInput -> noEff $ m {input = newInput}
    ChangeVerbose v -> noEff $ m {opts = EvalOpts v (strategy (opts m))}
    ChangeEvalStrategy strat ->
      noEff $ m {opts = EvalOpts (verbose (opts m)) strat}
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
    PreviousLevel ->
      let pl = previousLevel (tutorialState m)
       in noEff $ m {tutorialState = pl, input = getTutorialInput pl}
    NoOp -> noEff m

nextLevel :: TutorialState -> TutorialState
nextLevel TutorialOff = TutorialOff
nextLevel (Level n) = Level (n + 1)

previousLevel :: TutorialState -> TutorialState
previousLevel TutorialOff = TutorialOff
previousLevel (Level 0) = Level 0
previousLevel (Level n) = Level (n - 1)

toggleTutorialState :: TutorialState -> TutorialState
toggleTutorialState TutorialOff = Level 0
toggleTutorialState _ = TutorialOff

tutorialButtonLabel :: TutorialState -> MisoString
tutorialButtonLabel TutorialOff = "Start Tutorial"
tutorialButtonLabel (Level _) = "Stop Tutorial"

displayLevel :: Int -> MisoString
displayLevel n = toMisoString ("Level " ++ show n)

viewTutorialText :: TutorialState -> View Action
viewTutorialText TutorialOff = div_ [] []
viewTutorialText (Level n) =
  case (,) <$> Map.lookup n levelToTitleMap <*> Map.lookup n levelToTextMap of
    Nothing -> error $ "title or text not defined for level " ++ show n
    Just (title, txt) ->
      div_
        []
        [ p_ [] [text (displayLevel n <> ": " <> title)]
        , p_ [style_ paragraphStyle] [text txt]
        ]

paragraphStyle :: Map.Map MisoString MisoString
paragraphStyle = Map.singleton "width" "50%"

--viewTutorialText (Level 1) = div_ [] [p_ [] ["another example"]]
--viewTutorialText (Level 2) = div_ [] [p_ [] ["application"]]
--viewTutorialText (Level 3) = div_ [] [p_ [] ["2 parameters"]]
--viewTutorialText (Level 4) = div_ [] [p_ [] ["2 parameters applied"]]
--viewTutorialText (Level 5) =
--  div_ [] [p_ [] ["left associativity of abstractions"]]
--viewTutorialText (Level 6) =
--  div_ [] [p_ [] ["abstractions extend as far right as possible"]]
--
getNextButtonStyle :: TutorialState -> Map.Map MisoString MisoString
getNextButtonStyle TutorialOff = Map.singleton "display" "none"
getNextButtonStyle (Level n)
  | n < maxLevel = Map.singleton "display" "inline"
  | otherwise = Map.singleton "display" "none"

getPreviousButtonStyle :: TutorialState -> Map.Map MisoString MisoString
getPreviousButtonStyle TutorialOff = Map.singleton "display" "none"
getPreviousButtonStyle (Level n)
  | n > 0 = Map.singleton "display" "inline"
  | otherwise = Map.singleton "display" "none"

viewSingleStep :: String -> View Action
viewSingleStep step = p_ [] [text $ toMisoString step]

viewModel :: Model -> View Action
viewModel m =
  div_
    []
    [ p_
        [style_ paragraphStyle]
        [ "This is a repl for the lambda calculus language. You can type any expression you like, or go through the tutorial to know more about lambda calculus, and build features that you would find in today's mainstream languages (eg Python, Javascript, Java ...) along the way. This is a work in progress. I hope you'll find it useful."
        ]
    , div_
        []
        [ button_
            [onClick ToggleTutorial]
            [text $ tutorialButtonLabel (tutorialState m)]
        , button_
            [ style_ (getPreviousButtonStyle (tutorialState m))
            , onClick PreviousLevel
            ]
            ["Previous"]
        , button_
            [style_ (getNextButtonStyle (tutorialState m)), onClick NextLevel]
            ["Next"]
        , span_ [] ["Print all steps ?"]
        , select_
            [ on "change" valueDecoder $
              ChangeVerbose . toEnum . read . fromMisoString
            ]
            [ option_ [selected_ $ verbose $ opts m, value_ "1"] ["Yes"]
            , option_ [selected_ $ not $ verbose $ opts m, value_ "0"] ["No"]
            ]
        , span_ [] ["Evaluation mode:"]
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
    , viewTutorialText (tutorialState m)
    , textarea_ [cols_ "50", defaultValue_ (input m), onInput ChangeInput] []
    , div_ [] (map viewSingleStep (quieter (opts m) (fromMisoString $ input m)))
    ]
