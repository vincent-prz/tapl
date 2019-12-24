{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

module Main where

import CodeSample
import GameData
import GameLogic

import Lib.Lib
import Untyped.Evaluator
import Untyped.Parser

import Data.List
import qualified Data.Map as Map
import Miso
import Miso.String hiding (head, length, map, null, zip)
-- | JSAddle import
#ifndef __GHCJS__
import Language.Javascript.JSaddle.Warp as JSaddle
import qualified Network.Wai.Handler.Warp as Warp
import Network.WebSockets
#endif
data EvalOpts = EvalOpts
  { verbose :: Bool
  , strategy :: EvaluationStrategy
  } deriving (Eq)

data Model = Model
  { opts :: EvalOpts
  , input :: MisoString
  , output :: MisoString
  , notes :: MisoString
  , codeSample :: CodeSample
  , levelInd :: Int
  , isGameStarting :: Bool
  , isGameOver :: Bool
  , isLevelSuccessful :: Bool
  } deriving (Eq)

data Action
  = ChangeInput MisoString
  | ChangeVerbose Bool
  | ChangeEvalStrategy EvaluationStrategy
  | ChangeSample Int
  | RunProgram
  | DisplayAbout
  | StartGame
  | ReallyStartGame
  | EndGame
  | SubmitLevelAttempt
  | GoToNextLevel
  | NoOp
  deriving (Eq)
#ifndef __GHCJS__
runApp :: JSM () -> IO ()
runApp f =
  Warp.runSettings
    (Warp.setPort 8080 (Warp.setTimeout 3600 Warp.defaultSettings)) =<<
  JSaddle.jsaddleOr defaultConnectionOptions (f >> syncPoint) JSaddle.jsaddleApp
#else
runApp :: IO () -> IO ()
runApp app = app
#endif
defaultOpts :: EvalOpts
defaultOpts = EvalOpts True FullBeta

valueToEvaluationStrategy :: MisoString -> EvaluationStrategy
valueToEvaluationStrategy "0" = CallByValue
valueToEvaluationStrategy "1" = FullBeta
valueToEvaluationStrategy v =
  error $ "unrecognized value for evaluation strategy :" ++ fromMisoString v

processInput :: EvalOpts -> String -> Either String [String]
processInput opts input =
  let parseResult = fullParser input
   in case parseResult of
        Left err -> Left $ show err
        Right p -> runProgram opts p

runProgram :: EvalOpts -> Program -> Either String [String]
runProgram opts t =
  if verbose opts
    then case evalProgram (strategy opts) t of
           Left err -> Left $ show err
           Right ts -> Right $ map show ts
    else case evalProgramFinalResult (strategy opts) t of
           Left err -> Left $ show err
           Right t' -> Right [show t']

-- FIXME: lexical error on multine string here.. why ?
initialNotes :: MisoString
initialNotes =
  "Hi There! This is a repl for the lambda calculus language. You can type any expression you like, take a look at the code samples, or try a gamified tutorial if you want to know more about lambda calculus. I hope you'll  find it useful."

main :: IO ()
main = runApp $ startApp App {..}
  where
    initialAction = NoOp
    model =
      Model
        { opts = defaultOpts
        , input = code $ head codeSampleList
        , output = ""
        , notes = initialNotes
        , codeSample = head codeSampleList
        , levelInd = -1
        , isGameStarting = False
        , isGameOver = False
        , isLevelSuccessful = False
        }
    update = updateModel
    view = viewModel
    events = defaultEvents
    subs = []
    mountPoint = Nothing

isGameOn :: Model -> Bool
isGameOn m = levelInd m >= 0

lvlExpectsSubmission :: Int -> Bool
lvlExpectsSubmission ind =
  case levels Map.!? ind of
    Nothing -> error $ "Error: could not get level " ++ show ind
    Just lvl -> not (null (expectations lvl))

goToNextLevel :: Model -> Model
goToNextLevel m =
  let nextLevelInd = levelInd m + 1
      nextLevel = levels Map.!? nextLevelInd
   in if nextLevelInd > length (Map.keys levels) - 1
        then m {isLevelSuccessful = False, isGameOver = True}
        else m
               { levelInd = nextLevelInd
               , isLevelSuccessful = False
               , input = maybe "" initialCode nextLevel
               , output = ""
               , notes =
                   case nextLevel of
                     Nothing ->
                       error $
                       "Error: could not get level " ++ show (levelInd m)
                     Just lvl ->
                       "Level " <> toMisoString (show nextLevelInd) <> ": " <>
                       lvlTitle lvl <>
                       "\n\n" <>
                       lvlExcerpt lvl
               }

updateModel :: Action -> Model -> Effect Action Model
updateModel action m =
  case action of
    ChangeInput newInput -> noEff $ m {input = newInput}
    ChangeVerbose v -> noEff $ m {opts = EvalOpts v (strategy (opts m))}
    ChangeEvalStrategy strat ->
      noEff $ m {opts = EvalOpts (verbose (opts m)) strat}
    ChangeSample ind ->
      let cs = codeSampleList !! ind
       in noEff $ m {input = code cs, codeSample = cs, notes = excerpt cs}
    RunProgram ->
      let result = processInput (opts m) (fromMisoString $ input m)
       in case result of
            Left err -> noEff $ m {output = toMisoString err}
            Right ts ->
              noEff $
              m {output = toMisoString $ Data.List.intercalate "\n -> " ts}
    DisplayAbout ->
      noEff $
      m
        { notes =
            "Hi There! This is a repl for the lambda calculus language. You can type any expression you like, and / or take a look at the code samples. I hope you'll find it useful."
        }
    StartGame -> noEff $ m {isGameStarting = True}
    ReallyStartGame -> noEff $ goToNextLevel m {isGameStarting = False}
    SubmitLevelAttempt ->
      case levels Map.!? levelInd m of
        Nothing -> error $ "Error: could not get level " ++ show (levelInd m)
        Just lvl ->
          case processSubmission lvl (fromMisoString (input m)) of
            Left err -> noEff $ m {output = toMisoString err}
            Right _ -> noEff $ m {isLevelSuccessful = True}
    GoToNextLevel -> noEff $ goToNextLevel m
    EndGame ->
      noEff $
      m {levelInd = -1, isGameOver = False, input = "", output = "", notes = ""}
    NoOp -> noEff m

--            toMisoString <$> Data.List.intercalate "\n -> " <$>
viewCodeSampleOption :: CodeSample -> Int -> View Action
viewCodeSampleOption cs ind =
  option_ [value_ $ toMisoString $ show ind] [text $ title cs]

inputTextAreaColor :: MisoString
inputTextAreaColor = "#F8F8FF"

inputTextAreaRows :: MisoString
inputTextAreaRows = "18"

outputTextAreaRows :: MisoString
outputTextAreaRows = "9"

notesTextAreaRows :: MisoString
notesTextAreaRows = "7"

textAreaClassVal :: MisoString
textAreaClassVal = "textarea is-medium"

getDisplayStyle :: Bool -> Map.Map MisoString MisoString
getDisplayStyle b
  | b = Map.empty
  | otherwise = Map.singleton "display" "none"

viewGameIntroModal :: View Action
viewGameIntroModal =
  div_
    [class_ "is-active modal"]
    [ div_ [class_ "modal-background"] []
    , div_
        [class_ "modal-card"]
        [ header_
            [class_ "modal-card-head"]
            [p_ [class_ "modal-card-title"] ["Welcome!"]]
        , section_ [class_ "modal-card-body"] [text introText1]
        , section_ [class_ "modal-card-body"] [text introText2]
        , footer_
            [class_ "modal-card-foot"]
            [ button_
                [class_ "button is-success", onClick ReallyStartGame]
                ["Ok"]
            ]
        ]
    ]

viewGameOverModal :: View Action
viewGameOverModal =
  div_
    [class_ "is-active modal"]
    [ div_ [class_ "modal-background"] []
    , div_
        [class_ "modal-card"]
        [ header_
            [class_ "modal-card-head"]
            [p_ [class_ "modal-card-title"] ["Congratulations!"]]
        , section_ [class_ "modal-card-body"] [text gameOverText]
        , footer_
            [class_ "modal-card-foot"]
            [button_ [class_ "button is-success", onClick EndGame] ["Ok"]]
        ]
    ]

viewLevelSuccessModal :: View Action
viewLevelSuccessModal =
  div_
    [class_ "is-active modal"]
    [ div_ [class_ "modal-background"] []
    , div_
        [class_ "modal-card"]
        [ header_
            [class_ "modal-card-head"]
            [p_ [class_ "modal-card-title"] ["Good answer!"]]
        , section_ [class_ "modal-card-body"] ["Congrats!"]
        , footer_
            [class_ "modal-card-foot"]
            [ button_
                [class_ "button is-success", onClick GoToNextLevel]
                ["next"]
            ]
        ]
    ]

viewGameModal :: Model -> View Action
viewGameModal m
  | isGameOver m = viewGameOverModal
  | isLevelSuccessful m = viewLevelSuccessModal
  | isGameStarting m = viewGameIntroModal
  | otherwise = div_ [] []

viewModel :: Model -> View Action
viewModel m =
  div_
    []
    [ link_
        [ rel_ "stylesheet"
        , href_ "https://cdn.jsdelivr.net/npm/bulma@0.8.0/css/bulma.min.css"
        ]
    , div_
        []
        [ viewGameModal m
        , div_
            [class_ "select is-primary"]
            [ select_
                [ on "change" valueDecoder $
                  ChangeVerbose . toEnum . read . fromMisoString
                ]
                [ option_ [value_ "1"] ["Print all reductions steps"]
                , option_ [value_ "0"] ["Print only the output"]
                ]
            ]
        , div_
            [class_ "select is-primary"]
            [ select_
                [ on "change" valueDecoder $
                  ChangeEvalStrategy . valueToEvaluationStrategy
                ]
                [ option_ [value_ "0"] ["Call by value"]
                , option_ [value_ "1"] ["Full Beta reduction"]
                ]
            ]
        , div_
            [ style_ (getDisplayStyle (not (isGameOn m)))
            , class_ "select is-primary"
            ]
            [ select_
                [ on "change" valueDecoder $
                  ChangeSample . read . fromMisoString
                ]
                (mapWithIndex viewCodeSampleOption codeSampleList)
            ]
        , if isGameOn m
            then if lvlExpectsSubmission (levelInd m)
                   then button_
                          [ class_ "button is-danger"
                          , onClick SubmitLevelAttempt
                          ]
                          ["Submit"]
                   else button_
                          [class_ "button is-primary", onClick GoToNextLevel]
                          ["Next"]
            else button_
                   [class_ "button is-primary", onClick StartGame]
                   ["Start Game"]
        , button_ [class_ "button is-primary", onClick RunProgram] ["Run!"]
        --, button_ [class_ "button", onClick DisplayAbout] ["About"]
        ]
    , div_
        [class_ "columns"]
        [ div_
            [class_ "column is-two-thirds"]
            [ textarea_
                [ style_ $ Map.singleton "background-color" inputTextAreaColor
                , class_ textAreaClassVal
                , rows_ inputTextAreaRows
                , value_ (input m)
                , onInput ChangeInput
                ]
                []
            ]
        , div_
            [class_ "column"]
            [ textarea_
                [ class_ textAreaClassVal
                , rows_ outputTextAreaRows
                , value_ (output m)
                , readonly_ True
                ]
                []
            , textarea_
                [ class_ textAreaClassVal
                , rows_ notesTextAreaRows
                , value_ (notes m)
                , readonly_ True
                ]
                []
            ]
        ]
    ]
