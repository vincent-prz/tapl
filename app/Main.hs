{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

module Main where

import Data.Either
import Data.List

import qualified Data.Map as Map

--import Lib.Lib
import Expectation
import Untyped.Evaluator
import Untyped.Parser

import Miso
import Miso.String hiding (map, null, zip)
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

data CodeSample = CodeSample
  { title :: MisoString
  , code :: MisoString
  , excerpt :: MisoString
  } deriving (Eq)

data Level = Level
  { lvlTitle :: MisoString
  , initialCode :: MisoString
  , lvlExcerpt :: MisoString
  , expectations :: [Expectation]
  }

data Model = Model
  { opts :: EvalOpts
  , input :: MisoString
  , output :: MisoString
  , notes :: MisoString
  , lastRunIsSuccesful :: Maybe Bool
  , codeSample :: CodeSample
  , levelInd :: Int
  } deriving (Eq)

data Action
  = ChangeInput MisoString
  | ChangeVerbose Bool
  | ChangeEvalStrategy EvaluationStrategy
  | ChangeSample Int
  | RunProgram
  | DisplayAbout
  | SubmitLevelAttempt
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

main :: IO ()
main = runApp $ startApp App {..}
  where
    initialAction = NoOp
    model =
      Model
        { opts = defaultOpts
        --, input = code $ Prelude.head codeSampleList
        , input = maybe "" initialCode $ Map.lookup 0 levels
        , output = ""
        , notes = maybe "" lvlExcerpt $ Map.lookup 0 levels
        , lastRunIsSuccesful = Nothing
        , codeSample = Prelude.head codeSampleList
        , levelInd = 0
        }
    update = updateModel -- update function
    view = viewModel -- view function
    events = defaultEvents -- default delegated events
    subs = [] -- empty subscription list
    mountPoint = Nothing -- mount point for application (Nothing defaults to 'body')

codeSampleList :: [CodeSample]
codeSampleList =
  [ CodeSample
      { title = "The Identity function"
      , code = "id = \\x.x\nid \\x.x x"
      , excerpt =
          "The identity function is the simplest function you can build."
      }
  , CodeSample
      { title = "Booleans"
      , code =
          "true = \\t.\\f.t\nfalse = \\t.\\f.f\nnot = \\b.b false true\nnot true"
      , excerpt =
          "Here is how booleans can be encoded in the lambda calculus. Can you build the `and` function ? `Or`, `xor`, etc..."
      }
  , CodeSample
      { title = "Numbers"
      , code =
          "0 = \\s.\\z.z\n1 = \\s.\\z.s z\n2 = \\s.\\z.s (s z)\nsucc = \\n.\\s.\\z.s (n s z)\nsucc 1"
      , excerpt =
          "Here is an encoding of numbers. The idea is: a number `n` is a function which applies `n` times its first argument `s` to its second argument `z`."
      }
  , CodeSample
      { title = "Numbers: Addition"
      , code =
          "0 = \\s.\\z.z\n1 = \\s.\\z.s z\n2 = \\s.\\z.s (s z)\nsucc = \\n.\\s.\\z.s (n s z)\nplus = \\m.\\n.\\s.\\z.m s (n s z)\nplus 2 2"
      , excerpt =
          "Here is how addition would be defined. Can you define the multiplication ?"
      }
  ]

levels :: Map.Map Int Level
levels =
  Map.fromList
    [ ( 0
      , Level
          { lvlTitle = "the identity function"
          , initialCode = "\\x.x"
          , lvlExcerpt = "hello world"
          , expectations =
              rights
                [ buildExpectation "\\t.\\f.t" "\\t.\\f.f"
                , buildExpectation "\\t.\\f.f" "\\t.\\f.t"
                ]
          })
    , ( 1
      , Level
          { lvlTitle = "Church booleans"
          , initialCode =
              "true = \\t.\\f.t\nfalse = \\t.\\f.f\nnot = \\b.b false true\nnot true"
          , lvlExcerpt = "hello booleans"
          , expectations = []
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

-- FIXME: clunkyness with records
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
            Left err ->
              noEff $
              m {output = toMisoString err, lastRunIsSuccesful = Just False}
            Right ts ->
              noEff $
              m
                { output = toMisoString $ Data.List.intercalate "\n -> " ts
                , lastRunIsSuccesful = Just True
                }
    DisplayAbout ->
      noEff $
      m
        { notes =
            "Hi There! This is a repl for the lambda calculus language. You can type any expression you like, and / or take a look at the code samples. I hope you'll find it useful."
        }
    -- FIXME: refactor this
    SubmitLevelAttempt ->
      case ( levels Map.!? levelInd m
           , singleTermParser (fromMisoString (input m))) of
        (Just lvl, Right t) ->
          case testSubmission t (expectations lvl) of
            Left err -> noEff $ m {output = toMisoString err}
            Right _ -> noEff $ m {output = "Good answer!"}
        _ -> undefined
    --  let nextLevelInd = levelInd m + 1
    --      nextLevel = levels Map.!? nextLevelInd
    --      nextInput = initialCode <$> nextLevel
    --      nextNotes = lvlExcerpt <$> nextLevel
    --      nextResult = processInput (opts m) <$> (fromMisoString <$> nextInput)
    --   in noEff $
    --      m
    --        { levelInd = nextLevelInd
    --        , input = fromMaybe (error "submit error") nextInput
    --        , notes = fromMaybe (error "submit error") nextNotes
    --        , output =
    --            case fromMaybe (error "submit error") nextResult of
    --              Left err -> toMisoString err
    --              Right ts -> toMisoString $ Data.List.intercalate "\n -> " ts
    --        }
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

textAreaClassVal :: Maybe Bool -> MisoString
textAreaClassVal isSuccess =
  case isSuccess of
    Nothing -> "textarea is-medium"
    (Just True) -> "textarea is-medium is-success"
    (Just False) -> "textarea is-medium is-danger"

submitButtonStyle :: Int -> Map.Map MisoString MisoString
submitButtonStyle n
  | n < Data.List.length (Map.keys levels) - 1 = Map.fromList []
  | otherwise = Map.fromList [("display", "none")]

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
          --div_
            --[class_ "select is-primary"]
            --[ select_
            --    [ on "change" valueDecoder $
            --      ChangeSample . read . fromMisoString
            --    ]
            --    (mapWithIndex viewCodeSampleOption codeSampleList)
            --]
        [ div_
            [class_ "select is-primary"]
            [ select_
                [ on "change" valueDecoder $
                  ChangeVerbose . toEnum . read . fromMisoString
                ]
                [ option_ [value_ "1"] ["Print all reductions steps"]
                , option_ [value_ "0"] ["Print only the output"]
                ]
            ]
        --, div_
        --    [class_ "select is-primary"]
        --    [ select_
        --        [ on "change" valueDecoder $
        --          ChangeEvalStrategy . valueToEvaluationStrategy
        --        ]
        --        [ option_ [value_ "0"] ["Call by value"]
        --        , option_ [value_ "1"] ["Full Beta reduction"]
        --        ]
        --    ]
        , button_
            [ class_ "button is-primary"
            , style_ (submitButtonStyle (levelInd m))
            , onClick SubmitLevelAttempt
            ]
            ["Submit"]
        , button_ [class_ "button is-primary", onClick RunProgram] ["Run!"]
        --, button_ [class_ "button", onClick DisplayAbout] ["About"]
        ]
    , div_
        [class_ "columns"]
        [ div_
            [class_ "column is-two-thirds"]
            [ textarea_
                [ style_ $ Map.singleton "background-color" inputTextAreaColor
                , class_ $ textAreaClassVal (lastRunIsSuccesful m)
                , rows_ inputTextAreaRows
                , value_ (input m)
                , onInput ChangeInput
                ]
                []
            ]
        , div_
            [class_ "column"]
            [ textarea_
                [ class_ $ textAreaClassVal Nothing
                , rows_ outputTextAreaRows
                , value_ (output m)
                , readonly_ True
                ]
                []
            , textarea_
                [ class_ $ textAreaClassVal Nothing
                , rows_ notesTextAreaRows
                , value_ (notes m)
                , readonly_ True
                ]
                []
            ]
        ]
    ]
