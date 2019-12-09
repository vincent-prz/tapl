{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

module Main where

import Lib.Lib
import Untyped.Evaluator
import Untyped.Parser

import qualified Data.Map as Map
import Miso
import Miso.String hiding (map, null)
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

data Model = Model
  { opts :: EvalOpts
  , input :: MisoString
  , codeSample :: CodeSample
  } deriving (Eq)

data Action
  = ChangeInput MisoString
  | ChangeVerbose Bool
  | ChangeEvalStrategy EvaluationStrategy
  | ChangeSample Int
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

processInput :: EvalOpts -> String -> [String]
processInput opts input =
  let parseResult = fullParser input
   in case parseResult of
        Left err -> [show err]
        Right p -> runProgram opts p

runProgram :: EvalOpts -> Program -> [String]
runProgram opts t =
  if verbose opts
    then case evalProgram (strategy opts) t of
           Left err -> [show err]
           Right ts -> map show ts
    else case evalProgramFinalResult (strategy opts) t of
           Left err -> [show err]
           Right t' -> [show t']

main :: IO ()
main = runApp $ startApp App {..}
  where
    initialAction = NoOp
    model =
      Model
        { opts = defaultOpts
        , input = code $ Prelude.head codeSampleList
        , codeSample = Prelude.head codeSampleList
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
       in noEff $ m {input = code cs, codeSample = cs}
    NoOp -> noEff m

paragraphStyle :: Map.Map MisoString MisoString
paragraphStyle = Map.singleton "width" "50%"

viewSingleStep :: String -> View Action
viewSingleStep step = p_ [] [text $ toMisoString step]

viewCodeSampleOption :: CodeSample -> Int -> View Action
viewCodeSampleOption cs ind =
  option_ [value_ $ toMisoString $ show ind] [text $ title cs]

viewModel :: Model -> View Action
viewModel m =
  div_
    []
    [ p_
        [style_ paragraphStyle]
        [ "This is a repl for the lambda calculus language. You can type any expression you like, and / or take a look at the code samples, which show how to build features that you would find in today's mainstream languages (eg Python, Javascript, Java, etc). This is a work in progress. I hope you'll find it useful."
        ]
    , div_
        []
        [ span_ [] ["Samples"]
        , select_
            [on "change" valueDecoder $ ChangeSample . read . fromMisoString]
            (mapWithIndex viewCodeSampleOption codeSampleList)
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
    , p_ [style_ paragraphStyle] [text $ excerpt $ codeSample m]
    , textarea_
        [rows_ "10", cols_ "50", value_ (input m), onInput ChangeInput]
        [text $ input m]
    , div_
        []
        (map viewSingleStep (processInput (opts m) (fromMisoString $ input m)))
    ]
