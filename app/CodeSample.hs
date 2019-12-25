{-# LANGUAGE OverloadedStrings #-}

module CodeSample where

import Miso.String

data CodeSample = CodeSample
  { title :: MisoString
  , code :: MisoString
  , excerpt :: MisoString
  } deriving (Eq)

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
