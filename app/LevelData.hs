{-# LANGUAGE OverloadedStrings #-}

module LevelData where

import Data.Either
import LevelLogic

import qualified Data.Map as Map

levels :: Map.Map Int Level
levels =
  Map.fromList
    [ ( 0
      , Level
          { lvlTitle = "The Identity function"
          , initialCode = "\\x.x"
          , lvlExcerpt =
              "This is the simplest value of the lambda calculus: the identity function.\
            \ The `\\` represents a lambda, what is on the left of the `.` is the name\
            \ of the parameter (`x` here), and what is on the the right is the body of\
            \ the function (`x` as well, conforming to our intuition of an identity\
            \ function)."
          , expectations = []
          })
    , ( 1
      , Level
          { lvlTitle = "Function application"
          , initialCode = "\\x.x x"
          , lvlExcerpt =
              "Here we have a lambda which applies its parameter `x` to itself.\
            \ Indeed, given 2 terms `t1` and `t2`, `t1 t2` expresses the application \
            \ of `t1` on `t2`. Remember: every value is a function, in particular the \
            \ parameter `x` and the return value `x x` will be functions."
          , expectations = []
          })
    , ( 2
      , Level
          { lvlTitle = "Function application 2"
          , initialCode = "id = \\x.x\nid \\x.x x"
          , lvlExcerpt =
              "For more convenience, the possibility to assign values to variables\
            \ has been added. Here the identity function is assigned to the `id` \
            \ variable. When running code with several lines, the output of the \
            \last line will be computed. Try to run this and see if the \
            \result makes sense."
          , expectations = []
          })
    , ( 3
      , Level
          { lvlTitle = "Alpha equivalence"
          , initialCode = "id = \\y.y\nid \\hello.hello hello"
          , lvlExcerpt =
              "The name of the parameters doesn't matter, ie `\\x.x` \
              \ is the same lambda as `\\y.y`. `\\x.x` is said to be \
              \ alpha equivalent to `\\y.y`"
          , expectations = []
          })
    , ( 4
      , Level
          { lvlTitle = "Multiple parameters"
          , initialCode = "f = \\x.\\y.x y\nf (\\x.x) (\\x.x x)"
          , lvlExcerpt =
              "To create a function of 2 parameters you need to create a double lambda\
              \, like with the function `f` here. Indeed, `f` can equivalently be seen\
              \ as a function of one parameter `x returning another function of `y` \
              \(here \\y.x y), or of a function of 2 parameters `x` and `y`, and returning \
              \ the application of `x` on `y`. This equivalence is known as currying.\
              \Try to run this and see if it makes sense."
          , expectations = []
          })
    , ( 5
      , Level
          { lvlTitle = "A constant function"
          , initialCode = ""
          , lvlExcerpt =
              "This is the first level with something to submit! Write a function of 2 parameters\
              \ which discards the first one and returns the second one. Note: as per the currying\
              \ equivalence, such function can also be seen as a constant function which \
              \ always returns the identity function."
          , expectations =
              rights
                [ buildExpectation ["\\t.\\f.t"] "\\x.x"
                , buildExpectation ["\\t.\\f.f"] "\\x.x"
                ]
          })
    , ( 6
      , Level
          { lvlTitle = "Church booleans"
          , initialCode =
              "true = \\t.\\f.t\nfalse = \\t.\\f.f\nnot=\\b.b false true\nnot true"
          , lvlExcerpt =
              "Booleans can be represented this way in the lambda calculus. \
              \Namely `true` is a function of 2 parameters which returns the first one, and \
              \`false` is the function of 2 parameters which picks the second one. Here we \
              \ also define a `not` function, which maps `true` to `false` and `false` to \
              \ `true`. Run the code and see that it indeed returns `false`. Try to run the \
              \ function on `false` if you'd like."
          , expectations = []
          })
    , ( 7
      , Level
          { lvlTitle = "Church booleans: AND"
          , initialCode =
              "true = \\t.\\f.t\nfalse = \\t.\\f.f\nand = \\a.\\b.a b a\nand true false"
          , lvlExcerpt =
              "This is how the function `and` would be defined. You can try to evaluate\
              \ this function on various sets of inputs to convince yourself that \
              \ it behaves like the logical AND.\n\nThe idea of the function is the following:\
              \ if `a` is false, then we want to return `false`, so something of the\
              \ form `a <something> a` will yield `a` (by definition of `false`, which \
              \ returns its second argument), which is `false`. Besides, if `a` is `true`\
              \ then `and a b == b`, so in this case we just want to return `b`. By \
              \ definition of `true`, if `a` is `true`, `a <something> a` evaluates to `something \
              \, hence we just have to take something = b. So finally the body of `and` is `a b a`."
          , expectations = []
          })
    , ( 8
      , Level
          { lvlTitle = "Church booleans: OR"
          , initialCode =
              "true = \\t.\\f.t\nfalse = \\t.\\f.f\nnot = \\b.b false true\nand = \\a.\\b.a b a\n"
          , lvlExcerpt =
              "can you implement OR? Note: you can define it in terms of `and` and `not`\
              \ using De Morgan's law, but you can also implement it from scratch."
          , expectations =
              rights
                [ buildExpectation ["\\t.\\f.t", "\\t.\\f.t"] "\\t.\\f.t"
                , buildExpectation ["\\t.\\f.t", "\\t.\\f.f"] "\\t.\\f.t"
                , buildExpectation ["\\t.\\f.f", "\\t.\\f.t"] "\\t.\\f.t"
                , buildExpectation ["\\t.\\f.f", "\\t.\\f.f"] "\\t.\\f.f"
                ]
          })
    , ( 9
      , Level
          { lvlTitle = "Church booleans: XOR"
          , initialCode =
              "true = \\t.\\f.t\nfalse = \\t.\\f.f\nnot = \\b.b false true\nand = \\a.\\b.a b a\n"
          , lvlExcerpt = "can you implement XOR?"
          , expectations =
              rights
                [ buildExpectation ["\\t.\\f.t", "\\t.\\f.t"] "\\t.\\f.f"
                , buildExpectation ["\\t.\\f.t", "\\t.\\f.f"] "\\t.\\f.t"
                , buildExpectation ["\\t.\\f.f", "\\t.\\f.t"] "\\t.\\f.t"
                , buildExpectation ["\\t.\\f.f", "\\t.\\f.f"] "\\t.\\f.f"
                ]
          })
    , ( 10
      , Level
          { lvlTitle = "Church numerals"
          , initialCode =
              "0 = \\s.\\z.z\n1 = \\s.\\z.s z\n2 = \\s.\\z.s (s z)\nsucc = \\n.\\s.\\z.s (n s z)\nsucc 1"
          , lvlExcerpt =
              "Here is an encoding of numbers. The idea is: a number `n` \
               \is a function which applies `n` times its first argument `s`\
               \to its second argument `z`. We have defined a `succ` function, which \
               \computes the successor of a number."
          , expectations = []
          })
    , ( 11
      , Level
          { lvlTitle = "Church numerals: addition"
          , initialCode =
              "0 = \\s.\\z.z\n1 = \\s.\\z.s z\n2 = \\s.\\z.s (s z)\nsucc = \\n.\\s.\\z.s (n s z)\n"
          , lvlExcerpt =
              "Define a function which takes 2 numbers and returns their addition. Hint: use the\
              \ interpretation which sees a number `n` as a function of 2 arguments which applies the \
              \first argument `n` times to the second. You can use the `succ` function."
          , expectations =
              rights
                [ buildExpectation
                    ["\\s.\\z.s z", "\\s.\\z.s z"]
                    "\\s.\\z.s (s z)"
                , buildExpectation ["\\s.\\z.z", "\\s.\\z.z"] "\\s.\\z.z"
                , buildExpectation
                    ["\\s.\\z.s z", "\\s.\\z.s (s z)"]
                    "\\s.\\z.s (s (s z))"
                ]
          })
    , ( 12
      , Level
          { lvlTitle = "Church numerals: multiplication"
          , initialCode =
              "0 = \\s.\\z.z\n1 = \\s.\\z.s z\n2 = \\s.\\z.s (s z)\nsucc = \\n.\\s.\\z.s (n s z)\n"
          , lvlExcerpt =
              "Define a function which takes 2 numbers and returns their multiplication."
          , expectations =
              rights
                [ buildExpectation ["\\s.\\z.s z", "\\s.\\z.s z"] "\\s.\\z.s z"
                , buildExpectation ["\\s.\\z.z", "\\s.\\z.s z"] "\\s.\\z.z"
                , buildExpectation
                    ["\\s.\\z.s z", "\\s.\\z.s (s z)"]
                    "\\s.\\z.s (s z)"
                ]
          })
    , ( 13
      , Level
          { lvlTitle = "End of game"
          , initialCode = ""
          , lvlExcerpt = "The End."
          , expectations = []
          })
    ]
