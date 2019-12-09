{-# LANGUAGE OverloadedStrings #-}

module TutorialText where

import qualified Data.Map as Map
import Miso.String hiding (map, null)

levelToTitleMap :: Map.Map Int MisoString
levelToTitleMap =
  Map.fromList
    [ (0, "The simplest value")
    , (1, "Every value is a function")
    , (2, "Function application")
    , (3, "There is nothing more to it")
    , (4, "Multi parameter function")
    ]

levelToTextMap :: Map.Map Int MisoString
levelToTextMap =
  Map.fromList
    [ ( 0
      , "Here is the simplest value you can construct in the lambda calculus. '\\' can be read as 'lambda', and is the symbol we'll use to define lambdas (or functions). The 'x' at the left of the '.' corresponds to the argument name of the lambda, and at the right of the dot lies the body of the function we are defining. Hence here we have the function which takes some 'x', and returns 'x', it is called the identity function.")
    , ( 1
      , "In the lambda calculus, every value is a function. There are no numbers, no booleans, no strings, etc (at least not natively). Here is another example of function: it takes as input one function 'x', and returns the application of 'x' on itself (which will be a function as well). Indeed, given 2 lambda functions 'f' and 'g', 'f g' means f applied to g.")
    , ( 2
      , "Here is an example of one function applied to another. It means: apply the function around parentheses to the function on the right. Since the function around parentheses is the identity function from before, it should return the function on the right as is. And indeed, it is what we get in the output. Note: the parentheses matter here, more on this later.")
    , ( 3
      , "At this point, we've explored every feature of the language. There is nothing more in this language than variable definition, lambda definition (the technical term is abstraction), and function application. But it turns out this tiny language has the property of being Turing complete, meaning that everything you can do in mainstream programming languages (eg Python, Javascript, Java...), you can theoretically do it with this language. Let's try to get a feel of how this is possible in the next levels.")
    , ( 4
      , "To create a function of 2 parameters you need to create a double lambda, like here. Indeed, the lambda below can equivalently be seen as a function of one parameter ('x' here) returning another function ('\\y,y'), and a function of 2 parameters 'x' and 'y', which in this case discards 'x' and returns 'y'. This equivalence is known as currying. Likewise, a function of n parameters is equivalent to n nested lambdas.")
    ]
