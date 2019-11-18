module UntypedSpec where

import Data.Bifunctor (first)
import Data.Either (isLeft)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Test.Hspec
import Untyped.Evaluator
import Untyped.Parser

-- dependency injection of name generator
ngMock :: NameGenerator
ngMock [] = "a"
ngMock (h:_) =
  let letters = map (: []) ['a' .. 'z']
      indexOfLast :: Maybe Int
      indexOfLast = elemIndex h letters
      newLetter :: Maybe String
      newLetter = ((+ 1) <$> indexOfLast) >>= atMay letters
   in fromMaybe "a" newLetter --FIXME: swallowing error here

-- FIXME: deal with errors correctly here
parseThenEval :: String -> Either RuntimeError Term
parseThenEval input =
  first (const ParsingError) (fullParser input) >>= evalWithNameGen ngMock

spec :: Spec
spec = do
  describe "Untyped Parsing" $ do
    it "parses simple variable" $ do
      fmap show (fullParser "x") `shouldBe` Right "x"
    it "parses identity lambda" $ do
      fmap show (fullParser "\\x.x") `shouldBe` Right "\\x.x"
    it "parses simple application" $ do
      fmap show (fullParser "x y") `shouldBe` Right "(x y)"
    it "parses abstraction of simple application" $ do
      fmap show (fullParser "\\x.x y") `shouldBe` Right "\\x.(x y)"
    it "parses application associatively to the left" $ do
      fmap show (fullParser "s t u") `shouldBe` Right "((s t) u)"
    it "parses application associatively to the left v2" $ do
      fmap show (fullParser "(\\x.x) t u") `shouldBe` Right "((\\x.x t) u)"
    it "parses application with parens" $ do
      fmap show (fullParser "s (t u)") `shouldBe` Right "(s (t u))"
    it "parses double abstraction" $ do
      fmap show (fullParser "\\x.\\y.x y") `shouldBe` Right "\\x.\\y.(x y)"
    it "parses application of lambda" $ do
      fmap show (fullParser "(\\x.x) y") `shouldBe` Right "(\\x.x y)"
    it "parses application with bodies as far to the right as possible" $ do
      fmap show (fullParser "\\x.\\y.x y x") `shouldBe`
        Right "\\x.\\y.((x y) x)"
    it "parses application of id to id" $ do
      fmap show (fullParser "(\\x.x) (\\y.y)") `shouldBe` Right "(\\x.x \\y.y)"
    it "parses application of id to id without parens" $ do
      fmap show (fullParser "(\\x.x) \\y.y") `shouldBe` Right "(\\x.x \\y.y)"
    it "fails when given lambda with malformed bound variable" $ do
      isLeft (fullParser "\\x y.x") `shouldBe` True
    it "fails when given x.x" $ do isLeft (fullParser "x.x") `shouldBe` True
  describe "Untyped: convert to nameless terms" $ do
    it "converts simple variable" $ do
      removeNames ["x"] (T_VAR "x") `shouldBe` Right (NT_VAR 0)
    it "converts simple application" $ do
      removeNames ["x", "y"] (T_APP (T_VAR "x") (T_VAR "y")) `shouldBe`
        Right (NT_APP (NT_VAR 0) (NT_VAR 1))
    it "converts simple abstraction" $ do
      removeNames [] (T_ABS (T_VAR "x") (T_VAR "x")) `shouldBe`
        Right (NT_ABS (NT_VAR 0))
    it "converts abstraction with free variable" $ do
      removeNames ["y"] (T_ABS (T_VAR "x") (T_APP (T_VAR "x") (T_VAR "y"))) `shouldBe`
        Right (NT_ABS (NT_APP (NT_VAR 0) (NT_VAR 1)))
  describe "Untyped: convert to named terms" $ do
    it "renames simple variable" $ do
      restoreNames ngMock ["x"] (NT_VAR 0) `shouldBe` Right (T_VAR "x")
    it "renames simple application" $ do
      restoreNames ngMock ["x", "y"] (NT_APP (NT_VAR 0) (NT_VAR 1)) `shouldBe`
        Right (T_APP (T_VAR "x") (T_VAR "y"))
    it "renames simple abstraction" $ do
      restoreNames ngMock [] (NT_ABS (NT_VAR 0)) `shouldBe`
        Right (T_ABS (T_VAR "a") (T_VAR "a"))
    it "renames abstraction with free variable" $ do
      restoreNames ngMock ["a"] (NT_ABS (NT_APP (NT_VAR 0) (NT_VAR 1))) `shouldBe`
        Right (T_ABS (T_VAR "b") (T_APP (T_VAR "b") (T_VAR "a")))
  describe "Untyped Parsing + Evaluating" $ do
    it "evaluates identity" $ do
      fmap show (parseThenEval "\\x.x") `shouldBe` Right "\\a.a"
    it "evaluates identity applied to itself" $ do
      fmap show (parseThenEval "(\\x.x) (\\x.x)") `shouldBe` Right "\\a.a"
    it "evaluates definition of true" $ do
      fmap show (parseThenEval "\\t.\\f.t") `shouldBe` Right "\\a.\\b.a"
    it "evaluates not true to false" $ do
      fmap show (parseThenEval "(\\b. b (\\t.\\f.f) (\\t.\\f.t)) \\t.\\f.t") `shouldBe`
        Right "\\a.\\b.b"
    it "evaluates not false to true" $ do
      fmap show (parseThenEval "(\\b. b (\\t.\\f.f) (\\t.\\f.t)) \\t.\\f.f") `shouldBe`
        Right "\\a.\\b.a"
    it "evaluates succ zero to a term equivalent to one" $ do
      fmap show (parseThenEval "(\\c.\\s.\\z.s (c s z)) \\s.\\z.z") `shouldBe`
        Right "\\a.\\b.(a ((\\c.\\d.d a) b))"
    it "fails on unbound variable" $ do
      fmap show (parseThenEval "x") `shouldBe` Left (UnboundVariable "x")
    it "fails on unbound variable inside simple application" $ do
      fmap show (parseThenEval "\\x.x y") `shouldBe` Left (UnboundVariable "y")
