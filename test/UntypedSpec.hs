module UntypedSpec where

import Data.Bifunctor (first)
import Data.Either (isLeft)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Test.Hspec
import Untyped.Evaluator
import Untyped.Parser

parseThenEval :: String -> Either RuntimeError Term
parseThenEval input = first (const ParsingError) (fullParser input) >>= eval

parseThenEvalBeta :: String -> Either RuntimeError Term
parseThenEvalBeta input =
  first (const ParsingError) (fullParser input) >>= evalBeta

spec :: Spec
spec = do
  describe "Untyped Parsing" $ do
    it "parses simple variable" $ do
      fmap show (fullParser "x") `shouldBe` Right "x"
    it "parses identity lambda" $ do
      fmap show (fullParser "\\x.x") `shouldBe` Right "\\x.x"
    it "parses simple application" $ do
      fmap show (fullParser "x y") `shouldBe` Right "x y"
    it "parses abstraction of simple application" $ do
      fmap show (fullParser "\\x.x y") `shouldBe` Right "\\x.x y"
    it "parses application associatively to the left" $ do
      fmap show (fullParser "s t u") `shouldBe` Right "s t u"
    it "parses application associatively to the left v2" $ do
      fmap show (fullParser "(\\x.x) t u") `shouldBe` Right "(\\x.x) t u"
    it "parses application with parens" $ do
      fmap show (fullParser "s (t u)") `shouldBe` Right "s (t u)"
    it "parses double abstraction" $ do
      fmap show (fullParser "\\x.\\y.x y") `shouldBe` Right "\\x.\\y.x y"
    it "parses application of lambda" $ do
      fmap show (fullParser "(\\x.x) y") `shouldBe` Right "(\\x.x) y"
    it "parses application with bodies as far to the right as possible" $ do
      fmap show (fullParser "\\x.\\y.x y x") `shouldBe` Right "\\x.\\y.x y x"
    it "parses application of id to id" $ do
      fmap show (fullParser "(\\x.x) (\\y.y)") `shouldBe` Right "(\\x.x) \\y.y"
    it "parses application of id to id without parens" $ do
      fmap show (fullParser "(\\x.x) \\y.y") `shouldBe` Right "(\\x.x) \\y.y"
    it "parses \\x.x \\x.x" $ do
      fmap show (fullParser "\\x.x (\\x.x)") `shouldBe` Right "\\x.x \\x.x"
    it "fails when given lambda with malformed bound variable" $ do
      isLeft (fullParser "\\x y.x") `shouldBe` True
    it "fails when given x.x" $ do isLeft (fullParser "x.x") `shouldBe` True
    it "fails when unknown token" $ do isLeft (fullParser "x,x") `shouldBe` True
  describe "Untyped Parsing + Evaluating" $ do
    it "evaluates identity" $ do
      fmap show (parseThenEval "\\x.x") `shouldBe` Right "\\x.x"
    it "evaluates identity applied to itself" $ do
      fmap show (parseThenEval "(\\x.x) (\\x.x)") `shouldBe` Right "\\x.x"
    it "evaluates definition of true" $ do
      fmap show (parseThenEval "\\t.\\f.t") `shouldBe` Right "\\t.\\f.t"
    it "evaluates not true to false" $ do
      fmap show (parseThenEval "(\\b. b (\\t.\\f.f) (\\t.\\f.t)) \\t.\\f.t") `shouldBe`
        Right "\\t.\\f.f"
    it "evaluates not false to true" $ do
      fmap show (parseThenEval "(\\b. b (\\t.\\f.f) (\\t.\\f.t)) \\t.\\f.f") `shouldBe`
        Right "\\t.\\f.t"
    it "evaluates succ zero to a term equivalent to one" $ do
      fmap show (parseThenEval "(\\c.\\s.\\z.s (c s z)) \\s.\\z.z") `shouldBe`
        Right "\\s.\\z.s ((\\s.\\z.z) s z)"
    it "prevents variable capture" $ do
      fmap show (parseThenEval "\\x.(\\x.x) x") `shouldBe` Right "\\x.(\\x.x) x"
    it "prevents variable capture v2" $ do
      fmap show (parseThenEval "(\\y.\\x.x y) \\x.x") `shouldBe`
        Right "\\x.x \\x.x"
    it "prevents variable capture v3" $ do
      fmap show (parseThenEval "(\\z.((\\x.\\z.x) z)) \\x.\\y.x y") `shouldBe`
        Right "\\z.\\x.\\y.x y"
    it "evaluates nested expression" $ do
      fmap show (parseThenEval "(\\x.x) ((\\x.x) (\\z. (\\x.x) z))") `shouldBe`
        Right "\\z.(\\x.x) z"
    it "preserves bound variable" $ do
      fmap show (parseThenEval "(\\y.(\\x.\\x.x) y) \\s.\\w.\\z.s w z") `shouldBe`
        Right "\\x.x"
    it "fails on unbound variable" $ do
      fmap show (parseThenEval "x") `shouldBe` Left (UnboundVariable "x")
    it "fails on unbound variable inside simple application" $ do
      fmap show (parseThenEval "\\x.x y") `shouldBe` Left (UnboundVariable "y")
  describe "Untyped Beta evaluation" $ do
    it "fully reduces \\x. (\\y.y) x" $ do
      fmap show (parseThenEvalBeta "\\x. (\\y.y) x") `shouldBe` Right "\\x.x"
    it "fully reduces \\x. (\\x.x) x" $ do
      fmap show (parseThenEvalBeta "\\x. (\\x.x) x") `shouldBe` Right "\\x.x"
    it "prevents variable capture" $ do
      fmap show (parseThenEvalBeta "(\\y.\\x.x y) \\x.x") `shouldBe`
        Right "\\x.x \\x.x"
    it "prevents variable capture v2" $ do
      fmap show (parseThenEvalBeta "(\\y.\\x.x y) \\x.x") `shouldBe`
        Right "\\x.x \\x.x"
    it "prevents variable capture v3" $ do
      fmap show (parseThenEvalBeta "\\z.((\\x.\\z.x) z)") `shouldBe`
        Right "\\z.\\z1.z"
    it "prevents variable capture v4" $ do
      fmap show (parseThenEvalBeta "\\z1.\\z.((\\x.\\z.x) (z z1))") `shouldBe`
        Right "\\z1.\\z.\\z2.z z1"
    it "fully reduces complex expression" $ do
      fmap show (parseThenEvalBeta "\\z1.\\z.((\\x.\\z.x) z z1)") `shouldBe`
        Right "\\z1.\\z.z"
    it "fully reduces nested expression" $ do
      fmap show (parseThenEvalBeta "(\\x.x) ((\\x.x) (\\z. (\\x.x) z))") `shouldBe`
        Right "\\z.z"
    it "fully reduces succ zero to one" $ do
      fmap show (parseThenEvalBeta "(\\c.\\s.\\z.s (c s z)) \\s.\\z.z") `shouldBe`
        Right "\\s.\\z.s z"
    it "preserves bound variable" $ do
      fmap show (parseThenEvalBeta "\\y.(\\x.\\x.x) y") `shouldBe`
        Right "\\y.\\x.x"
    it "fails on unbound variable" $ do
      fmap show (parseThenEvalBeta "x") `shouldBe` Left (UnboundVariable "x")
    it "fails on unbound variable inside simple application" $ do
      fmap show (parseThenEvalBeta "\\x.x y") `shouldBe`
        Left (UnboundVariable "y")
