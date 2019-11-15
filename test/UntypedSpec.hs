module UntypedSpec where

import Data.Either (isLeft)
import Test.Hspec
import Untyped.Evaluator
import Untyped.Parser

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
    it "parses application with parens" $ do
      fmap show (fullParser "s (t u)") `shouldBe` Right "(s (t u))"
    it "parses double abstraction" $ do
      fmap show (fullParser "\\x.\\y.x y") `shouldBe` Right "\\x.\\y.(x y)"
    it "parses application of lambda" $ do
      fmap show (fullParser "(\\x.x) y") `shouldBe` Right "(\\x.x y)"
    it "parses application with higher precedence than abstraction" $ do
      fmap show (fullParser "\\x.\\y.x y x") `shouldBe`
        Right "\\x.\\y.((x y) x)"
    it "fails when given lambda with malformed bound variable" $ do
      isLeft (fullParser "\\x y.x") `shouldBe` True
    it "fails when given x.x" $ do isLeft (fullParser "x.x") `shouldBe` True
  describe "Untyped: convert to nameless terms" $ do
    it "converts simple variable" $ do
      removeNames ["x"] (T_VAR "x") `shouldBe` Right (NT_VAR 0)
    it "converts simple abstraction" $ do
      removeNames [] (T_ABS (T_VAR "x") (T_VAR "x")) `shouldBe`
        Right (NT_ABS (NT_VAR 0))
    --it "converts abstraction with free variable" $ do
    --  removeNames ["y"] (T_ABS (T_VAR "x") (T_APP "x")) `shouldBe`
    --    Right (NT_ABS (NT_VAR 0))
