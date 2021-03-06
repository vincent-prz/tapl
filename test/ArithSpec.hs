module ArithSpec where

import Arith.Evaluator
import Arith.Parser
import Test.Hspec

spec :: Spec
spec = do
  describe "Arith Parsing" $ do
    it "parses constant zero" $ do fullParser "0" `shouldBe` Right T_ZERO
    it "parses constant true" $ do fullParser "true" `shouldBe` Right T_TRUE
    it "parses simple call to succ" $ do
      fullParser "succ 0" `shouldBe` Right (T_SUCC T_ZERO)
    it "parses simple call to pred" $ do
      fullParser "pred 0" `shouldBe` Right (T_PRED T_ZERO)
    it "parses simple call to iszero" $ do
      fullParser "iszero 0" `shouldBe` Right (T_IS_ZERO T_ZERO)
    it "parses double call to succ" $ do
      fullParser "succ succ 0" `shouldBe` Right (T_SUCC (T_SUCC T_ZERO))
    it "parses simple conditional" $ do
      fullParser "if true then 0 else false" `shouldBe`
        Right (T_IF_THEN_ELSE T_TRUE T_ZERO T_FALSE)
    it "parses non trivial conditional" $ do
      fullParser "if succ true then 0 else false" `shouldBe`
        Right (T_IF_THEN_ELSE (T_SUCC T_TRUE) T_ZERO T_FALSE)
  describe "Arith Evaluating" $ do
    it "evaluates zero" $ do eval T_ZERO `shouldBe` T_ZERO
    it "evaluates succ zero" $ do eval (T_SUCC T_ZERO) `shouldBe` T_SUCC T_ZERO
    it "evaluates pred zero to zero" $ do eval (T_PRED T_ZERO) `shouldBe` T_ZERO
    it "evaluates pred succ 0 to 0" $ do
      eval (T_PRED (T_SUCC T_ZERO)) `shouldBe` T_ZERO
    it "evaluates pred succ succ 0 to succ 0" $ do
      eval (T_PRED (T_SUCC (T_SUCC T_ZERO))) `shouldBe` T_SUCC T_ZERO
    it "evaluates succ pred 0 to succ 0" $ do
      eval (T_SUCC (T_PRED T_ZERO)) `shouldBe` T_SUCC T_ZERO
    it "evaluates succ pred succ 0 to succ 0" $ do
      eval (T_SUCC (T_PRED (T_SUCC T_ZERO))) `shouldBe` T_SUCC T_ZERO
    it "evaluates pred pred 0 to 0" $ do
      eval (T_PRED (T_PRED T_ZERO)) `shouldBe` T_ZERO
    it "evaluates pred succ pred 0 to 0" $ do
      eval (T_PRED (T_SUCC (T_PRED T_ZERO))) `shouldBe` T_ZERO
    it "evaluates iszero 0 to true" $ do
      eval (T_IS_ZERO T_ZERO) `shouldBe` T_TRUE
    it "evaluates iszero succ 0 to false" $ do
      eval (T_IS_ZERO (T_SUCC T_ZERO)) `shouldBe` T_FALSE
    it "evaluates iszero pred succ 0 to true" $ do
      eval (T_IS_ZERO (T_PRED (T_SUCC T_ZERO))) `shouldBe` T_TRUE
    it "evaluates if true then 0 else succ 0 to 0" $ do
      eval (T_IF_THEN_ELSE T_TRUE T_ZERO (T_SUCC T_ZERO)) `shouldBe` T_ZERO
    it "evaluates if false then 0 else succ 0 to succ 0" $ do
      eval (T_IF_THEN_ELSE T_FALSE T_ZERO (T_SUCC T_ZERO)) `shouldBe`
        T_SUCC T_ZERO
    it "evaluates if iszero 0 then 0 else succ 0 to 0" $ do
      eval (T_IF_THEN_ELSE (T_IS_ZERO T_ZERO) T_ZERO (T_SUCC T_ZERO)) `shouldBe`
        T_ZERO
    it "evaluates if true then pred succ 0 else succ 0 to 0" $ do
      eval (T_IF_THEN_ELSE T_TRUE (T_PRED (T_SUCC T_ZERO)) (T_SUCC T_ZERO)) `shouldBe`
        T_ZERO
    it "evaluates if iszero zero then pred succ 0 else succ 0 to 0" $ do
      eval
        (T_IF_THEN_ELSE
           (T_IS_ZERO T_ZERO)
           (T_PRED (T_SUCC T_ZERO))
           (T_SUCC T_ZERO)) `shouldBe`
        T_ZERO
