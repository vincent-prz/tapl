module UntypedSpec where

import Test.Hspec
import Untyped.Parser

spec :: Spec
spec = do
  describe "Untyped Parsing" $ do
    it "parses simple variable" $ do
      fullParser "x" `shouldBe` (Right $ T_VARIABLE "x")
    it "parses identity lambda" $ do
      fullParser "\\x.x" `shouldBe` (Right $ T_ABSTRACTION (T_VARIABLE "x"))
    it "parses simple application" $ do
      fullParser "x y" `shouldBe`
        (Right $ T_APPLICATION (T_VARIABLE "x") (T_VARIABLE "y"))
    it "parses application of identity" $ do
      fullParser "\\x.x y" `shouldBe`
        (Right $ T_APPLICATION (T_ABSTRACTION (T_VARIABLE "x")) (T_VARIABLE "y"))
