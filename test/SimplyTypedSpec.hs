module SimplyTypedSpec where

import SimplyTyped.ParserWrapper
import Test.Hspec

spec :: Spec
spec =
  describe "Simply typed parsing" $ do
    it "simple variable" $ show (fullParser "x") `shouldBe` "x"
    it "bool identity" $ show (fullParser "\\x:Bool.x") `shouldBe` "\\x:Bool.x"
    it "arrow identity" $
      show (fullParser "\\x:Bool->Bool.x") `shouldBe` "\\x:Bool->Bool.x"
    it "2 params lambda" $
      show (fullParser "\\x:Bool.\\y:Bool.x") `shouldBe` "\\x:Bool.\\y:Bool.x"
    it "simple application" $ show (fullParser "x $ y") `shouldBe` "x $ y"
    it "double application" $
      show (fullParser "x $ y $ z") `shouldBe` "x $ y $ z"
    it "application with parens" $
      show (fullParser "x $ (y $ z)") `shouldBe` "x $ (y $ z)"
    it "application of lambda" $
      show (fullParser "(\\x:Bool.x) $ x") `shouldBe` "(\\x:Bool.x) $ x"
    it "if then else" $
      show (fullParser "if x then y else z") `shouldBe` "if x then y else z"
    it "application inside if then else" $
      show (fullParser "if x then y else z $ f") `shouldBe`
      "if x then y else z $ f"
    it "application of if then else" $
      show (fullParser "(if x then y else z) $ f") `shouldBe`
      "(if x then y else z) $ f"
