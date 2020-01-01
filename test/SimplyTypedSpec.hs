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
