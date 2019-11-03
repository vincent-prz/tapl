import Arith.Arith
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    describe "Arith Parsing" $ do
      it "parses constant zero" $ do fullParser "0" `shouldBe` Right T_ZERO
      it "parses constant true" $ do fullParser "true" `shouldBe` Right T_TRUE
      it "parses simple call to succ" $ do
        fullParser "succ 0" `shouldBe` Right (T_SUCC T_ZERO)
      it "parses double call to succ" $ do
        fullParser "succ succ 0" `shouldBe` Right (T_SUCC (T_SUCC T_ZERO))
