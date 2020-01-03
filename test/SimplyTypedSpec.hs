module SimplyTypedSpec where

import SimplyTyped.Lexer
import SimplyTyped.ParserWrapper
import SimplyTyped.TypeChecker
import Test.Hspec

spec :: Spec
spec = do
  describe "Simply typed parsing" $ do
    it "simple variable" $ show (fullParser "x") `shouldBe` "x"
    it "Bool identity" $ show (fullParser "\\x:Bool.x") `shouldBe` "\\x:Bool.x"
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
  describe "Simply typed typechecking" $ do
    it "tru" $ show <$> typecheck (fullParser "tru") `shouldBe` Right "Bool"
    it "fls" $ show <$> typecheck (fullParser "fls") `shouldBe` Right "Bool"
    it "Bool identity" $
      show <$> typecheck (fullParser "\\x:Bool.x") `shouldBe` Right "Bool->Bool"
    it "Bool->Bool identity" $
      show <$>
      typecheck (fullParser "\\x:Bool->Bool.x") `shouldBe`
      Right "(Bool->Bool)->Bool->Bool"
    it "application of id to tru" $
      show <$>
      typecheck (fullParser "(\\x:Bool.x) $ tru") `shouldBe` Right "Bool"
    it "2 arg func" $
      show <$>
      typecheck (fullParser "\\x:Bool. \\y:Bool. x") `shouldBe`
      Right "Bool->Bool->Bool"
    it "application of 2 arg func" $
      show <$>
      typecheck (fullParser "(\\x:Bool. \\y:Bool. x) $ fls $ tru") `shouldBe`
      Right "Bool"
    it "currying of 2 arg func" $
      show <$>
      typecheck (fullParser "(\\x:Bool. \\y:Bool. x) $ fls") `shouldBe`
      Right "Bool->Bool"
    it "lambda taking function as arg" $
      show <$>
      typecheck (fullParser "\\f:Bool->Bool.f $ (f $ tru)") `shouldBe`
      Right "(Bool->Bool)->Bool"
    it "failure when applying Bool identity to function" $
      typecheck (fullParser "(\\x:Bool.x) $ (\\x:Bool.x)") `shouldBe`
      Left (ArgMisMatch {expected = TBool, got = Arrow TBool TBool})
    it "failure when applying something to Bool constant" $
      typecheck (fullParser "tru $ fls") `shouldBe`
      Left (FuncAppliedToConst TBool)
    it "if then else" $
      show <$>
      typecheck (fullParser "if tru then tru else fls") `shouldBe` Right "Bool"
    it "if then else of functions" $
      show <$>
      typecheck (fullParser "if tru then \\x:Bool.x else \\y:Bool.y") `shouldBe`
      Right "Bool->Bool"
    it "failure when condition of if is not Bool" $
      typecheck (fullParser "if \\x:Bool.x then tru else fls") `shouldBe`
      Left (IfCondNotBool (Arrow TBool TBool))
    it "failure when branches of if don't have the same type" $
      typecheck (fullParser "if fls then tru else \\x:Bool.x") `shouldBe`
      Left (IfBranchesTypeMismatch TBool (Arrow TBool TBool))
