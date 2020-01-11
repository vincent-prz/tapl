module SimplyTypedSpec where

import SimplyTyped.Evaluator
import SimplyTyped.Lexer
import SimplyTyped.Parser
import SimplyTyped.TypeChecker
import Test.Hspec

parseThenEval :: String -> Term
parseThenEval s =
  case fullParser s of
    Left err -> error err
    Right term -> evalTerm term

parseThenTypeCheck :: String -> Either TypingError Type
parseThenTypeCheck s =
  case fullParser s of
    Left err -> error err
    Right term -> typecheck term

spec :: Spec
spec = do
  describe "Simply typed parsing" $ do
    it "simple variable" $ show <$> fullParser "x" `shouldBe` Right "x"
    it "Bool identity" $
      show <$> fullParser "\\x:Bool.x" `shouldBe` Right "\\x:Bool.x"
    it "arrow identity" $
      show <$> fullParser "\\x:Bool->Bool.x" `shouldBe` Right "\\x:Bool->Bool.x"
    it "2 params lambda" $
      show <$>
      fullParser "\\x:Bool.\\y:Bool.x" `shouldBe` Right "\\x:Bool.\\y:Bool.x"
    it "simple application" $
      show <$> fullParser "x $ y" `shouldBe` Right "x $ y"
    it "double application" $
      show <$> fullParser "x $ y $ z" `shouldBe` Right "x $ y $ z"
    it "application with parens" $
      show <$> fullParser "x $ (y $ z)" `shouldBe` Right "x $ (y $ z)"
    it "application of lambda" $
      show <$> fullParser "(\\x:Bool.x) $ x" `shouldBe` Right "(\\x:Bool.x) $ x"
    it "if then else" $
      show <$>
      fullParser "if x then y else z" `shouldBe` Right "if x then y else z"
    it "application inside if then else" $
      show <$>
      fullParser "if x then y else z $ f" `shouldBe`
      Right "if x then y else z $ f"
    it "application of if then else" $
      show <$>
      fullParser "(if x then y else z) $ f" `shouldBe`
      Right "(if x then y else z) $ f"
  describe "Simply typed typechecking" $ do
    it "true" $ show <$> parseThenTypeCheck "true" `shouldBe` Right "Bool"
    it "false" $ show <$> parseThenTypeCheck "false" `shouldBe` Right "Bool"
    it "Bool identity" $
      show <$> parseThenTypeCheck "\\x:Bool.x" `shouldBe` Right "Bool->Bool"
    it "Bool->Bool identity" $
      show <$>
      parseThenTypeCheck "\\x:Bool->Bool.x" `shouldBe`
      Right "(Bool->Bool)->Bool->Bool"
    it "application of id to true" $
      show <$> parseThenTypeCheck "(\\x:Bool.x) $ true" `shouldBe` Right "Bool"
    it "2 arg func" $
      show <$>
      parseThenTypeCheck "\\x:Bool. \\y:Bool. x" `shouldBe`
      Right "Bool->Bool->Bool"
    it "application of 2 arg func" $
      show <$>
      parseThenTypeCheck "(\\x:Bool. \\y:Bool. x) $ false $ true" `shouldBe`
      Right "Bool"
    it "currying of 2 arg func" $
      show <$>
      parseThenTypeCheck "(\\x:Bool. \\y:Bool. x) $ false" `shouldBe`
      Right "Bool->Bool"
    it "lambda taking function as arg" $
      show <$>
      parseThenTypeCheck "\\f:Bool->Bool.f $ (f $ true)" `shouldBe`
      Right "(Bool->Bool)->Bool"
    it "failure when applying Bool identity to function" $
      parseThenTypeCheck "(\\x:Bool.x) $ (\\x:Bool.x)" `shouldBe`
      Left (ArgMisMatch {expected = TBool, got = Arrow TBool TBool})
    it "failure when applying something to Bool constant" $
      parseThenTypeCheck "true $ false" `shouldBe`
      Left (FuncAppliedToConst TBool)
    it "if then else" $
      show <$>
      parseThenTypeCheck "if true then true else false" `shouldBe` Right "Bool"
    it "if then else of functions" $
      show <$>
      parseThenTypeCheck "if true then \\x:Bool.x else \\y:Bool.y" `shouldBe`
      Right "Bool->Bool"
    it "failure when condition of if is not Bool" $
      parseThenTypeCheck "if \\x:Bool.x then true else false" `shouldBe`
      Left (IfGuardNotBool (Arrow TBool TBool))
    it "failure when branches of if don't have the same type" $
      parseThenTypeCheck "if false then true else \\x:Bool.x" `shouldBe`
      Left (IfBranchesTypeMismatch TBool (Arrow TBool TBool))
    it "failure when unbound variable" $
      parseThenTypeCheck "\\x:Bool.y" `shouldBe` Left (UnboundVariable "y")
  describe "Simply typed evaluation" $ do
    it "identity" $ show (parseThenEval "\\x:Bool.x") `shouldBe` "\\x:Bool.x"
    it "simple application" $
      show (parseThenEval "(\\f:Bool->Bool.f) $ \\x:Bool.x") `shouldBe`
      "\\x:Bool.x"
    it "const true" $ show (parseThenEval "true") `shouldBe` "true"
    it "const false" $ show (parseThenEval "false") `shouldBe` "false"
    it "identity applied to true" $
      show (parseThenEval "(\\x:Bool.x) $ true") `shouldBe` "true"
    it "if true .." $
      show (parseThenEval "if true then true else false") `shouldBe` "true"
    it "if false .." $
      show (parseThenEval "if false then true else false") `shouldBe` "false"
    it "if expr .." $
      show (parseThenEval "if (\\x:Bool.x) $ true then true else false") `shouldBe`
      "true"
    it "application of lambda with if" $
      show (parseThenEval "(\\x:Bool.if x then true else false) $ true") `shouldBe`
      "true"
