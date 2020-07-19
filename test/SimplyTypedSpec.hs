module SimplyTypedSpec where

import SimplyTyped.Evaluator
import SimplyTyped.Lexer
import SimplyTyped.Parser
import SimplyTyped.Desugar
import SimplyTyped.TypeChecker
import Test.Hspec

parseThenEval :: String -> Term
parseThenEval s =
  case fullParser s of
    Left err -> error err
    Right terms -> evalTerm (desugar terms)

parseThenTypeCheck :: String -> Either TypingError Type
parseThenTypeCheck s =
  case fullParser s of
    Left err -> error err
    Right terms -> typecheck (desugar terms)

spec :: Spec
spec = do
  describe "Simply typed parsing" $ do
    it "simple variable" $ show <$> fullParser "x" `shouldBe` Right "[x]"
    it "Bool identity" $
      show <$> fullParser "\\x:Bool.x" `shouldBe` Right "[\\x:Bool.x]"
    it "arrow identity" $
      show <$> fullParser "\\x:Bool->Bool.x" `shouldBe` Right "[\\x:Bool->Bool.x]"
    it "2 params lambda" $
      show <$>
      fullParser "\\x:Bool.\\y:Bool.x" `shouldBe` Right "[\\x:Bool.\\y:Bool.x]"
    it "simple application" $
      show <$> fullParser "x $ y" `shouldBe` Right "[x $ y]"
    it "double application" $
      show <$> fullParser "x $ y $ z" `shouldBe` Right "[x $ y $ z]"
    it "application with parens" $
      show <$> fullParser "x $ (y $ z)" `shouldBe` Right "[x $ (y $ z)]"
    it "application of lambda" $
      show <$> fullParser "(\\x:Bool.x) $ x" `shouldBe` Right "[(\\x:Bool.x) $ x]"
    it "if then else" $
      show <$>
      fullParser "if x then y else z" `shouldBe` Right "[if x then y else z]"
    it "application inside if then else" $
      show <$>
      fullParser "if x then y else z $ f" `shouldBe`
      Right "[if x then y else z $ f]"
    it "application of if then else" $
      show <$>
      fullParser "(if x then y else z) $ f" `shouldBe`
      Right "[(if x then y else z) $ f]"
    -- check that application binds more tightly than else
    it "application in else branch" $
      show <$>
      fullParser "if true then true else (\\x:Bool.x) $ true" `shouldBe`
      Right "[if true then true else (\\x:Bool.x) $ true]"
    it "0" $ show <$> fullParser "0" `shouldBe` Right "[0]"
    it "succ 0" $ show <$> fullParser "succ 0" `shouldBe` Right "[succ 0]"
    it "succ (succ 0)" $
      show <$> fullParser "succ (succ 0)" `shouldBe` Right "[succ succ 0]"
    it "unit" $ show <$> fullParser "()" `shouldBe` Right "[()]"
    it "unit then var" $ show <$> fullParser "();x" `shouldBe` Right "[(),x]"
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
    it "0" $ show <$> parseThenTypeCheck "0" `shouldBe` Right "Nat"
    it "succ 0" $ show <$> parseThenTypeCheck "succ 0" `shouldBe` Right "Nat"
    it "lambda succ" $
      show <$> parseThenTypeCheck "\\x:Nat.succ x" `shouldBe` Right "Nat->Nat"
    it "failure when succ true" $
      parseThenTypeCheck "succ true" `shouldBe`
      Left (ArgMisMatch {expected = TNat, got = TBool})
    it "failure when lambda succ with wrong input type" $
      parseThenTypeCheck "\\x:Bool.succ x" `shouldBe`
      Left (ArgMisMatch {expected = TNat, got = TBool})
    it "pred 0" $ show <$> parseThenTypeCheck "pred 0" `shouldBe` Right "Nat"
    it "pred succ 0" $
      show <$> parseThenTypeCheck "pred succ 0" `shouldBe` Right "Nat"
    it "failure when pred true" $
      parseThenTypeCheck "pred true" `shouldBe`
      Left (ArgMisMatch {expected = TNat, got = TBool})
    it "iszero 0" $
      show <$> parseThenTypeCheck "iszero 0" `shouldBe` Right "Bool"
    it "iszero succ 0" $
      show <$> parseThenTypeCheck "iszero 0" `shouldBe` Right "Bool"
    it "failure when iszero true" $
      parseThenTypeCheck "iszero true" `shouldBe`
      Left (ArgMisMatch {expected = TNat, got = TBool})
    it "unit" $ show <$> parseThenTypeCheck "()" `shouldBe` Right "Unit"
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
    it "const 0" $ show (parseThenEval "0") `shouldBe` "0"
    it "succ 0" $ show (parseThenEval "succ 0") `shouldBe` "succ 0"
    it "lambda ConstZero applied to 0" $
      show (parseThenEval "(\\x:Nat.0) $ 0") `shouldBe` "0"
    it "lambda succ applied to 0" $
      show (parseThenEval "(\\x:Nat.succ x) $ 0") `shouldBe` "succ 0"
    it "lambda ConstZero applied to succ 0" $
      show (parseThenEval "(\\x:Nat.0) $ (succ 0)") `shouldBe` "0"
    it "succ of application" $
      show (parseThenEval "succ (\\x:Nat.x) $ 0") `shouldBe` "succ 0"
    it "pred 0" $ show (parseThenEval "pred 0") `shouldBe` "0"
    it "pred succ 0" $ show (parseThenEval "pred succ 0") `shouldBe` "0"
    it "lambda pred" $
      show (parseThenEval "\\x:Nat.pred x") `shouldBe` "\\x:Nat.pred x"
    it "lambda pred applied to 0" $
      show (parseThenEval "(\\x:Nat.pred x) $ 0") `shouldBe` "0"
    it "iszero 0" $ show (parseThenEval "iszero 0") `shouldBe` "true"
    it "iszero succ 0" $ show (parseThenEval "iszero succ 0") `shouldBe` "false"
    it "lambda iszero" $
      show (parseThenEval "\\x:Nat.iszero x") `shouldBe` "\\x:Nat.iszero x"
    it "lambda iszero applied to 0" $
      show (parseThenEval "(\\x:Nat.iszero x) $ 0") `shouldBe` "true"
    it "iszero pred succ 0" $
      show (parseThenEval "iszero pred succ 0") `shouldBe` "true"
    it "unit" $ show (parseThenEval "()") `shouldBe` "()"
  describe "Simply typed sequencing" $ do
    it "unit then id bool" $ show (parseThenEval "();\\x:Bool.x") `shouldBe` "\\x:Bool.x"
    it "unit then identity applied to true" $
      show (parseThenEval "();(\\x:Bool.x) $ true") `shouldBe` "true"
    it "unit then const 0" $ show (parseThenEval "();0") `shouldBe` "0"
    it "unit then unit" $ show (parseThenEval "();()") `shouldBe` "()"
    it "unit x2 then const 0" $ show (parseThenEval "();();0") `shouldBe` "0"
    it "unit x2 then identity applied to true" $
      show (parseThenEval "();();(\\x:Bool.x) $ true") `shouldBe` "true"
