module SimplyTyped.TypeChecker where

import qualified Data.Map as Map
import SimplyTyped.Lexer
import SimplyTyped.Parser

type TypeContext = Map.Map String Type

data TypingError
  = ArgMisMatch { expected :: Type
                , got :: Type }
  | FuncAppliedToConst Type
  | IfGuardNotBool Type
  | IfBranchesTypeMismatch Type
                           Type
  | UnboundVariable String -- approx: unbound var is a typing error
  deriving (Eq, Show)

typecheck :: Term -> Either TypingError Type
typecheck = typecheckWithContext Map.empty

typecheckWithContext :: TypeContext -> Term -> Either TypingError Type
typecheckWithContext ctx (Var s) =
  case ctx Map.!? s of
    Nothing -> Left $ UnboundVariable s
    Just t -> Right t
typecheckWithContext ctx (Abs s t b) =
  Arrow t <$> typecheckWithContext (Map.insert s t ctx) b
typecheckWithContext ctx (App t1 t2) = do
  typ1 <- typecheckWithContext ctx t1
  typ2 <- typecheckWithContext ctx t2
  typecheckApplication typ1 typ2
typecheckWithContext _ ConstTrue = Right TBool
typecheckWithContext _ ConstFalse = Right TBool
typecheckWithContext ctx (IfThenElse t1 t2 t3) = do
  typ1 <- typecheckWithContext ctx t1
  if typ1 == TBool
    then do
      typ2 <- typecheckWithContext ctx t2
      typ3 <- typecheckWithContext ctx t3
      if typ2 == typ3
        then return typ2
        else Left $ IfBranchesTypeMismatch typ2 typ3
    else Left $ IfGuardNotBool typ1
typecheckWithContext _ ConstZero = Right TNat
typecheckWithContext ctx (Succ t) = do
  typ <- typecheckWithContext ctx t
  if typ == TNat
    then return TNat
    else Left $ ArgMisMatch {expected = TNat, got = typ}
typecheckWithContext ctx (Pred t) = do
  typ <- typecheckWithContext ctx t
  if typ == TNat
    then return TNat
    else Left $ ArgMisMatch {expected = TNat, got = typ}

-- check that t1 can be applied to t2
typecheckApplication :: Type -> Type -> Either TypingError Type
typecheckApplication (Arrow t1 t2) t3 =
  if t1 == t3
    then Right t2
    else Left (ArgMisMatch {expected = t1, got = t3})
typecheckApplication t _ = Left $ FuncAppliedToConst t
