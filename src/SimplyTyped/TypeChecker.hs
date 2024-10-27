module SimplyTyped.TypeChecker
  ( TypeContext,
    TypingError (..),
    typecheck,
  )
where

import Control.Monad.State (MonadState (get), MonadTrans (lift), StateT, evalStateT, modify)
import qualified Data.Map as Map
import SimplyTyped.Definitions

type TypeContext = Map.Map String Type

data TypingError
  = ArgMisMatch
      { expected :: Type,
        got :: Type
      }
  | FuncAppliedToConst Type
  | IfGuardNotBool Type
  | IfBranchesTypeMismatch
      Type
      Type
  | UnboundVariable String -- approx: unbound var is a typing error
  | AscriptionMismatch
      { expected :: Type,
        got :: Type
      }
  | ProjAppliedToNonPair Type
  | OutOfBoundProj Int
  deriving (Eq, Show)

typecheck :: Term -> Either TypingError Type
typecheck t = evalStateT (typecheckWithContext t) Map.empty

typecheckWithContext :: Term -> StateT TypeContext (Either TypingError) Type
typecheckWithContext (Var s) = do
  ctx <- get
  case ctx Map.!? s of
    Nothing -> lift $ Left $ UnboundVariable s
    Just t -> lift $ Right t
typecheckWithContext (Abs (Just s) t b) = do
  modify (Map.insert s t)
  Arrow t <$> typecheckWithContext b
typecheckWithContext (Abs Nothing t b) =
  Arrow t <$> typecheckWithContext b
typecheckWithContext (App t2 (Assign s t1)) = do
  typ1 <- typecheckWithContext t1
  modify (Map.insert s typ1)
  typ2 <- typecheckWithContext t2
  lift $ typecheckApplication typ2 TUnit
typecheckWithContext (App t1 t2) = do
  typ1 <- typecheckWithContext t1
  typ2 <- typecheckWithContext t2
  lift $ typecheckApplication typ1 typ2
typecheckWithContext ConstTrue = lift $ Right TBool
typecheckWithContext ConstFalse = lift $ Right TBool
typecheckWithContext (IfThenElse t1 t2 t3) = do
  typ1 <- typecheckWithContext t1
  if typ1 == TBool
    then do
      typ2 <- typecheckWithContext t2
      typ3 <- typecheckWithContext t3
      if typ2 == typ3
        then return typ2
        else lift $ Left $ IfBranchesTypeMismatch typ2 typ3
    else lift $ Left $ IfGuardNotBool typ1
typecheckWithContext ConstZero = lift $ Right TNat
typecheckWithContext (Succ t) = typecheckTerm t TNat TNat
typecheckWithContext (Pred t) = typecheckTerm t TNat TNat
typecheckWithContext (IsZero t) = typecheckTerm t TNat TBool
typecheckWithContext ConstUnit = lift $ Right TUnit
typecheckWithContext (Ascription t ty) = do
  actualType <- typecheckWithContext t
  if actualType == ty
    then return ty
    else lift $ Left $ AscriptionMismatch ty actualType
typecheckWithContext (LetExpr x t1 t2) = do
  ty1 <- typecheckWithContext t1
  modify (Map.insert x ty1)
  typecheckWithContext t2
typecheckWithContext (Tuple ts) = TTuple <$> mapM typecheckWithContext ts
typecheckWithContext (Projection t n) = do
  ty <- typecheckWithContext t
  case ty of
    TTuple ts ->
      if n `elem` [1 .. length ts]
        then return (ts !! (n - 1))
        else lift $ Left (OutOfBoundProj n)
    _ -> lift $ Left (ProjAppliedToNonPair ty)
typecheckWithContext (Assign _ t) = typecheckWithContext t >>= const (return TUnit)

typecheckTerm :: Term -> Type -> Type -> StateT TypeContext (Either TypingError) Type
typecheckTerm t expected output = do
  ctx <- get
  typ <- typecheckWithContext t
  if typ == expected
    then return output
    else lift $ Left $ ArgMisMatch {expected = expected, got = typ}

-- check that t1 can be applied to t2
typecheckApplication :: Type -> Type -> Either TypingError Type
typecheckApplication (Arrow t1 t2) t3 =
  if t1 == t3
    then Right t2
    else Left (ArgMisMatch {expected = t1, got = t3})
typecheckApplication t _ = Left $ FuncAppliedToConst t
