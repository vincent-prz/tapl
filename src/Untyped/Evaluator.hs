module Untyped.Evaluator where

import Control.Monad ((<=<))
import Data.List (elemIndex)
import Untyped.Parser (Term(..))

-- this is not really a "nameless" term, since we are remembering the names in disguise
-- in order to restore them later, rather than generating arbitrary names
data NamelessTerm
  = NT_VAR Int
  | NT_ABS String
           NamelessTerm
  | NT_APP NamelessTerm
           NamelessTerm
  deriving (Eq)

instance Show NamelessTerm where
  show (NT_VAR x) = show x
  show (NT_ABS _ t) = "\\" ++ "." ++ show t
  show (NT_APP t1 t2) = showL t1 ++ " " ++ showR t2
    where
      showL (NT_VAR x) = show x
      showL t@(NT_ABS _ _) = "(" ++ show t ++ ")"
      showL t@(NT_APP _ _) = show t
      showR (NT_VAR x) = show x
      showR t@(NT_ABS _ _) = show t
      showR t@(NT_APP _ _) = "(" ++ show t ++ ")"

type Context = [String]

atMay :: [a] -> Int -> Maybe a
atMay [] _ = Nothing
atMay (h:_) 0 = Just h
atMay (_:t) n = atMay t (n - 1)

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither b Nothing = Left b
maybeToEither _ (Just a) = Right a

data RuntimeError
  = ParsingError -- FIXME:only here for tests, remove this
  | UnboundVariable String
  | RestoreNameError
  deriving (Eq, Show)

removeNames :: Context -> Term -> Either RuntimeError NamelessTerm
removeNames c (T_VAR s) =
  maybeToEither (UnboundVariable s) $ fmap NT_VAR (elemIndex s c)
removeNames c (T_APP t1 t2) = NT_APP <$> removeNames c t1 <*> removeNames c t2
removeNames c (T_ABS (T_VAR s) t) = NT_ABS s <$> removeNames (s : c) t

restoreNames :: Context -> NamelessTerm -> Either RuntimeError Term
restoreNames c (NT_VAR n) =
  maybeToEither RestoreNameError $ fmap T_VAR (atMay c n)
restoreNames c (NT_APP t1 t2) =
  T_APP <$> restoreNames c t1 <*> restoreNames c t2
restoreNames c (NT_ABS s t) = T_ABS (T_VAR s) <$> restoreNames (s : c) t

shift :: Int -> Int -> NamelessTerm -> NamelessTerm
shift d c v@(NT_VAR k)
  | k < c = v
  | otherwise = NT_VAR (k + d)
shift d c (NT_ABS s t) = NT_ABS s (shift d (c + 1) t)
shift d c (NT_APP t1 t2) = NT_APP (shift d c t1) (shift d c t2)

substitution :: Int -> NamelessTerm -> NamelessTerm -> NamelessTerm
substitution j s v@(NT_VAR k)
  | k == j = s
  | otherwise = v
substitution j s (NT_ABS name t1) =
  NT_ABS name (substitution (j + 1) (shift 1 0 s) t1)
substitution j s (NT_APP t1 t2) =
  NT_APP (substitution j s t1) (substitution j s t2)

eval :: Term -> Either RuntimeError Term
eval = restoreNames [] <=< return . evalNameless <=< removeNames []

evalNameless :: NamelessTerm -> NamelessTerm
evalNameless term =
  let newTerm = evalNameless1Step term
   in if newTerm == term
        then newTerm
        else evalNameless newTerm

evalNameless1Step :: NamelessTerm -> NamelessTerm
evalNameless1Step (NT_APP (NT_ABS _ t12) v2@(NT_ABS _ _)) =
  let shiftedv2 = shift 1 0 v2
      substituedt12 = substitution 0 shiftedv2 t12
   in shift (-1) 0 substituedt12
evalNameless1Step (NT_APP v1@(NT_ABS _ _) t2) = NT_APP v1 (evalNameless1Step t2)
evalNameless1Step (NT_APP t1 t2) = NT_APP (evalNameless t1) t2
evalNameless1Step t = t

-- beta evaluation
-- TODO: refactor this with `Strategy` pattern
evalBeta :: Term -> Either RuntimeError Term
evalBeta = restoreNames [] <=< return . evalBetaNameless <=< removeNames []

evalBetaNameless :: NamelessTerm -> NamelessTerm
evalBetaNameless term =
  let newTerm = evalBetaNameless1Step term
   in if newTerm == term
        then newTerm
        else evalBetaNameless newTerm

evalBetaNameless1Step :: NamelessTerm -> NamelessTerm
evalBetaNameless1Step (NT_APP (NT_ABS _ t12) t2) = substitution 0 t2 t12
evalBetaNameless1Step (NT_APP t1 t2) =
  NT_APP (evalBetaNameless1Step t1) (evalBetaNameless1Step t2)
evalBetaNameless1Step (NT_ABS varname t) =
  NT_ABS varname (evalBetaNameless1Step t)
evalBetaNameless1Step t = t

-- experimental
freeVars :: Term -> [String]
freeVars = g []
  where
    g :: [String] -> Term -> [String]
    g boundVars (T_VAR s)
      | s `elem` boundVars = []
      | otherwise = [s]
    g boundVars (T_APP t1 t2) = g boundVars t1 ++ g boundVars t2
    g boundVars (T_ABS (T_VAR s) t) = g (s : boundVars) t

pickFreshName :: String -> [String] -> String
pickFreshName s l
  | s `notElem` l = s
pickFreshName s l = g 1 s l
  where
    g n s l
      | s ++ show n `elem` l = g (n + 1) s l
      | otherwise = s ++ show n

substitution' :: String -> Term -> Term -> Term
substitution' x s (T_VAR y)
  | x == y = s
  | otherwise = T_VAR y
substitution' x s (T_APP t1 t2) =
  T_APP (substitution' x s t1) (substitution' x s t2)
substitution' x s t@(T_ABS (T_VAR y) t1)
  | x == y = t
substitution' x s t@(T_ABS (T_VAR y) t1) =
  let fv = freeVars s
   in if y `notElem` fv
        then T_ABS (T_VAR y) (substitution' x s t1)
        else T_ABS (T_VAR (pickFreshName y fv)) (substitution' x s t1)

evalBeta' :: Term -> Either RuntimeError Term
evalBeta' term = do
  newTerm <- evalBeta1Step' [] term -- this is suspicious
  if newTerm == term
    then return newTerm
    else evalBeta' newTerm

evalBeta1Step' :: Context -> Term -> Either RuntimeError Term
evalBeta1Step' c (T_APP (T_ABS (T_VAR x) t12) t2) =
  Right (substitution' x t2 t12)
evalBeta1Step' c (T_APP t1 t2) =
  T_APP <$> evalBeta1Step' c t1 <*> evalBeta1Step' c t2
evalBeta1Step' c (T_ABS (T_VAR x) t) =
  T_ABS (T_VAR x) <$> evalBeta1Step' (x : c) t
evalBeta1Step' c t@(T_VAR x)
  | x `elem` c = Right t
  | otherwise = Left (UnboundVariable x)
