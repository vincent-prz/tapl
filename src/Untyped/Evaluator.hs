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
  show (NT_APP t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"

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
