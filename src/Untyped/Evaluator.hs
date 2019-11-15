module Untyped.Evaluator where

import Control.Monad ((<=<))
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Untyped.Parser (Term(..))

data NamelessTerm
  = NT_VAR Int
  | NT_ABS NamelessTerm
  | NT_APP NamelessTerm
           NamelessTerm
  deriving (Eq)

instance Show NamelessTerm where
  show (NT_VAR x) = show x
  show (NT_ABS t) = "\\" ++ "." ++ show t
  show (NT_APP t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"

type Context = [String]

type NameGenerator = [String] -> String

atMay :: [a] -> Int -> Maybe a
atMay [] _ = Nothing
atMay (h:_) 0 = Just h
atMay (_:t) n = atMay t (n - 1)

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither b Nothing = Left b
maybeToEither _ (Just a) = Right a

data RuntimeError
  = RemoveNameError
  | RestoreNameError
  deriving (Eq, Show)

removeNames :: Context -> Term -> Either RuntimeError NamelessTerm
removeNames c (T_VAR s) =
  maybeToEither RemoveNameError $ fmap NT_VAR (elemIndex s c)
removeNames c (T_APP t1 t2) = NT_APP <$> removeNames c t1 <*> removeNames c t2
removeNames c (T_ABS (T_VAR s) t) = NT_ABS <$> removeNames (s : c) t

restoreNames ::
     NameGenerator -> Context -> NamelessTerm -> Either RuntimeError Term
restoreNames _ c (NT_VAR n) =
  maybeToEither RestoreNameError $ fmap T_VAR (atMay c n)
restoreNames ng c (NT_APP t1 t2) =
  T_APP <$> restoreNames ng c t1 <*> restoreNames ng c t2
restoreNames ng c (NT_ABS t) =
  let newName = ng c
   in T_ABS (T_VAR newName) <$> restoreNames ng (newName : c) t

-- FIXME: stops working after 26 names!
genNewVarName :: NameGenerator
genNewVarName [] = "a"
genNewVarName (h:_) =
  let letters = map (: []) ['a' .. 'z']
      indexOfLast :: Maybe Int
      indexOfLast = elemIndex h letters
      newLetter :: Maybe String
      newLetter = ((+ 1) <$> indexOfLast) >>= atMay letters
   in fromMaybe "a" newLetter --FIXME: swallowing error here

shift :: Int -> Int -> NamelessTerm -> NamelessTerm
shift d c v@(NT_VAR k)
  | k < c = v
  | otherwise = NT_VAR (k + d)
shift d c (NT_ABS t) = NT_ABS (shift d (c + 1) t)
shift d c (NT_APP t1 t2) = NT_APP (shift d c t1) (shift d c t2)

substitution :: Int -> NamelessTerm -> NamelessTerm -> NamelessTerm
substitution j s v@(NT_VAR k)
  | k == j = s
  | otherwise = v
substitution j s (NT_ABS t1) = NT_ABS (substitution (j + 1) (shift 1 0 s) t1)
substitution j s (NT_APP t1 t2) =
  NT_APP (substitution j s t1) (substitution j s t2)

eval :: Term -> Either RuntimeError Term
eval =
  restoreNames genNewVarName [] <=< return . evalNameless <=< removeNames []

evalNameless :: NamelessTerm -> NamelessTerm
evalNameless term =
  let newTerm = evalNameless1Step term
   in if newTerm == term
        then newTerm
        else evalNameless newTerm

evalNameless1Step :: NamelessTerm -> NamelessTerm
evalNameless1Step (NT_APP (NT_ABS t12) v2@(NT_ABS _)) =
  let shiftedv2 = shift 1 0 v2
      substituedt12 = substitution 0 shiftedv2 t12
   in shift (-1) 0 substituedt12
evalNameless1Step (NT_APP v1@(NT_ABS _) t2) = NT_APP v1 (evalNameless1Step t2)
evalNameless1Step (NT_APP t1 t2) = NT_APP (evalNameless t1) t2
evalNameless1Step t = t
