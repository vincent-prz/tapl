module Untyped.Evaluator where

import Data.List (elemIndex)
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

--
--atMay :: [a] -> Int -> Maybe a
--atMay [] _ = Nothing
--atMay (h:_) 0 = Just h
--atMay (_:t) n = atMay t (n - 1)
--
maybeToEither :: b -> Maybe a -> Either b a
maybeToEither b Nothing = Left b
maybeToEither _ (Just a) = Right a

data RuntimeError =
  RemoveNameError
  deriving (Eq, Show)

removeNames :: Context -> Term -> Either RuntimeError NamelessTerm
removeNames c (T_VARIABLE s) =
  maybeToEither RemoveNameError $ fmap NT_VAR (elemIndex s c)
removeNames c (T_ABSTRACTION _ _) = Right (NT_ABS (NT_VAR 0))

restoreNames :: Context -> NamelessTerm -> Term
restoreNames c t = undefined

shift :: Int -> Int -> NamelessTerm -> NamelessTerm
shift = undefined

substitution :: Int -> NamelessTerm -> NamelessTerm -> NamelessTerm
substitution = undefined

eval :: Term -> Term
--eval = restoreNames [] . evalNameless . removeNames []
eval = undefined

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
