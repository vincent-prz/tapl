module SimplyTyped.Evaluator where

import qualified Data.Map as Map
import SimplyTyped.Parser (Term(..))

data RuntimeError
  = ParsingError
  | UnboundVariable String
  deriving (Eq, Show)

type Context = Map.Map String Term

getFreeVars :: Term -> [String]
getFreeVars = g []
  where
    g :: [String] -> Term -> [String]
    g boundVars (Var s)
      | s `elem` boundVars = []
      | otherwise = [s]
    g boundVars (App t1 t2) = g boundVars t1 ++ g boundVars t2
    g boundVars (Abs s _ t) = g (s : boundVars) t
    g _ ConstTrue = []
    g _ ConstFalse = []
    g boundVars (IfThenElse t1 t2 t3) =
      g boundVars t1 ++ g boundVars t2 ++ g boundVars t3

pickFreshName :: String -> [String] -> String
pickFreshName s l
  | s `notElem` l = s
pickFreshName s l = g 1 s l
  where
    g :: Int -> String -> [String] -> String
    g n name existingNames
      | name ++ show n `elem` existingNames = g (n + 1) name existingNames
      | otherwise = name ++ show n

substitution :: String -> Term -> Term -> Term
substitution x s (Var y)
  | x == y = s
  | otherwise = Var y
substitution x s (App t1 t2) = App (substitution x s t1) (substitution x s t2)
substitution x _ t@(Abs y _ _)
  | x == y = t
substitution x s (Abs y typ t1) =
  let fv = getFreeVars s
   in if y `notElem` fv
        then Abs y typ (substitution x s t1)
        else let freshName = pickFreshName y fv
                 t1' = substitution y (Var freshName) t1
              in Abs freshName typ (substitution x s t1')
substitution _ _ ConstTrue = ConstTrue
substitution _ _ ConstFalse = ConstFalse
substitution x s (IfThenElse t1 t2 t3) =
  IfThenElse (substitution x s t1) (substitution x s t2) (substitution x s t3)

checkVarsAreBound :: Context -> Term -> Either String ()
checkVarsAreBound c = g (Map.keys c)
  where
    g :: [String] -> Term -> Either String ()
    g boundVars (Var x)
      | x `elem` boundVars = Right ()
      | otherwise = Left x
    g boundVars (App t1 t2) = g boundVars t1 >> g boundVars t2
    g boundVars (Abs x _ t) = g (x : boundVars) t
    g _ ConstTrue = Right ()
    g _ ConstFalse = Right ()
    g boundVars (IfThenElse t1 t2 t3) =
      g boundVars t1 >> g boundVars t2 >> g boundVars t3

evalTerm :: Term -> Either RuntimeError Term
evalTerm term =
  case checkVarsAreBound Map.empty term of
    Left x -> Left (UnboundVariable x)
    Right _ -> Right (actualEval term)
  where
    actualEval :: Term -> Term
    actualEval t =
      let newTerm = eval1Step t
       in if newTerm == t
            then newTerm
            else actualEval newTerm

isValue :: Term -> Bool
isValue ConstTrue = True
isValue ConstFalse = True
isValue Abs {} = True
isValue _ = False

-- call by value
eval1Step :: Term -> Term
eval1Step (App (Abs x _ t12) v2)
  | isValue v2 = substitution x v2 t12
eval1Step (App v1@Abs {} t2) = App v1 (eval1Step t2)
eval1Step (App t1 t2) = App (eval1Step t1) t2
eval1Step (IfThenElse ConstTrue t2 _) = t2
eval1Step (IfThenElse ConstFalse _ t3) = t3
eval1Step (IfThenElse t1 t2 t3) = IfThenElse (eval1Step t1) t2 t3
eval1Step t = t
-- beta evaluation
--evalBeta1Step :: Term -> Term
--evalBeta1Step (App (Abs (Var x) t12) t2) = substitution x t2 t12
--evalBeta1Step (App t1 t2) = App (evalBeta1Step t1) (evalBeta1Step t2)
--evalBeta1Step (Abs (Var x) t) = Abs (Var x) (evalBeta1Step t)
--evalBeta1Step t = t
