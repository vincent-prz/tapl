module SimplyTyped.Definitions where

data Type
  = TBool
  | TNat
  | Arrow Type
          Type
  | TUnit
  deriving (Eq)

instance Show Type where
  show TBool = "Bool"
  show TNat = "Nat"
  show (Arrow t1 t2) = showL t1 ++ "->" ++ show t2
    where
      showL arr@(Arrow _ _) = "(" ++ show arr ++ ")"
      showL t = show t
  show TUnit = "Unit"

data Term
  = Var String
  | Abs String
        Type
        Term
  | App Term
        Term
  | ConstTrue
  | ConstFalse
  | IfThenElse Term
               Term
               Term
  | ConstZero
  | Succ Term
  | Pred Term
  | IsZero Term
  | ConstUnit
  -- below is syntactic sugar
  | Ascription Term
               Type
  | Let String Term Term
  deriving (Eq)

instance Show Term where
  show (Var s) = s
  show (Abs s t b) = "\\" ++ s ++ ":" ++ show t ++ "." ++ show b
  show ConstTrue = "true"
  show ConstFalse = "false"
  show (IfThenElse t1 t2 t3) =
    "if " ++ show t1 ++ " then " ++ show t2 ++ " else " ++ show t3
  show ConstZero = "0"
  show ConstUnit = "()"
  show (Succ t) = "succ " ++ show t
  show (Pred t) = "pred " ++ show t
  show (IsZero t) = "iszero " ++ show t
  show (Ascription t ty) = show t ++ "as" ++ show ty
  show (App t1 t2) = showL t1 ++ " $ " ++ showR t2
    where
      showL (Var s) = s
      showL t@Abs {} = "(" ++ show t ++ ")"
      showL t@(App _ _) = show t
      showL t@IfThenElse {} = "(" ++ show t ++ ")"
      showL t@Succ {} = "(" ++ show t ++ ")"
      showL t@Pred {} = "(" ++ show t ++ ")"
      showL t@IsZero {} = "(" ++ show t ++ ")"
      showL t = show t
      showR t@(App _ _) = "(" ++ show t ++ ")"
      showR t = show t

data CoreTerm
  = CoVar String
  | CoAbs String
          Type
          CoreTerm
  | CoApp CoreTerm
          CoreTerm
  | CoConstTrue
  | CoConstFalse
  | CoIfThenElse CoreTerm
                 CoreTerm
                 CoreTerm
  | CoConstZero
  | CoSucc CoreTerm
  | CoPred CoreTerm
  | CoIsZero CoreTerm
  | CoConstUnit
  deriving (Eq)

instance Show CoreTerm where
  show (CoVar s) = s
  show (CoAbs s t b) = "\\" ++ s ++ ":" ++ show t ++ "." ++ show b
  show CoConstTrue = "true"
  show CoConstFalse = "false"
  show (CoIfThenElse t1 t2 t3) =
    "if " ++ show t1 ++ " then " ++ show t2 ++ " else " ++ show t3
  show CoConstZero = "0"
  show CoConstUnit = "()"
  show (CoSucc t) = "succ " ++ show t
  show (CoPred t) = "pred " ++ show t
  show (CoIsZero t) = "iszero " ++ show t
  show (CoApp t1 t2) = showL t1 ++ " $ " ++ showR t2
    where
      showL (CoVar s) = s
      showL t@CoAbs {} = "(" ++ show t ++ ")"
      showL t@(CoApp _ _) = show t
      showL t@CoIfThenElse {} = "(" ++ show t ++ ")"
      showL t@CoSucc {} = "(" ++ show t ++ ")"
      showL t@CoPred {} = "(" ++ show t ++ ")"
      showL t@CoIsZero {} = "(" ++ show t ++ ")"
      showL t = show t
      showR t@(CoApp _ _) = "(" ++ show t ++ ")"
      showR t = show t
