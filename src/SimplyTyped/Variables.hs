module SimplyTyped.Variables (getFreshName, getFreeVars, pickFreshName) where
import SimplyTyped.Definitions (CoreTerm(..))

getFreshName :: String -> CoreTerm -> String
getFreshName defaultName = pickFreshName defaultName . getAllVars

getAllVars :: CoreTerm -> [String]
getAllVars = getVars False

getFreeVars :: CoreTerm -> [String]
getFreeVars = getVars True

getVars :: Bool -> CoreTerm -> [String]
getVars excludeBounded = g []
  where
    g :: [String] -> CoreTerm -> [String]
    g boundVars (CoVar s)
      | excludeBounded && s `elem` boundVars = []
      | otherwise = [s]
    g boundVars (CoApp t1 t2) = g boundVars t1 ++ g boundVars t2
    g boundVars (CoAbs s _ t) = g (s : boundVars) t
    g _ CoConstTrue = []
    g _ CoConstFalse = []
    g _ CoConstZero = []
    g _ CoConstUnit = []
    g boundVars (CoIfThenElse t1 t2 t3) =
      g boundVars t1 ++ g boundVars t2 ++ g boundVars t3
    g boundVars (CoSucc t) = g boundVars t
    g boundVars (CoPred t) = g boundVars t
    g boundVars (CoIsZero t) = g boundVars t

pickFreshName :: String -> [String] -> String
pickFreshName s l
  | s `notElem` l = s
pickFreshName s l = g 1 s l
  where
    g :: Int -> String -> [String] -> String
    g n name existingNames
      | name ++ show n `elem` existingNames = g (n + 1) name existingNames
      | otherwise = name ++ show n
