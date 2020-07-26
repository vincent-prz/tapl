{-# OPTIONS_GHC -w #-}
module SimplyTyped.Parser where

import SimplyTyped.Lexer
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.9

data HappyAbsSyn t4 t5
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,69) ([40928,57346,671,16384,0,0,0,0,0,0,57344,671,40928,57346,671,32,57344,927,40928,2,0,16384,57376,671,40928,2,1088,16384,1,0,0,16,64,16384,0,64,0,64,0,40928,2,0,0,0,2112,8192,57344,671,40928,2,64,16384,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseTerms","Terms","Term","var","true","false","'0'","succ","pred","iszero","lambda","'.'","'$'","'('","')'","if","then","else","':'","';'","type","%eof"]
        bit_start = st * 24
        bit_end = (st + 1) * 24
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..23]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (6) = happyShift action_3
action_0 (7) = happyShift action_4
action_0 (8) = happyShift action_5
action_0 (9) = happyShift action_6
action_0 (10) = happyShift action_7
action_0 (11) = happyShift action_8
action_0 (12) = happyShift action_9
action_0 (13) = happyShift action_10
action_0 (16) = happyShift action_11
action_0 (18) = happyShift action_12
action_0 (4) = happyGoto action_13
action_0 (5) = happyGoto action_14
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (6) = happyShift action_3
action_1 (7) = happyShift action_4
action_1 (8) = happyShift action_5
action_1 (9) = happyShift action_6
action_1 (10) = happyShift action_7
action_1 (11) = happyShift action_8
action_1 (12) = happyShift action_9
action_1 (13) = happyShift action_10
action_1 (16) = happyShift action_11
action_1 (18) = happyShift action_12
action_1 (5) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (15) = happyShift action_15
action_2 _ = happyFail (happyExpListPerState 2)

action_3 _ = happyReduce_3

action_4 _ = happyReduce_7

action_5 _ = happyReduce_8

action_6 _ = happyReduce_10

action_7 (6) = happyShift action_3
action_7 (7) = happyShift action_4
action_7 (8) = happyShift action_5
action_7 (9) = happyShift action_6
action_7 (10) = happyShift action_7
action_7 (11) = happyShift action_8
action_7 (12) = happyShift action_9
action_7 (13) = happyShift action_10
action_7 (16) = happyShift action_11
action_7 (18) = happyShift action_12
action_7 (5) = happyGoto action_23
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (6) = happyShift action_3
action_8 (7) = happyShift action_4
action_8 (8) = happyShift action_5
action_8 (9) = happyShift action_6
action_8 (10) = happyShift action_7
action_8 (11) = happyShift action_8
action_8 (12) = happyShift action_9
action_8 (13) = happyShift action_10
action_8 (16) = happyShift action_11
action_8 (18) = happyShift action_12
action_8 (5) = happyGoto action_22
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (6) = happyShift action_3
action_9 (7) = happyShift action_4
action_9 (8) = happyShift action_5
action_9 (9) = happyShift action_6
action_9 (10) = happyShift action_7
action_9 (11) = happyShift action_8
action_9 (12) = happyShift action_9
action_9 (13) = happyShift action_10
action_9 (16) = happyShift action_11
action_9 (18) = happyShift action_12
action_9 (5) = happyGoto action_21
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (6) = happyShift action_20
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (6) = happyShift action_3
action_11 (7) = happyShift action_4
action_11 (8) = happyShift action_5
action_11 (9) = happyShift action_6
action_11 (10) = happyShift action_7
action_11 (11) = happyShift action_8
action_11 (12) = happyShift action_9
action_11 (13) = happyShift action_10
action_11 (16) = happyShift action_11
action_11 (17) = happyShift action_19
action_11 (18) = happyShift action_12
action_11 (5) = happyGoto action_18
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (6) = happyShift action_3
action_12 (7) = happyShift action_4
action_12 (8) = happyShift action_5
action_12 (9) = happyShift action_6
action_12 (10) = happyShift action_7
action_12 (11) = happyShift action_8
action_12 (12) = happyShift action_9
action_12 (13) = happyShift action_10
action_12 (16) = happyShift action_11
action_12 (18) = happyShift action_12
action_12 (5) = happyGoto action_17
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (24) = happyAccept
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (15) = happyShift action_15
action_14 (22) = happyShift action_16
action_14 _ = happyReduce_1

action_15 (6) = happyShift action_3
action_15 (7) = happyShift action_4
action_15 (8) = happyShift action_5
action_15 (9) = happyShift action_6
action_15 (10) = happyShift action_7
action_15 (11) = happyShift action_8
action_15 (12) = happyShift action_9
action_15 (13) = happyShift action_10
action_15 (16) = happyShift action_11
action_15 (18) = happyShift action_12
action_15 (5) = happyGoto action_28
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (6) = happyShift action_3
action_16 (7) = happyShift action_4
action_16 (8) = happyShift action_5
action_16 (9) = happyShift action_6
action_16 (10) = happyShift action_7
action_16 (11) = happyShift action_8
action_16 (12) = happyShift action_9
action_16 (13) = happyShift action_10
action_16 (16) = happyShift action_11
action_16 (18) = happyShift action_12
action_16 (4) = happyGoto action_27
action_16 (5) = happyGoto action_14
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (15) = happyShift action_15
action_17 (19) = happyShift action_26
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (15) = happyShift action_15
action_18 (17) = happyShift action_25
action_18 _ = happyFail (happyExpListPerState 18)

action_19 _ = happyReduce_14

action_20 (21) = happyShift action_24
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (15) = happyShift action_15
action_21 _ = happyReduce_13

action_22 (15) = happyShift action_15
action_22 _ = happyReduce_12

action_23 (15) = happyShift action_15
action_23 _ = happyReduce_11

action_24 (23) = happyShift action_30
action_24 _ = happyFail (happyExpListPerState 24)

action_25 _ = happyReduce_6

action_26 (6) = happyShift action_3
action_26 (7) = happyShift action_4
action_26 (8) = happyShift action_5
action_26 (9) = happyShift action_6
action_26 (10) = happyShift action_7
action_26 (11) = happyShift action_8
action_26 (12) = happyShift action_9
action_26 (13) = happyShift action_10
action_26 (16) = happyShift action_11
action_26 (18) = happyShift action_12
action_26 (5) = happyGoto action_29
action_26 _ = happyFail (happyExpListPerState 26)

action_27 _ = happyReduce_2

action_28 _ = happyReduce_5

action_29 (15) = happyShift action_15
action_29 (20) = happyShift action_32
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (14) = happyShift action_31
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (6) = happyShift action_3
action_31 (7) = happyShift action_4
action_31 (8) = happyShift action_5
action_31 (9) = happyShift action_6
action_31 (10) = happyShift action_7
action_31 (11) = happyShift action_8
action_31 (12) = happyShift action_9
action_31 (13) = happyShift action_10
action_31 (16) = happyShift action_11
action_31 (18) = happyShift action_12
action_31 (5) = happyGoto action_34
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (6) = happyShift action_3
action_32 (7) = happyShift action_4
action_32 (8) = happyShift action_5
action_32 (9) = happyShift action_6
action_32 (10) = happyShift action_7
action_32 (11) = happyShift action_8
action_32 (12) = happyShift action_9
action_32 (13) = happyShift action_10
action_32 (16) = happyShift action_11
action_32 (18) = happyShift action_12
action_32 (5) = happyGoto action_33
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (15) = happyShift action_15
action_33 _ = happyReduce_9

action_34 (15) = happyShift action_15
action_34 _ = happyReduce_4

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 ([happy_var_1]
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_3  4 happyReduction_2
happyReduction_2 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1 : happy_var_3
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  5 happyReduction_3
happyReduction_3 (HappyTerminal (TOK_VAR happy_var_1))
	 =  HappyAbsSyn5
		 (Var happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happyReduce 6 5 happyReduction_4
happyReduction_4 ((HappyAbsSyn5  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TOK_TYPE happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TOK_VAR happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (Abs happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_5 = happySpecReduce_3  5 happyReduction_5
happyReduction_5 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (App happy_var_1 happy_var_3
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  5 happyReduction_6
happyReduction_6 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (happy_var_2
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  5 happyReduction_7
happyReduction_7 _
	 =  HappyAbsSyn5
		 (ConstTrue
	)

happyReduce_8 = happySpecReduce_1  5 happyReduction_8
happyReduction_8 _
	 =  HappyAbsSyn5
		 (ConstFalse
	)

happyReduce_9 = happyReduce 6 5 happyReduction_9
happyReduction_9 ((HappyAbsSyn5  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (IfThenElse happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_10 = happySpecReduce_1  5 happyReduction_10
happyReduction_10 _
	 =  HappyAbsSyn5
		 (ConstZero
	)

happyReduce_11 = happySpecReduce_2  5 happyReduction_11
happyReduction_11 (HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (Succ happy_var_2
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_2  5 happyReduction_12
happyReduction_12 (HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (Pred happy_var_2
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_2  5 happyReduction_13
happyReduction_13 (HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (IsZero happy_var_2
	)
happyReduction_13 _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_2  5 happyReduction_14
happyReduction_14 _
	_
	 =  HappyAbsSyn5
		 (ConstUnit
	)

happyNewToken action sts stk [] =
	action 24 24 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TOK_VAR happy_dollar_dollar -> cont 6;
	TOK_TRUE -> cont 7;
	TOK_FALSE -> cont 8;
	TOK_ZERO -> cont 9;
	TOK_SUCC -> cont 10;
	TOK_PRED -> cont 11;
	TOK_ISZERO -> cont 12;
	TOK_LAMBDA -> cont 13;
	TOK_DOT -> cont 14;
	TOK_DOLLAR -> cont 15;
	TOK_LEFT_PAREN -> cont 16;
	TOK_RIGHT_PAREN -> cont 17;
	TOK_IF -> cont 18;
	TOK_THEN -> cont 19;
	TOK_ELSE -> cont 20;
	TOK_COLON -> cont 21;
	TOK_SEMICOLON -> cont 22;
	TOK_TYPE happy_dollar_dollar -> cont 23;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 24 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Either String a -> (a -> Either String b) -> Either String b
happyThen = ((>>=))
happyReturn :: () => a -> Either String a
happyReturn = (return)
happyThen1 m k tks = ((>>=)) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Either String a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [String]) -> Either String a
happyError' = (\(tokens, _) -> parseError tokens)
parseTerms tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> Either String a
parseError _ = Left "Parse Error"

data Term
  = Var String
  | Abs String Type Term
  | App Term Term
  | ConstTrue
  | ConstFalse
  | IfThenElse Term Term Term
  | ConstZero
  | Succ Term
  | Pred Term
  | IsZero Term
  | ConstUnit
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

fullParser :: String -> Either String [Term]
fullParser s =
  case lexer s of
    Left err -> Left $ show err
    Right lexemes -> parseTerms lexemes
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}







# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4











































{-# LINE 7 "<command-line>" #-}
{-# LINE 1 "/home/vincent/.stack/programs/x86_64-linux/ghc-tinfo6-8.4.4/lib/ghc-8.4.4/include/ghcversion.h" #-}















{-# LINE 7 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc13615_0/ghc_2.h" #-}




















































































































































































{-# LINE 7 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 









{-# LINE 43 "templates/GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Int Happy_IntList







{-# LINE 65 "templates/GenericTemplate.hs" #-}

{-# LINE 75 "templates/GenericTemplate.hs" #-}

{-# LINE 84 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 137 "templates/GenericTemplate.hs" #-}

{-# LINE 147 "templates/GenericTemplate.hs" #-}
indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 267 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 333 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
