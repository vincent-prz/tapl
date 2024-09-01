{-# OPTIONS_GHC -w #-}
module SimplyTyped.Parser where

import SimplyTyped.Lexer
import SimplyTyped.Definitions
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn t4 t5
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,93) ([40928,514,21500,64,256,4,0,0,0,0,0,0,0,5375,57360,671,64514,16467,128,256,53232,257,10750,16416,0,0,0,0,2306,40928,514,21500,64,0,2,0,2,4164,32768,514,0,0,32768,0,4096,0,8200,0,1025,8192,128,0,8,0,1,0,65280,4116,40928,514,0,0,0,0,0,0,36868,32768,528,2048,0,256,57344,671,64514,16467,32640,2058,20464,257,1024,0,128,2,16400,0,2050,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseTerms","Terms","Term","var","true","false","'0'","succ","pred","iszero","lambda","'.'","'$'","'('","')'","if","then","else","':'","';'","'_'","type","as","let","'='","'in'","%eof"]
        bit_start = st Prelude.* 29
        bit_end = (st Prelude.+ 1) Prelude.* 29
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..28]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

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
action_0 (26) = happyShift action_13
action_0 (4) = happyGoto action_14
action_0 (5) = happyGoto action_15
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
action_1 (26) = happyShift action_13
action_1 (5) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (15) = happyShift action_16
action_2 (25) = happyShift action_18
action_2 _ = happyFail (happyExpListPerState 2)

action_3 _ = happyReduce_3

action_4 _ = happyReduce_8

action_5 _ = happyReduce_9

action_6 _ = happyReduce_11

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
action_7 (26) = happyShift action_13
action_7 (5) = happyGoto action_27
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
action_8 (26) = happyShift action_13
action_8 (5) = happyGoto action_26
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
action_9 (26) = happyShift action_13
action_9 (5) = happyGoto action_25
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (6) = happyShift action_23
action_10 (23) = happyShift action_24
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
action_11 (17) = happyShift action_22
action_11 (18) = happyShift action_12
action_11 (26) = happyShift action_13
action_11 (5) = happyGoto action_21
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
action_12 (26) = happyShift action_13
action_12 (5) = happyGoto action_20
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (6) = happyShift action_19
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (29) = happyAccept
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (15) = happyShift action_16
action_15 (22) = happyShift action_17
action_15 (25) = happyShift action_18
action_15 _ = happyReduce_1

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
action_16 (26) = happyShift action_13
action_16 (5) = happyGoto action_35
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (6) = happyShift action_3
action_17 (7) = happyShift action_4
action_17 (8) = happyShift action_5
action_17 (9) = happyShift action_6
action_17 (10) = happyShift action_7
action_17 (11) = happyShift action_8
action_17 (12) = happyShift action_9
action_17 (13) = happyShift action_10
action_17 (16) = happyShift action_11
action_17 (18) = happyShift action_12
action_17 (26) = happyShift action_13
action_17 (4) = happyGoto action_34
action_17 (5) = happyGoto action_15
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (24) = happyShift action_33
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (27) = happyShift action_32
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (15) = happyShift action_16
action_20 (19) = happyShift action_31
action_20 (25) = happyShift action_18
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (15) = happyShift action_16
action_21 (17) = happyShift action_30
action_21 (25) = happyShift action_18
action_21 _ = happyFail (happyExpListPerState 21)

action_22 _ = happyReduce_15

action_23 (21) = happyShift action_29
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (21) = happyShift action_28
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (15) = happyShift action_16
action_25 (25) = happyShift action_18
action_25 _ = happyReduce_14

action_26 (15) = happyShift action_16
action_26 (25) = happyShift action_18
action_26 _ = happyReduce_13

action_27 (15) = happyShift action_16
action_27 (25) = happyShift action_18
action_27 _ = happyReduce_12

action_28 (24) = happyShift action_39
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (24) = happyShift action_38
action_29 _ = happyFail (happyExpListPerState 29)

action_30 _ = happyReduce_7

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
action_31 (26) = happyShift action_13
action_31 (5) = happyGoto action_37
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
action_32 (26) = happyShift action_13
action_32 (5) = happyGoto action_36
action_32 _ = happyFail (happyExpListPerState 32)

action_33 _ = happyReduce_16

action_34 _ = happyReduce_2

action_35 _ = happyReduce_6

action_36 (15) = happyShift action_16
action_36 (25) = happyShift action_18
action_36 (28) = happyShift action_43
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (15) = happyShift action_16
action_37 (20) = happyShift action_42
action_37 (25) = happyShift action_18
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (14) = happyShift action_41
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (14) = happyShift action_40
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (6) = happyShift action_3
action_40 (7) = happyShift action_4
action_40 (8) = happyShift action_5
action_40 (9) = happyShift action_6
action_40 (10) = happyShift action_7
action_40 (11) = happyShift action_8
action_40 (12) = happyShift action_9
action_40 (13) = happyShift action_10
action_40 (16) = happyShift action_11
action_40 (18) = happyShift action_12
action_40 (26) = happyShift action_13
action_40 (5) = happyGoto action_47
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (6) = happyShift action_3
action_41 (7) = happyShift action_4
action_41 (8) = happyShift action_5
action_41 (9) = happyShift action_6
action_41 (10) = happyShift action_7
action_41 (11) = happyShift action_8
action_41 (12) = happyShift action_9
action_41 (13) = happyShift action_10
action_41 (16) = happyShift action_11
action_41 (18) = happyShift action_12
action_41 (26) = happyShift action_13
action_41 (5) = happyGoto action_46
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (6) = happyShift action_3
action_42 (7) = happyShift action_4
action_42 (8) = happyShift action_5
action_42 (9) = happyShift action_6
action_42 (10) = happyShift action_7
action_42 (11) = happyShift action_8
action_42 (12) = happyShift action_9
action_42 (13) = happyShift action_10
action_42 (16) = happyShift action_11
action_42 (18) = happyShift action_12
action_42 (26) = happyShift action_13
action_42 (5) = happyGoto action_45
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (6) = happyShift action_3
action_43 (7) = happyShift action_4
action_43 (8) = happyShift action_5
action_43 (9) = happyShift action_6
action_43 (10) = happyShift action_7
action_43 (11) = happyShift action_8
action_43 (12) = happyShift action_9
action_43 (13) = happyShift action_10
action_43 (16) = happyShift action_11
action_43 (18) = happyShift action_12
action_43 (26) = happyShift action_13
action_43 (5) = happyGoto action_44
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (15) = happyShift action_16
action_44 _ = happyReduce_17

action_45 (15) = happyShift action_16
action_45 (25) = happyShift action_18
action_45 _ = happyReduce_10

action_46 (15) = happyShift action_16
action_46 (25) = happyShift action_18
action_46 _ = happyReduce_4

action_47 (15) = happyShift action_16
action_47 (25) = happyShift action_18
action_47 _ = happyReduce_5

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
		 (Abs (Just happy_var_2) happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_5 = happyReduce 6 5 happyReduction_5
happyReduction_5 ((HappyAbsSyn5  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TOK_TYPE happy_var_4)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (Abs Nothing happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_6 = happySpecReduce_3  5 happyReduction_6
happyReduction_6 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (App happy_var_1 happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  5 happyReduction_7
happyReduction_7 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (happy_var_2
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  5 happyReduction_8
happyReduction_8 _
	 =  HappyAbsSyn5
		 (ConstTrue
	)

happyReduce_9 = happySpecReduce_1  5 happyReduction_9
happyReduction_9 _
	 =  HappyAbsSyn5
		 (ConstFalse
	)

happyReduce_10 = happyReduce 6 5 happyReduction_10
happyReduction_10 ((HappyAbsSyn5  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (IfThenElse happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_11 = happySpecReduce_1  5 happyReduction_11
happyReduction_11 _
	 =  HappyAbsSyn5
		 (ConstZero
	)

happyReduce_12 = happySpecReduce_2  5 happyReduction_12
happyReduction_12 (HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (Succ happy_var_2
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_2  5 happyReduction_13
happyReduction_13 (HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (Pred happy_var_2
	)
happyReduction_13 _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_2  5 happyReduction_14
happyReduction_14 (HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (IsZero happy_var_2
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_2  5 happyReduction_15
happyReduction_15 _
	_
	 =  HappyAbsSyn5
		 (ConstUnit
	)

happyReduce_16 = happySpecReduce_3  5 happyReduction_16
happyReduction_16 (HappyTerminal (TOK_TYPE happy_var_3))
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Ascription happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happyReduce 6 5 happyReduction_17
happyReduction_17 ((HappyAbsSyn5  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TOK_VAR happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (LetExpr happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	action 29 29 notHappyAtAll (HappyState action) sts stk []

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
	TOK_WILDCARD -> cont 23;
	TOK_TYPE happy_dollar_dollar -> cont 24;
	TOK_AS -> cont 25;
	TOK_LET -> cont 26;
	TOK_EQUAL -> cont 27;
	TOK_IN -> cont 28;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 29 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Either String a -> (a -> Either String b) -> Either String b
happyThen = ((>>=))
happyReturn :: () => a -> Either String a
happyReturn = (return)
happyThen1 m k tks = ((>>=)) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Either String a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> Either String a
happyError' = (\(tokens, _) -> parseError tokens)
parseTerms tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> Either String a
parseError _ = Left "Parse Error"

fullParser :: String -> Either String [Term]
fullParser s =
  case lexer s of
    Left err -> Left $ show err
    Right lexemes -> parseTerms lexemes
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
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
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
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





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









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
