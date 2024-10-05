{-# OPTIONS_GHC -w #-}
module SimplyTyped.Parser where

import SimplyTyped.Lexer
import SimplyTyped.Definitions
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn t4 t5 t6
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,116) ([16320,1029,16320,1029,49152,512,0,0,0,0,0,0,0,0,16320,1029,16320,1029,16320,1029,64,128,16320,1031,16320,1029,64,0,0,0,49152,576,0,16384,16320,1029,16320,1029,0,256,0,2048,49152,520,49152,8706,0,2,0,0,0,32,0,32,32768,512,32768,512,32768,512,0,256,0,256,0,0,0,0,16320,1029,16320,1029,16320,1029,0,0,0,0,0,0,0,0,49152,4608,49152,528,49152,8704,0,0,16384,0,16384,0,16320,1029,16320,1029,16320,1029,16320,1029,32768,0,32768,512,32768,512,32768,512,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseTerms","Terms","Term","TupleElems","var","true","false","'0'","succ","pred","iszero","lambda","'.'","'$'","'('","')'","if","then","else","':'","';'","'_'","type","as","let","'='","'in'","','","number","%eof"]
        bit_start = st Prelude.* 32
        bit_end = (st Prelude.+ 1) Prelude.* 32
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..31]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (7) = happyShift action_3
action_0 (8) = happyShift action_4
action_0 (9) = happyShift action_5
action_0 (10) = happyShift action_6
action_0 (11) = happyShift action_7
action_0 (12) = happyShift action_8
action_0 (13) = happyShift action_9
action_0 (14) = happyShift action_10
action_0 (17) = happyShift action_11
action_0 (19) = happyShift action_12
action_0 (27) = happyShift action_13
action_0 (4) = happyGoto action_14
action_0 (5) = happyGoto action_15
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (7) = happyShift action_3
action_1 (8) = happyShift action_4
action_1 (9) = happyShift action_5
action_1 (10) = happyShift action_6
action_1 (11) = happyShift action_7
action_1 (12) = happyShift action_8
action_1 (13) = happyShift action_9
action_1 (14) = happyShift action_10
action_1 (17) = happyShift action_11
action_1 (19) = happyShift action_12
action_1 (27) = happyShift action_13
action_1 (5) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (15) = happyShift action_16
action_2 (16) = happyShift action_17
action_2 (26) = happyShift action_19
action_2 _ = happyFail (happyExpListPerState 2)

action_3 _ = happyReduce_3

action_4 _ = happyReduce_8

action_5 _ = happyReduce_9

action_6 _ = happyReduce_11

action_7 (7) = happyShift action_3
action_7 (8) = happyShift action_4
action_7 (9) = happyShift action_5
action_7 (10) = happyShift action_6
action_7 (11) = happyShift action_7
action_7 (12) = happyShift action_8
action_7 (13) = happyShift action_9
action_7 (14) = happyShift action_10
action_7 (17) = happyShift action_11
action_7 (19) = happyShift action_12
action_7 (27) = happyShift action_13
action_7 (5) = happyGoto action_29
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (7) = happyShift action_3
action_8 (8) = happyShift action_4
action_8 (9) = happyShift action_5
action_8 (10) = happyShift action_6
action_8 (11) = happyShift action_7
action_8 (12) = happyShift action_8
action_8 (13) = happyShift action_9
action_8 (14) = happyShift action_10
action_8 (17) = happyShift action_11
action_8 (19) = happyShift action_12
action_8 (27) = happyShift action_13
action_8 (5) = happyGoto action_28
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (7) = happyShift action_3
action_9 (8) = happyShift action_4
action_9 (9) = happyShift action_5
action_9 (10) = happyShift action_6
action_9 (11) = happyShift action_7
action_9 (12) = happyShift action_8
action_9 (13) = happyShift action_9
action_9 (14) = happyShift action_10
action_9 (17) = happyShift action_11
action_9 (19) = happyShift action_12
action_9 (27) = happyShift action_13
action_9 (5) = happyGoto action_27
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (7) = happyShift action_25
action_10 (24) = happyShift action_26
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (7) = happyShift action_3
action_11 (8) = happyShift action_4
action_11 (9) = happyShift action_5
action_11 (10) = happyShift action_6
action_11 (11) = happyShift action_7
action_11 (12) = happyShift action_8
action_11 (13) = happyShift action_9
action_11 (14) = happyShift action_10
action_11 (17) = happyShift action_11
action_11 (18) = happyShift action_24
action_11 (19) = happyShift action_12
action_11 (27) = happyShift action_13
action_11 (5) = happyGoto action_22
action_11 (6) = happyGoto action_23
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (7) = happyShift action_3
action_12 (8) = happyShift action_4
action_12 (9) = happyShift action_5
action_12 (10) = happyShift action_6
action_12 (11) = happyShift action_7
action_12 (12) = happyShift action_8
action_12 (13) = happyShift action_9
action_12 (14) = happyShift action_10
action_12 (17) = happyShift action_11
action_12 (19) = happyShift action_12
action_12 (27) = happyShift action_13
action_12 (5) = happyGoto action_21
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (7) = happyShift action_20
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (32) = happyAccept
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (15) = happyShift action_16
action_15 (16) = happyShift action_17
action_15 (23) = happyShift action_18
action_15 (26) = happyShift action_19
action_15 _ = happyReduce_1

action_16 (31) = happyShift action_40
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (7) = happyShift action_3
action_17 (8) = happyShift action_4
action_17 (9) = happyShift action_5
action_17 (10) = happyShift action_6
action_17 (11) = happyShift action_7
action_17 (12) = happyShift action_8
action_17 (13) = happyShift action_9
action_17 (14) = happyShift action_10
action_17 (17) = happyShift action_11
action_17 (19) = happyShift action_12
action_17 (27) = happyShift action_13
action_17 (5) = happyGoto action_39
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (7) = happyShift action_3
action_18 (8) = happyShift action_4
action_18 (9) = happyShift action_5
action_18 (10) = happyShift action_6
action_18 (11) = happyShift action_7
action_18 (12) = happyShift action_8
action_18 (13) = happyShift action_9
action_18 (14) = happyShift action_10
action_18 (17) = happyShift action_11
action_18 (19) = happyShift action_12
action_18 (27) = happyShift action_13
action_18 (4) = happyGoto action_38
action_18 (5) = happyGoto action_15
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (25) = happyShift action_37
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (28) = happyShift action_36
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (15) = happyShift action_16
action_21 (16) = happyShift action_17
action_21 (20) = happyShift action_35
action_21 (26) = happyShift action_19
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (15) = happyShift action_16
action_22 (16) = happyShift action_17
action_22 (18) = happyShift action_33
action_22 (26) = happyShift action_19
action_22 (30) = happyShift action_34
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (18) = happyShift action_32
action_23 _ = happyFail (happyExpListPerState 23)

action_24 _ = happyReduce_15

action_25 (22) = happyShift action_31
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (22) = happyShift action_30
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (16) = happyShift action_17
action_27 (26) = happyShift action_19
action_27 _ = happyReduce_14

action_28 (16) = happyShift action_17
action_28 (26) = happyShift action_19
action_28 _ = happyReduce_13

action_29 (16) = happyShift action_17
action_29 (26) = happyShift action_19
action_29 _ = happyReduce_12

action_30 (25) = happyShift action_46
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (25) = happyShift action_45
action_31 _ = happyFail (happyExpListPerState 31)

action_32 _ = happyReduce_18

action_33 _ = happyReduce_7

action_34 (7) = happyShift action_3
action_34 (8) = happyShift action_4
action_34 (9) = happyShift action_5
action_34 (10) = happyShift action_6
action_34 (11) = happyShift action_7
action_34 (12) = happyShift action_8
action_34 (13) = happyShift action_9
action_34 (14) = happyShift action_10
action_34 (17) = happyShift action_11
action_34 (19) = happyShift action_12
action_34 (27) = happyShift action_13
action_34 (5) = happyGoto action_43
action_34 (6) = happyGoto action_44
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (7) = happyShift action_3
action_35 (8) = happyShift action_4
action_35 (9) = happyShift action_5
action_35 (10) = happyShift action_6
action_35 (11) = happyShift action_7
action_35 (12) = happyShift action_8
action_35 (13) = happyShift action_9
action_35 (14) = happyShift action_10
action_35 (17) = happyShift action_11
action_35 (19) = happyShift action_12
action_35 (27) = happyShift action_13
action_35 (5) = happyGoto action_42
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (7) = happyShift action_3
action_36 (8) = happyShift action_4
action_36 (9) = happyShift action_5
action_36 (10) = happyShift action_6
action_36 (11) = happyShift action_7
action_36 (12) = happyShift action_8
action_36 (13) = happyShift action_9
action_36 (14) = happyShift action_10
action_36 (17) = happyShift action_11
action_36 (19) = happyShift action_12
action_36 (27) = happyShift action_13
action_36 (5) = happyGoto action_41
action_36 _ = happyFail (happyExpListPerState 36)

action_37 _ = happyReduce_16

action_38 _ = happyReduce_2

action_39 _ = happyReduce_6

action_40 _ = happyReduce_19

action_41 (15) = happyShift action_16
action_41 (16) = happyShift action_17
action_41 (26) = happyShift action_19
action_41 (29) = happyShift action_50
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (15) = happyShift action_16
action_42 (16) = happyShift action_17
action_42 (21) = happyShift action_49
action_42 (26) = happyShift action_19
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (15) = happyShift action_16
action_43 (16) = happyShift action_17
action_43 (26) = happyShift action_19
action_43 (30) = happyShift action_34
action_43 _ = happyReduce_20

action_44 _ = happyReduce_21

action_45 (15) = happyShift action_48
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (15) = happyShift action_47
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (7) = happyShift action_3
action_47 (8) = happyShift action_4
action_47 (9) = happyShift action_5
action_47 (10) = happyShift action_6
action_47 (11) = happyShift action_7
action_47 (12) = happyShift action_8
action_47 (13) = happyShift action_9
action_47 (14) = happyShift action_10
action_47 (17) = happyShift action_11
action_47 (19) = happyShift action_12
action_47 (27) = happyShift action_13
action_47 (5) = happyGoto action_54
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (7) = happyShift action_3
action_48 (8) = happyShift action_4
action_48 (9) = happyShift action_5
action_48 (10) = happyShift action_6
action_48 (11) = happyShift action_7
action_48 (12) = happyShift action_8
action_48 (13) = happyShift action_9
action_48 (14) = happyShift action_10
action_48 (17) = happyShift action_11
action_48 (19) = happyShift action_12
action_48 (27) = happyShift action_13
action_48 (5) = happyGoto action_53
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (7) = happyShift action_3
action_49 (8) = happyShift action_4
action_49 (9) = happyShift action_5
action_49 (10) = happyShift action_6
action_49 (11) = happyShift action_7
action_49 (12) = happyShift action_8
action_49 (13) = happyShift action_9
action_49 (14) = happyShift action_10
action_49 (17) = happyShift action_11
action_49 (19) = happyShift action_12
action_49 (27) = happyShift action_13
action_49 (5) = happyGoto action_52
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (7) = happyShift action_3
action_50 (8) = happyShift action_4
action_50 (9) = happyShift action_5
action_50 (10) = happyShift action_6
action_50 (11) = happyShift action_7
action_50 (12) = happyShift action_8
action_50 (13) = happyShift action_9
action_50 (14) = happyShift action_10
action_50 (17) = happyShift action_11
action_50 (19) = happyShift action_12
action_50 (27) = happyShift action_13
action_50 (5) = happyGoto action_51
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (16) = happyShift action_17
action_51 _ = happyReduce_17

action_52 (16) = happyShift action_17
action_52 (26) = happyShift action_19
action_52 _ = happyReduce_10

action_53 (15) = happyFail []
action_53 (16) = happyShift action_17
action_53 (26) = happyShift action_19
action_53 _ = happyReduce_4

action_54 (15) = happyFail []
action_54 (16) = happyShift action_17
action_54 (26) = happyShift action_19
action_54 _ = happyReduce_5

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

happyReduce_18 = happySpecReduce_3  5 happyReduction_18
happyReduction_18 _
	(HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (Tuple happy_var_2
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  5 happyReduction_19
happyReduction_19 (HappyTerminal (TOK_NUMBER happy_var_3))
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Projection happy_var_1 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  6 happyReduction_20
happyReduction_20 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn6
		 ([happy_var_1, happy_var_3]
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  6 happyReduction_21
happyReduction_21 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1 : happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 32 32 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TOK_VAR happy_dollar_dollar -> cont 7;
	TOK_TRUE -> cont 8;
	TOK_FALSE -> cont 9;
	TOK_ZERO -> cont 10;
	TOK_SUCC -> cont 11;
	TOK_PRED -> cont 12;
	TOK_ISZERO -> cont 13;
	TOK_LAMBDA -> cont 14;
	TOK_DOT -> cont 15;
	TOK_DOLLAR -> cont 16;
	TOK_LEFT_PAREN -> cont 17;
	TOK_RIGHT_PAREN -> cont 18;
	TOK_IF -> cont 19;
	TOK_THEN -> cont 20;
	TOK_ELSE -> cont 21;
	TOK_COLON -> cont 22;
	TOK_SEMICOLON -> cont 23;
	TOK_WILDCARD -> cont 24;
	TOK_TYPE happy_dollar_dollar -> cont 25;
	TOK_AS -> cont 26;
	TOK_LET -> cont 27;
	TOK_EQUAL -> cont 28;
	TOK_IN -> cont 29;
	TOK_COMMA -> cont 30;
	TOK_NUMBER happy_dollar_dollar -> cont 31;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 32 tk tks = happyError' (tks, explist)
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
