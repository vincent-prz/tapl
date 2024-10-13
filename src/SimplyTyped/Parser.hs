{-# OPTIONS_GHC -w #-}
module SimplyTyped.Parser where

import SimplyTyped.Lexer
import SimplyTyped.Definitions
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn t4 t5 t6 t7 t8 t9
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,138) ([65024,33096,32768,21055,32,0,1027,0,0,0,0,0,0,0,0,0,0,9208,517,65024,33096,32768,21055,32,32,512,63488,1827,2,18686,129,128,0,0,0,0,49152,320,0,0,32784,21055,32,36832,2068,0,284,0,0,256,0,4236,0,2816,68,0,2,0,0,0,0,2,0,128,0,128,1,8192,64,0,4104,0,1136,0,7168,1,0,0,0,0,0,36832,2068,63488,1315,2,18686,129,0,0,0,0,128,0,0,0,0,0,0,0,28672,4,0,0,0,0,0,0,0,0,0,192,0,2,0,71,0,3072,144,0,1091,0,192,17,0,0,0,4,2,256,128,9208,517,65024,33096,32768,21055,32,36832,2068,0,0,32,0,0,49152,17,0,0,192,0,0,0,32,0,2048,16,0,1027,0,192,1,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseTerms","Terms","Term","BaseType","Type","TupleType","TupleElems","var","true","false","'0'","succ","pred","iszero","'Bool'","'Nat'","'Unit'","lambda","'.'","'$'","'('","')'","if","then","else","':'","';'","'_'","as","let","'='","'in'","','","'->'","number","%eof"]
        bit_start = st Prelude.* 38
        bit_end = (st Prelude.+ 1) Prelude.* 38
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..37]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (10) = happyShift action_3
action_0 (11) = happyShift action_4
action_0 (12) = happyShift action_5
action_0 (13) = happyShift action_6
action_0 (14) = happyShift action_7
action_0 (15) = happyShift action_8
action_0 (16) = happyShift action_9
action_0 (20) = happyShift action_10
action_0 (23) = happyShift action_11
action_0 (25) = happyShift action_12
action_0 (32) = happyShift action_13
action_0 (4) = happyGoto action_14
action_0 (5) = happyGoto action_15
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (10) = happyShift action_3
action_1 (11) = happyShift action_4
action_1 (12) = happyShift action_5
action_1 (13) = happyShift action_6
action_1 (14) = happyShift action_7
action_1 (15) = happyShift action_8
action_1 (16) = happyShift action_9
action_1 (20) = happyShift action_10
action_1 (23) = happyShift action_11
action_1 (25) = happyShift action_12
action_1 (32) = happyShift action_13
action_1 (5) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (21) = happyShift action_16
action_2 (22) = happyShift action_17
action_2 (31) = happyShift action_19
action_2 _ = happyFail (happyExpListPerState 2)

action_3 _ = happyReduce_3

action_4 _ = happyReduce_8

action_5 _ = happyReduce_9

action_6 _ = happyReduce_11

action_7 (10) = happyShift action_3
action_7 (11) = happyShift action_4
action_7 (12) = happyShift action_5
action_7 (13) = happyShift action_6
action_7 (14) = happyShift action_7
action_7 (15) = happyShift action_8
action_7 (16) = happyShift action_9
action_7 (20) = happyShift action_10
action_7 (23) = happyShift action_11
action_7 (25) = happyShift action_12
action_7 (32) = happyShift action_13
action_7 (5) = happyGoto action_29
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (10) = happyShift action_3
action_8 (11) = happyShift action_4
action_8 (12) = happyShift action_5
action_8 (13) = happyShift action_6
action_8 (14) = happyShift action_7
action_8 (15) = happyShift action_8
action_8 (16) = happyShift action_9
action_8 (20) = happyShift action_10
action_8 (23) = happyShift action_11
action_8 (25) = happyShift action_12
action_8 (32) = happyShift action_13
action_8 (5) = happyGoto action_28
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (10) = happyShift action_3
action_9 (11) = happyShift action_4
action_9 (12) = happyShift action_5
action_9 (13) = happyShift action_6
action_9 (14) = happyShift action_7
action_9 (15) = happyShift action_8
action_9 (16) = happyShift action_9
action_9 (20) = happyShift action_10
action_9 (23) = happyShift action_11
action_9 (25) = happyShift action_12
action_9 (32) = happyShift action_13
action_9 (5) = happyGoto action_27
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (10) = happyShift action_25
action_10 (30) = happyShift action_26
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (10) = happyShift action_3
action_11 (11) = happyShift action_4
action_11 (12) = happyShift action_5
action_11 (13) = happyShift action_6
action_11 (14) = happyShift action_7
action_11 (15) = happyShift action_8
action_11 (16) = happyShift action_9
action_11 (20) = happyShift action_10
action_11 (23) = happyShift action_11
action_11 (24) = happyShift action_24
action_11 (25) = happyShift action_12
action_11 (32) = happyShift action_13
action_11 (5) = happyGoto action_22
action_11 (9) = happyGoto action_23
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (10) = happyShift action_3
action_12 (11) = happyShift action_4
action_12 (12) = happyShift action_5
action_12 (13) = happyShift action_6
action_12 (14) = happyShift action_7
action_12 (15) = happyShift action_8
action_12 (16) = happyShift action_9
action_12 (20) = happyShift action_10
action_12 (23) = happyShift action_11
action_12 (25) = happyShift action_12
action_12 (32) = happyShift action_13
action_12 (5) = happyGoto action_21
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (10) = happyShift action_20
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (38) = happyAccept
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (21) = happyShift action_16
action_15 (22) = happyShift action_17
action_15 (29) = happyShift action_18
action_15 (31) = happyShift action_19
action_15 _ = happyReduce_1

action_16 (37) = happyShift action_45
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (10) = happyShift action_3
action_17 (11) = happyShift action_4
action_17 (12) = happyShift action_5
action_17 (13) = happyShift action_6
action_17 (14) = happyShift action_7
action_17 (15) = happyShift action_8
action_17 (16) = happyShift action_9
action_17 (20) = happyShift action_10
action_17 (23) = happyShift action_11
action_17 (25) = happyShift action_12
action_17 (32) = happyShift action_13
action_17 (5) = happyGoto action_44
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (10) = happyShift action_3
action_18 (11) = happyShift action_4
action_18 (12) = happyShift action_5
action_18 (13) = happyShift action_6
action_18 (14) = happyShift action_7
action_18 (15) = happyShift action_8
action_18 (16) = happyShift action_9
action_18 (20) = happyShift action_10
action_18 (23) = happyShift action_11
action_18 (25) = happyShift action_12
action_18 (32) = happyShift action_13
action_18 (4) = happyGoto action_43
action_18 (5) = happyGoto action_15
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (17) = happyShift action_39
action_19 (18) = happyShift action_40
action_19 (19) = happyShift action_41
action_19 (23) = happyShift action_42
action_19 (6) = happyGoto action_37
action_19 (7) = happyGoto action_38
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (33) = happyShift action_36
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (21) = happyShift action_16
action_21 (22) = happyShift action_17
action_21 (26) = happyShift action_35
action_21 (31) = happyShift action_19
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (21) = happyShift action_16
action_22 (22) = happyShift action_17
action_22 (24) = happyShift action_33
action_22 (31) = happyShift action_19
action_22 (35) = happyShift action_34
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (24) = happyShift action_32
action_23 _ = happyFail (happyExpListPerState 23)

action_24 _ = happyReduce_15

action_25 (28) = happyShift action_31
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (28) = happyShift action_30
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (22) = happyShift action_17
action_27 (31) = happyShift action_19
action_27 _ = happyReduce_14

action_28 (22) = happyShift action_17
action_28 (31) = happyShift action_19
action_28 _ = happyReduce_13

action_29 (22) = happyShift action_17
action_29 (31) = happyShift action_19
action_29 _ = happyReduce_12

action_30 (17) = happyShift action_39
action_30 (18) = happyShift action_40
action_30 (19) = happyShift action_41
action_30 (23) = happyShift action_42
action_30 (6) = happyGoto action_37
action_30 (7) = happyGoto action_54
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (17) = happyShift action_39
action_31 (18) = happyShift action_40
action_31 (19) = happyShift action_41
action_31 (23) = happyShift action_42
action_31 (6) = happyGoto action_37
action_31 (7) = happyGoto action_53
action_31 _ = happyFail (happyExpListPerState 31)

action_32 _ = happyReduce_18

action_33 _ = happyReduce_7

action_34 (10) = happyShift action_3
action_34 (11) = happyShift action_4
action_34 (12) = happyShift action_5
action_34 (13) = happyShift action_6
action_34 (14) = happyShift action_7
action_34 (15) = happyShift action_8
action_34 (16) = happyShift action_9
action_34 (20) = happyShift action_10
action_34 (23) = happyShift action_11
action_34 (25) = happyShift action_12
action_34 (32) = happyShift action_13
action_34 (5) = happyGoto action_51
action_34 (9) = happyGoto action_52
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (10) = happyShift action_3
action_35 (11) = happyShift action_4
action_35 (12) = happyShift action_5
action_35 (13) = happyShift action_6
action_35 (14) = happyShift action_7
action_35 (15) = happyShift action_8
action_35 (16) = happyShift action_9
action_35 (20) = happyShift action_10
action_35 (23) = happyShift action_11
action_35 (25) = happyShift action_12
action_35 (32) = happyShift action_13
action_35 (5) = happyGoto action_50
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (10) = happyShift action_3
action_36 (11) = happyShift action_4
action_36 (12) = happyShift action_5
action_36 (13) = happyShift action_6
action_36 (14) = happyShift action_7
action_36 (15) = happyShift action_8
action_36 (16) = happyShift action_9
action_36 (20) = happyShift action_10
action_36 (23) = happyShift action_11
action_36 (25) = happyShift action_12
action_36 (32) = happyShift action_13
action_36 (5) = happyGoto action_49
action_36 _ = happyFail (happyExpListPerState 36)

action_37 _ = happyReduce_23

action_38 (36) = happyShift action_48
action_38 _ = happyReduce_16

action_39 _ = happyReduce_20

action_40 _ = happyReduce_21

action_41 _ = happyReduce_22

action_42 (17) = happyShift action_39
action_42 (18) = happyShift action_40
action_42 (19) = happyShift action_41
action_42 (23) = happyShift action_42
action_42 (6) = happyGoto action_37
action_42 (7) = happyGoto action_46
action_42 (8) = happyGoto action_47
action_42 _ = happyFail (happyExpListPerState 42)

action_43 _ = happyReduce_2

action_44 _ = happyReduce_6

action_45 _ = happyReduce_19

action_46 (35) = happyShift action_61
action_46 (36) = happyShift action_48
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (24) = happyShift action_60
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (17) = happyShift action_39
action_48 (18) = happyShift action_40
action_48 (19) = happyShift action_41
action_48 (23) = happyShift action_42
action_48 (6) = happyGoto action_37
action_48 (7) = happyGoto action_59
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (21) = happyShift action_16
action_49 (22) = happyShift action_17
action_49 (31) = happyShift action_19
action_49 (34) = happyShift action_58
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (21) = happyShift action_16
action_50 (22) = happyShift action_17
action_50 (27) = happyShift action_57
action_50 (31) = happyShift action_19
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (21) = happyShift action_16
action_51 (22) = happyShift action_17
action_51 (31) = happyShift action_19
action_51 (35) = happyShift action_34
action_51 _ = happyReduce_28

action_52 _ = happyReduce_29

action_53 (21) = happyShift action_56
action_53 (36) = happyShift action_48
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (21) = happyShift action_55
action_54 (36) = happyShift action_48
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (10) = happyShift action_3
action_55 (11) = happyShift action_4
action_55 (12) = happyShift action_5
action_55 (13) = happyShift action_6
action_55 (14) = happyShift action_7
action_55 (15) = happyShift action_8
action_55 (16) = happyShift action_9
action_55 (20) = happyShift action_10
action_55 (23) = happyShift action_11
action_55 (25) = happyShift action_12
action_55 (32) = happyShift action_13
action_55 (5) = happyGoto action_67
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (10) = happyShift action_3
action_56 (11) = happyShift action_4
action_56 (12) = happyShift action_5
action_56 (13) = happyShift action_6
action_56 (14) = happyShift action_7
action_56 (15) = happyShift action_8
action_56 (16) = happyShift action_9
action_56 (20) = happyShift action_10
action_56 (23) = happyShift action_11
action_56 (25) = happyShift action_12
action_56 (32) = happyShift action_13
action_56 (5) = happyGoto action_66
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (10) = happyShift action_3
action_57 (11) = happyShift action_4
action_57 (12) = happyShift action_5
action_57 (13) = happyShift action_6
action_57 (14) = happyShift action_7
action_57 (15) = happyShift action_8
action_57 (16) = happyShift action_9
action_57 (20) = happyShift action_10
action_57 (23) = happyShift action_11
action_57 (25) = happyShift action_12
action_57 (32) = happyShift action_13
action_57 (5) = happyGoto action_65
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (10) = happyShift action_3
action_58 (11) = happyShift action_4
action_58 (12) = happyShift action_5
action_58 (13) = happyShift action_6
action_58 (14) = happyShift action_7
action_58 (15) = happyShift action_8
action_58 (16) = happyShift action_9
action_58 (20) = happyShift action_10
action_58 (23) = happyShift action_11
action_58 (25) = happyShift action_12
action_58 (32) = happyShift action_13
action_58 (5) = happyGoto action_64
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (36) = happyShift action_48
action_59 _ = happyReduce_25

action_60 _ = happyReduce_24

action_61 (17) = happyShift action_39
action_61 (18) = happyShift action_40
action_61 (19) = happyShift action_41
action_61 (23) = happyShift action_42
action_61 (6) = happyGoto action_37
action_61 (7) = happyGoto action_62
action_61 (8) = happyGoto action_63
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (35) = happyShift action_61
action_62 (36) = happyShift action_48
action_62 _ = happyReduce_26

action_63 _ = happyReduce_27

action_64 (22) = happyShift action_17
action_64 _ = happyReduce_17

action_65 (22) = happyShift action_17
action_65 (31) = happyShift action_19
action_65 _ = happyReduce_10

action_66 (21) = happyShift action_16
action_66 (22) = happyShift action_17
action_66 (31) = happyShift action_19
action_66 _ = happyReduce_4

action_67 (21) = happyShift action_16
action_67 (22) = happyShift action_17
action_67 (31) = happyShift action_19
action_67 _ = happyReduce_5

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
	(HappyAbsSyn7  happy_var_4) `HappyStk`
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
	(HappyAbsSyn7  happy_var_4) `HappyStk`
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
happyReduction_16 (HappyAbsSyn7  happy_var_3)
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
	(HappyAbsSyn9  happy_var_2)
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

happyReduce_20 = happySpecReduce_1  6 happyReduction_20
happyReduction_20 _
	 =  HappyAbsSyn6
		 (TBool
	)

happyReduce_21 = happySpecReduce_1  6 happyReduction_21
happyReduction_21 _
	 =  HappyAbsSyn6
		 (TNat
	)

happyReduce_22 = happySpecReduce_1  6 happyReduction_22
happyReduction_22 _
	 =  HappyAbsSyn6
		 (TUnit
	)

happyReduce_23 = happySpecReduce_1  7 happyReduction_23
happyReduction_23 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  7 happyReduction_24
happyReduction_24 _
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (TTuple happy_var_2
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  7 happyReduction_25
happyReduction_25 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (Arrow happy_var_1 happy_var_3
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  8 happyReduction_26
happyReduction_26 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn8
		 ([happy_var_1, happy_var_3]
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  8 happyReduction_27
happyReduction_27 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1 : happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  9 happyReduction_28
happyReduction_28 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn9
		 ([happy_var_1, happy_var_3]
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  9 happyReduction_29
happyReduction_29 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1 : happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 38 38 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TOK_VAR happy_dollar_dollar -> cont 10;
	TOK_TRUE -> cont 11;
	TOK_FALSE -> cont 12;
	TOK_ZERO -> cont 13;
	TOK_SUCC -> cont 14;
	TOK_PRED -> cont 15;
	TOK_ISZERO -> cont 16;
	TOK_BOOL -> cont 17;
	TOK_NAT -> cont 18;
	TOK_UNIT -> cont 19;
	TOK_LAMBDA -> cont 20;
	TOK_DOT -> cont 21;
	TOK_DOLLAR -> cont 22;
	TOK_LEFT_PAREN -> cont 23;
	TOK_RIGHT_PAREN -> cont 24;
	TOK_IF -> cont 25;
	TOK_THEN -> cont 26;
	TOK_ELSE -> cont 27;
	TOK_COLON -> cont 28;
	TOK_SEMICOLON -> cont 29;
	TOK_WILDCARD -> cont 30;
	TOK_AS -> cont 31;
	TOK_LET -> cont 32;
	TOK_EQUAL -> cont 33;
	TOK_IN -> cont 34;
	TOK_COMMA -> cont 35;
	TOK_ARROW -> cont 36;
	TOK_NUMBER happy_dollar_dollar -> cont 37;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 38 tk tks = happyError' (tks, explist)
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
