{-# OPTIONS_GHC -w #-}
module InputParser where
import InputLexer
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20
	= HappyTerminal (InputToken)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18
	| HappyAbsSyn19 t19
	| HappyAbsSyn20 t20

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,114) ([0,36864,0,32768,0,0,36,0,0,0,128,0,8,0,0,0,0,18,0,128,0,0,0,512,0,32,0,32768,16,0,4,0,0,0,2,0,0,0,0,0,0,32,0,144,0,128,0,0,0,64,0,61440,1,0,0,32768,0,0,0,0,64,0,0,0,49152,7,0,0,0,2,0,16,0,0,16,0,66,8192,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,2048,0,36864,0,256,0,8192,0,0,0,0,0,0,2048,0,31744,0,0,0,0,0,0,0,0,1024,0,15872,0,512,0,0,0,0,4096,0,0,0,32,0,0,0,8192,0,0,0,0,256,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_inputParser","File","NodeSets","NodeSet","NodeHeader","Fields","Field","NodeEntries","INodeEntry","Literals","Literal","Labels","Label","RelationshipSets","RelationshipSet","RelationshipHeader","RelationshipEntries","IRelationshipEntry","\",\"","\":\"","\";\"","strVal","intVal","boolVal","nullVal","string","\":ID\"","type","\":LABEL\"","\":START_ID\"","\":END_ID\"","\":TYPE\"","%eof"]
        bit_start = st Prelude.* 35
        bit_end = (st Prelude.+ 1) Prelude.* 35
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..34]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (29) = happyShift action_5
action_0 (32) = happyShift action_11
action_0 (4) = happyGoto action_6
action_0 (5) = happyGoto action_7
action_0 (6) = happyGoto action_3
action_0 (7) = happyGoto action_4
action_0 (16) = happyGoto action_8
action_0 (17) = happyGoto action_9
action_0 (18) = happyGoto action_10
action_0 _ = happyReduce_4

action_1 (29) = happyShift action_5
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 (7) = happyGoto action_4
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (29) = happyShift action_5
action_2 (32) = happyShift action_11
action_2 (6) = happyGoto action_17
action_2 (7) = happyGoto action_4
action_2 (16) = happyGoto action_18
action_2 (17) = happyGoto action_9
action_2 (18) = happyGoto action_10
action_2 _ = happyFail (happyExpListPerState 2)

action_3 _ = happyReduce_6

action_4 (28) = happyShift action_22
action_4 (10) = happyGoto action_20
action_4 (11) = happyGoto action_21
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (21) = happyShift action_19
action_5 _ = happyReduce_10

action_6 (35) = happyAccept
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (29) = happyShift action_5
action_7 (32) = happyShift action_11
action_7 (6) = happyGoto action_17
action_7 (7) = happyGoto action_4
action_7 (16) = happyGoto action_18
action_7 (17) = happyGoto action_9
action_7 (18) = happyGoto action_10
action_7 _ = happyReduce_2

action_8 (32) = happyShift action_11
action_8 (17) = happyGoto action_16
action_8 (18) = happyGoto action_10
action_8 _ = happyReduce_3

action_9 _ = happyReduce_31

action_10 (28) = happyShift action_15
action_10 (19) = happyGoto action_13
action_10 (20) = happyGoto action_14
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (21) = happyShift action_12
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (28) = happyShift action_27
action_12 (33) = happyShift action_32
action_12 (8) = happyGoto action_31
action_12 (9) = happyGoto action_26
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (28) = happyShift action_15
action_13 (20) = happyGoto action_30
action_13 _ = happyReduce_32

action_14 _ = happyReduce_36

action_15 (21) = happyShift action_29
action_15 _ = happyFail (happyExpListPerState 15)

action_16 _ = happyReduce_30

action_17 _ = happyReduce_5

action_18 (32) = happyShift action_11
action_18 (17) = happyGoto action_16
action_18 (18) = happyGoto action_10
action_18 _ = happyReduce_1

action_19 (28) = happyShift action_27
action_19 (31) = happyShift action_28
action_19 (8) = happyGoto action_25
action_19 (9) = happyGoto action_26
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (28) = happyShift action_22
action_20 (11) = happyGoto action_24
action_20 _ = happyReduce_7

action_21 _ = happyReduce_16

action_22 (21) = happyShift action_23
action_22 _ = happyReduce_20

action_23 (24) = happyShift action_37
action_23 (25) = happyShift action_38
action_23 (26) = happyShift action_39
action_23 (27) = happyShift action_40
action_23 (28) = happyShift action_47
action_23 (12) = happyGoto action_44
action_23 (13) = happyGoto action_36
action_23 (14) = happyGoto action_45
action_23 (15) = happyGoto action_46
action_23 _ = happyFail (happyExpListPerState 23)

action_24 _ = happyReduce_15

action_25 (21) = happyShift action_43
action_25 _ = happyReduce_8

action_26 _ = happyReduce_13

action_27 (22) = happyShift action_42
action_27 _ = happyFail (happyExpListPerState 27)

action_28 _ = happyReduce_11

action_29 (24) = happyShift action_37
action_29 (25) = happyShift action_38
action_29 (26) = happyShift action_39
action_29 (27) = happyShift action_40
action_29 (28) = happyShift action_41
action_29 (12) = happyGoto action_35
action_29 (13) = happyGoto action_36
action_29 _ = happyFail (happyExpListPerState 29)

action_30 _ = happyReduce_35

action_31 (21) = happyShift action_34
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (21) = happyShift action_33
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (34) = happyShift action_56
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (28) = happyShift action_27
action_34 (33) = happyShift action_55
action_34 (9) = happyGoto action_50
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (21) = happyShift action_54
action_35 _ = happyFail (happyExpListPerState 35)

action_36 _ = happyReduce_22

action_37 _ = happyReduce_23

action_38 _ = happyReduce_24

action_39 _ = happyReduce_25

action_40 _ = happyReduce_26

action_41 (21) = happyShift action_53
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (30) = happyShift action_52
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (28) = happyShift action_27
action_43 (31) = happyShift action_51
action_43 (9) = happyGoto action_50
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (21) = happyShift action_49
action_44 _ = happyReduce_18

action_45 (23) = happyShift action_48
action_45 _ = happyReduce_19

action_46 _ = happyReduce_28

action_47 _ = happyReduce_29

action_48 (28) = happyShift action_47
action_48 (15) = happyGoto action_62
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (24) = happyShift action_37
action_49 (25) = happyShift action_38
action_49 (26) = happyShift action_39
action_49 (27) = happyShift action_40
action_49 (28) = happyShift action_47
action_49 (13) = happyGoto action_58
action_49 (14) = happyGoto action_61
action_49 (15) = happyGoto action_46
action_49 _ = happyFail (happyExpListPerState 49)

action_50 _ = happyReduce_12

action_51 _ = happyReduce_9

action_52 _ = happyReduce_14

action_53 (28) = happyShift action_60
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (24) = happyShift action_37
action_54 (25) = happyShift action_38
action_54 (26) = happyShift action_39
action_54 (27) = happyShift action_40
action_54 (28) = happyShift action_59
action_54 (13) = happyGoto action_58
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (21) = happyShift action_57
action_55 _ = happyFail (happyExpListPerState 55)

action_56 _ = happyReduce_34

action_57 (34) = happyShift action_64
action_57 _ = happyFail (happyExpListPerState 57)

action_58 _ = happyReduce_21

action_59 (21) = happyShift action_63
action_59 _ = happyFail (happyExpListPerState 59)

action_60 _ = happyReduce_38

action_61 (23) = happyShift action_48
action_61 _ = happyReduce_17

action_62 _ = happyReduce_27

action_63 (28) = happyShift action_65
action_63 _ = happyFail (happyExpListPerState 63)

action_64 _ = happyReduce_33

action_65 _ = happyReduce_37

happyReduce_1 = happySpecReduce_2  4 happyReduction_1
happyReduction_1 (HappyAbsSyn16  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (postComputateFile $ File (reverse happy_var_1) (reverse happy_var_2)
	)
happyReduction_1 _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (postComputateFile $ File (reverse happy_var_1) []
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  4 happyReduction_3
happyReduction_3 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn4
		 (postComputateFile $ File [] (reverse happy_var_1)
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_0  4 happyReduction_4
happyReduction_4  =  HappyAbsSyn4
		 (postComputateFile $ File [] []
	)

happyReduce_5 = happySpecReduce_2  5 happyReduction_5
happyReduction_5 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_2 : happy_var_1
	)
happyReduction_5 _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  5 happyReduction_6
happyReduction_6 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 ([happy_var_1]
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_2  6 happyReduction_7
happyReduction_7 (HappyAbsSyn10  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (NodeSet happy_var_1 (reverse happy_var_2)
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  7 happyReduction_8
happyReduction_8 (HappyAbsSyn8  happy_var_3)
	_
	_
	 =  HappyAbsSyn7
		 (NodeHeader (reverse happy_var_3) False
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happyReduce 5 7 happyReduction_9
happyReduction_9 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (NodeHeader (reverse happy_var_3) True
	) `HappyStk` happyRest

happyReduce_10 = happySpecReduce_1  7 happyReduction_10
happyReduction_10 _
	 =  HappyAbsSyn7
		 (NodeHeader [] False
	)

happyReduce_11 = happySpecReduce_3  7 happyReduction_11
happyReduction_11 _
	_
	_
	 =  HappyAbsSyn7
		 (NodeHeader [] True
	)

happyReduce_12 = happySpecReduce_3  8 happyReduction_12
happyReduction_12 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_3 : happy_var_1
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  8 happyReduction_13
happyReduction_13 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 ([happy_var_1]
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  9 happyReduction_14
happyReduction_14 (HappyTerminal (Tok _ (TokenFieldType happy_var_3)))
	_
	(HappyTerminal (Tok _ (TokenString happy_var_1)))
	 =  HappyAbsSyn9
		 (Field happy_var_1 happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_2  10 happyReduction_15
happyReduction_15 (HappyAbsSyn11  happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_2 : happy_var_1
	)
happyReduction_15 _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  10 happyReduction_16
happyReduction_16 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 ([happy_var_1]
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happyReduce 5 11 happyReduction_17
happyReduction_17 ((HappyAbsSyn14  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Tok _ (TokenString happy_var_1))) `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (INodeEntry happy_var_1 (reverse happy_var_3) (reverse happy_var_5)
	) `HappyStk` happyRest

happyReduce_18 = happySpecReduce_3  11 happyReduction_18
happyReduction_18 (HappyAbsSyn12  happy_var_3)
	_
	(HappyTerminal (Tok _ (TokenString happy_var_1)))
	 =  HappyAbsSyn11
		 (INodeEntry happy_var_1 (reverse happy_var_3) []
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  11 happyReduction_19
happyReduction_19 (HappyAbsSyn14  happy_var_3)
	_
	(HappyTerminal (Tok _ (TokenString happy_var_1)))
	 =  HappyAbsSyn11
		 (INodeEntry happy_var_1 [] (reverse happy_var_3)
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  11 happyReduction_20
happyReduction_20 (HappyTerminal (Tok _ (TokenString happy_var_1)))
	 =  HappyAbsSyn11
		 (INodeEntry happy_var_1 [] []
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  12 happyReduction_21
happyReduction_21 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_3 : happy_var_1
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  12 happyReduction_22
happyReduction_22 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn12
		 ([happy_var_1]
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  13 happyReduction_23
happyReduction_23 (HappyTerminal (Tok _ (TokenStrVal happy_var_1)))
	 =  HappyAbsSyn13
		 (LiteralStr happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  13 happyReduction_24
happyReduction_24 (HappyTerminal (Tok _ (TokenIntVal happy_var_1)))
	 =  HappyAbsSyn13
		 (LiteralInt happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  13 happyReduction_25
happyReduction_25 (HappyTerminal (Tok _ (TokenBoolVal happy_var_1)))
	 =  HappyAbsSyn13
		 (LiteralBool happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  13 happyReduction_26
happyReduction_26 _
	 =  HappyAbsSyn13
		 (LiteralNull
	)

happyReduce_27 = happySpecReduce_3  14 happyReduction_27
happyReduction_27 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_3 : happy_var_1
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  14 happyReduction_28
happyReduction_28 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn14
		 ([happy_var_1]
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  15 happyReduction_29
happyReduction_29 (HappyTerminal (Tok _ (TokenString happy_var_1)))
	 =  HappyAbsSyn15
		 (Label happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_2  16 happyReduction_30
happyReduction_30 (HappyAbsSyn17  happy_var_2)
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_2 : happy_var_1
	)
happyReduction_30 _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  16 happyReduction_31
happyReduction_31 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn16
		 ([happy_var_1]
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_2  17 happyReduction_32
happyReduction_32 (HappyAbsSyn19  happy_var_2)
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn17
		 (RelationshipSet happy_var_1 (reverse happy_var_2)
	)
happyReduction_32 _ _  = notHappyAtAll 

happyReduce_33 = happyReduce 7 18 happyReduction_33
happyReduction_33 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (RelationshipHeader (reverse happy_var_3)
	) `HappyStk` happyRest

happyReduce_34 = happyReduce 5 18 happyReduction_34
happyReduction_34 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (RelationshipHeader []
	) `HappyStk` happyRest

happyReduce_35 = happySpecReduce_2  19 happyReduction_35
happyReduction_35 (HappyAbsSyn20  happy_var_2)
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_2 : happy_var_1
	)
happyReduction_35 _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  19 happyReduction_36
happyReduction_36 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn19
		 ([happy_var_1]
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happyReduce 7 20 happyReduction_37
happyReduction_37 ((HappyTerminal (Tok _ (TokenString happy_var_7))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Tok _ (TokenString happy_var_5))) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Tok _ (TokenString happy_var_1))) `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (IRelationshipEntry happy_var_1 happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_38 = happyReduce 5 20 happyReduction_38
happyReduction_38 ((HappyTerminal (Tok _ (TokenString happy_var_5))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Tok _ (TokenString happy_var_3))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Tok _ (TokenString happy_var_1))) `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (IRelationshipEntry happy_var_1 [] happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	action 35 35 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	Tok _ TokenComma -> cont 21;
	Tok _ TokenColon -> cont 22;
	Tok _ TokenSemiColon -> cont 23;
	Tok _ (TokenStrVal happy_dollar_dollar) -> cont 24;
	Tok _ (TokenIntVal happy_dollar_dollar) -> cont 25;
	Tok _ (TokenBoolVal happy_dollar_dollar) -> cont 26;
	Tok _ TokenNullVal -> cont 27;
	Tok _ (TokenString happy_dollar_dollar) -> cont 28;
	Tok _ TokenID -> cont 29;
	Tok _ (TokenFieldType happy_dollar_dollar) -> cont 30;
	Tok _ TokenLabel -> cont 31;
	Tok _ TokenStartID -> cont 32;
	Tok _ TokenEndID -> cont 33;
	Tok _ TokenType -> cont 34;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 35 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Prelude.Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Prelude.Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(InputToken)], [Prelude.String]) -> HappyIdentity a
happyError' = HappyIdentity Prelude.. (\(tokens, _) -> parseError tokens)
inputParser tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [InputToken] -> a
parseError [] = error "unknown error"
parseError ts = error $ "Parse error at line " ++ (show ln) ++ " column " ++ (show col) ++ " previous character " ++ (show ch) ++ " tokens " ++ (show ts)
    where Tok p cl = head ts
          AlexPn ch ln col = p

data File       = File NodeSets RelationshipSets
                deriving (Show,Eq)

data NodeSet    = NodeSet NodeHeader NodeEntries
                deriving (Show,Eq)

type NodeSets   = [NodeSet]


-- Bool in nodeheader describes whether there is a Label field
data NodeHeader = NodeHeader Fields Bool
                deriving (Show,Eq)

type Fields     = [Field]
data Field      = Field String TokenFieldType
                deriving (Show,Eq)


type NodeEntries    = [INodeEntry]
data INodeEntry      = INodeEntry String Literals Labels
                    deriving (Show,Eq)
type Literals       = [Literal]
data Literal        = LiteralStr String
                    | LiteralInt Int
                    | LiteralBool Bool
                    | LiteralNull
                    deriving (Show,Eq)
type Labels         = [Label] 
data Label          = Label String
                    deriving (Show,Eq)

type RelationshipSets       = [RelationshipSet]
data RelationshipSet        = RelationshipSet RelationshipHeader RelationshipEntries
                            deriving (Show,Eq)
data RelationshipHeader     = RelationshipHeader Fields
                            deriving (Show,Eq)
type RelationshipEntries    = [IRelationshipEntry] 
-- Relationship entry is IDs, (fields)*, StartID, EndID
data IRelationshipEntry      = IRelationshipEntry String Literals String String
                            deriving (Show,Eq)


data DataType
    = TypeString
    | TypeInt
    | TypeBool
    | TypeNull
    deriving (Show, Eq)

type FieldEntry    = (String, String, DataType)
type NodeEntry     = (String, String, DataType)
type RelationEntry = (String, String, DataType)
type Node          = [NodeEntry]
type Relation      = [RelationEntry]
type Nodes         = [Node]
type Relations     = [Relation]
-- First list is node second is relation 
postComputateFile :: File -> (Nodes,Relations)
postComputateFile (File nodeSets relationshipSets) 
    = (postComputateNodeSets nodeSets,postComputateRelationshipSets relationshipSets)

postComputateNodeSets :: [NodeSet] -> Nodes
postComputateNodeSets []                   = []
postComputateNodeSets (nodeSet:nodeSets)   = postComputateNodeSet nodeSet ++ postComputateNodeSets nodeSets


postComputateNodeSet :: NodeSet -> Nodes
postComputateNodeSet (NodeSet (NodeHeader fields _) entries) 
    = postComputateNodeEntries fields entries

postComputateNodeEntries :: Fields -> NodeEntries -> Nodes
postComputateNodeEntries _ [] = []
postComputateNodeEntries fields (nodeEntry:nodeEntries) 
    = postComputateNodeEntry fields nodeEntry : postComputateNodeEntries fields nodeEntries

postComputateNodeEntry :: Fields -> INodeEntry -> Node
postComputateNodeEntry fields (INodeEntry id literals labels) 
    = [(":ID", id, TypeString)] ++ postComputateLiterals fields literals ++ postComputateLabels labels 

postComputateLabels :: Labels -> [NodeEntry]
postComputateLabels []                   = []
postComputateLabels ((Label str) : labels) 
    = (":LABEL",str,TypeString) : postComputateLabels labels

postComputateRelationshipSets :: [RelationshipSet] -> Relations
postComputateRelationshipSets [] = []
postComputateRelationshipSets (relationshipSet : relationshipSets)  
    = postComputateRelationshipSet relationshipSet ++ postComputateRelationshipSets relationshipSets

postComputateRelationshipSet :: RelationshipSet -> Relations
postComputateRelationshipSet (RelationshipSet (RelationshipHeader fields) relationshipEntries)
    = postComputateRelationshipEntries fields relationshipEntries

postComputateRelationshipEntries :: Fields -> RelationshipEntries -> Relations
postComputateRelationshipEntries _ [] = []
postComputateRelationshipEntries fields (relationshipEntry : relationshipEntries) 
    = postComputateRelationshipEntry fields relationshipEntry : postComputateRelationshipEntries fields relationshipEntries

postComputateRelationshipEntry :: Fields -> IRelationshipEntry -> Relation
postComputateRelationshipEntry fields (IRelationshipEntry start literals end t)
    = [(":START_ID", start, TypeString)] ++ postComputateLiterals fields literals ++ [(":END_ID", end, TypeString)] ++ [(":TYPE", t, TypeString)]

-- postComputateLiterals :: Fields -> Literals -> [FieldEntry]
-- postComputateLiterals []                                        []                              = []
-- postComputateLiterals ((Field fieldName TTypeString)  : fields) ((LiteralStr str)   : literals) = (fieldName, str,       TypeString) : postComputateLiterals fields literals
-- postComputateLiterals ((Field fieldName TTypeInteger) : fields) ((LiteralInt int)   : literals) = (fieldName, show int,  TypeInt)    : postComputateLiterals fields literals
-- postComputateLiterals ((Field fieldName TTypeBoolean) : fields) ((LiteralBool bool) : literals) = (fieldName, show bool, TypeBool)   : postComputateLiterals fields literals
-- postComputateLiterals ((Field fieldName _)            : fields) (LiteralNull        : literals) = (fieldName, "null",    TypeNull)   : postComputateLiterals fields literals
-- postComputateLiterals _ _ = error "Invalid n4j input"

postComputateLiterals :: Fields -> Literals -> [FieldEntry]
postComputateLiterals []                                        []                              = []
postComputateLiterals ((Field fieldName TTypeString)  : fields) ((LiteralStr str)   : literals) = (fieldName, str,       TypeString) : postComputateLiterals fields literals
postComputateLiterals ((Field fieldName TTypeInteger) : fields) ((LiteralInt int)   : literals) = (fieldName, show int,  TypeInt)    : postComputateLiterals fields literals
postComputateLiterals ((Field fieldName TTypeBoolean) : fields) ((LiteralBool bool) : literals) = (fieldName, show bool, TypeBool)   : postComputateLiterals fields literals

postComputateLiterals ((Field fieldName TTypeString)  : fields) ((LiteralNull)      : literals) = (fieldName, "null",    TypeString)   : postComputateLiterals fields literals
postComputateLiterals ((Field fieldName TTypeInteger) : fields) ((LiteralNull)      : literals) = (fieldName, "null",    TypeInt)      : postComputateLiterals fields literals
postComputateLiterals ((Field fieldName TTypeBoolean) : fields) ((LiteralNull)      : literals) = (fieldName, "null",    TypeBool)     : postComputateLiterals fields literals

postComputateLiterals _ _ = error "Invalid n4j input"
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