{-# OPTIONS_GHC -w #-}
module InputParser where
import InputLexer
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25
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
	| HappyAbsSyn21 t21
	| HappyAbsSyn22 t22
	| HappyAbsSyn23 t23
	| HappyAbsSyn24 t24
	| HappyAbsSyn25 t25

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,118) ([0,0,2,0,512,0,0,16,0,512,0,0,0,0,256,0,512,0,0,0,0,0,9,0,0,0,0,1,0,0,0,512,0,0,0,0,0,0,0,4096,0,0,0,0,256,0,512,0,0,8448,0,0,0,0,256,0,0,0,0,2,0,0,0,0,496,0,0,0,0,2,0,0,0,0,4,0,0,0,0,1024,0,0,9,0,2,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,496,0,0,0,0,2,0,512,0,0,16384,0,0,33,0,2,0,512,0,0,256,0,61440,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,61440,1,0,2,0,0,0,0,16384,0,512,0,0,0,0,0,1,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_inputParser","File","NodeSets","NodeSets1","NodeSet","NodeHeader","Fields","Field","NodeEntries","NodeEntries1","NodeEntry","Literals","Literal","Labels","Labels1","Label","RelationshipSets","RelationshipSets1","RelationshipSet","RelationshipHeader","RelationshipEntries","RelationshipEntries1","RelationshipEntry","\",\"","\":\"","\";\"","strVal","intVal","boolVal","nullVal","string","\":ID\"","type","\":LABEL\"","\":START_ID\"","\":END_ID\"","\":TYPE\"","%eof"]
        bit_start = st Prelude.* 40
        bit_end = (st Prelude.+ 1) Prelude.* 40
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..39]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (34) = happyShift action_6
action_0 (4) = happyGoto action_7
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_3
action_0 (7) = happyGoto action_4
action_0 (8) = happyGoto action_5
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (34) = happyShift action_6
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 (7) = happyGoto action_4
action_1 (8) = happyGoto action_5
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (37) = happyShift action_18
action_2 (19) = happyGoto action_14
action_2 (20) = happyGoto action_15
action_2 (21) = happyGoto action_16
action_2 (22) = happyGoto action_17
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (34) = happyShift action_6
action_3 (7) = happyGoto action_13
action_3 (8) = happyGoto action_5
action_3 _ = happyReduce_2

action_4 _ = happyReduce_4

action_5 (33) = happyShift action_12
action_5 (11) = happyGoto action_9
action_5 (12) = happyGoto action_10
action_5 (13) = happyGoto action_11
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (26) = happyShift action_8
action_6 _ = happyReduce_8

action_7 (40) = happyAccept
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (33) = happyShift action_29
action_8 (36) = happyShift action_30
action_8 (9) = happyGoto action_27
action_8 (10) = happyGoto action_28
action_8 _ = happyFail (happyExpListPerState 8)

action_9 _ = happyReduce_5

action_10 (33) = happyShift action_12
action_10 (13) = happyGoto action_26
action_10 _ = happyReduce_13

action_11 _ = happyReduce_15

action_12 (26) = happyShift action_25
action_12 _ = happyReduce_19

action_13 _ = happyReduce_3

action_14 _ = happyReduce_1

action_15 (37) = happyShift action_18
action_15 (21) = happyGoto action_24
action_15 (22) = happyGoto action_17
action_15 _ = happyReduce_30

action_16 _ = happyReduce_32

action_17 (33) = happyShift action_23
action_17 (23) = happyGoto action_20
action_17 (24) = happyGoto action_21
action_17 (25) = happyGoto action_22
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (26) = happyShift action_19
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (33) = happyShift action_29
action_19 (38) = happyShift action_46
action_19 (9) = happyGoto action_45
action_19 (10) = happyGoto action_28
action_19 _ = happyFail (happyExpListPerState 19)

action_20 _ = happyReduce_33

action_21 (33) = happyShift action_23
action_21 (25) = happyGoto action_44
action_21 _ = happyReduce_36

action_22 _ = happyReduce_38

action_23 (26) = happyShift action_43
action_23 _ = happyFail (happyExpListPerState 23)

action_24 _ = happyReduce_31

action_25 (29) = happyShift action_38
action_25 (30) = happyShift action_39
action_25 (31) = happyShift action_40
action_25 (32) = happyShift action_41
action_25 (33) = happyShift action_42
action_25 (14) = happyGoto action_33
action_25 (15) = happyGoto action_34
action_25 (16) = happyGoto action_35
action_25 (17) = happyGoto action_36
action_25 (18) = happyGoto action_37
action_25 _ = happyFail (happyExpListPerState 25)

action_26 _ = happyReduce_14

action_27 (26) = happyShift action_32
action_27 _ = happyReduce_6

action_28 _ = happyReduce_11

action_29 (27) = happyShift action_31
action_29 _ = happyFail (happyExpListPerState 29)

action_30 _ = happyReduce_9

action_31 (35) = happyShift action_55
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (33) = happyShift action_29
action_32 (36) = happyShift action_54
action_32 (10) = happyGoto action_53
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (26) = happyShift action_52
action_33 _ = happyReduce_17

action_34 _ = happyReduce_21

action_35 _ = happyReduce_18

action_36 (28) = happyShift action_51
action_36 _ = happyReduce_26

action_37 _ = happyReduce_28

action_38 _ = happyReduce_22

action_39 _ = happyReduce_23

action_40 _ = happyReduce_24

action_41 _ = happyReduce_25

action_42 _ = happyReduce_29

action_43 (29) = happyShift action_38
action_43 (30) = happyShift action_39
action_43 (31) = happyShift action_40
action_43 (32) = happyShift action_41
action_43 (33) = happyShift action_50
action_43 (14) = happyGoto action_49
action_43 (15) = happyGoto action_34
action_43 _ = happyFail (happyExpListPerState 43)

action_44 _ = happyReduce_37

action_45 (26) = happyShift action_48
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (26) = happyShift action_47
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (39) = happyShift action_62
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (33) = happyShift action_29
action_48 (38) = happyShift action_61
action_48 (10) = happyGoto action_53
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (26) = happyShift action_60
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (26) = happyShift action_59
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (33) = happyShift action_42
action_51 (18) = happyGoto action_58
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (29) = happyShift action_38
action_52 (30) = happyShift action_39
action_52 (31) = happyShift action_40
action_52 (32) = happyShift action_41
action_52 (33) = happyShift action_42
action_52 (15) = happyGoto action_56
action_52 (16) = happyGoto action_57
action_52 (17) = happyGoto action_36
action_52 (18) = happyGoto action_37
action_52 _ = happyFail (happyExpListPerState 52)

action_53 _ = happyReduce_10

action_54 _ = happyReduce_7

action_55 _ = happyReduce_12

action_56 _ = happyReduce_20

action_57 _ = happyReduce_16

action_58 _ = happyReduce_27

action_59 (33) = happyShift action_65
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (29) = happyShift action_38
action_60 (30) = happyShift action_39
action_60 (31) = happyShift action_40
action_60 (32) = happyShift action_41
action_60 (33) = happyShift action_64
action_60 (15) = happyGoto action_56
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (26) = happyShift action_63
action_61 _ = happyFail (happyExpListPerState 61)

action_62 _ = happyReduce_35

action_63 (39) = happyShift action_67
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (26) = happyShift action_66
action_64 _ = happyFail (happyExpListPerState 64)

action_65 _ = happyReduce_40

action_66 (33) = happyShift action_68
action_66 _ = happyFail (happyExpListPerState 66)

action_67 _ = happyReduce_34

action_68 _ = happyReduce_39

happyReduce_1 = happySpecReduce_2  4 happyReduction_1
happyReduction_1 (HappyAbsSyn19  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (File happy_var_1 happy_var_2
	)
happyReduction_1 _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (reverse happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_2  6 happyReduction_3
happyReduction_3 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_2 : happy_var_1
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  6 happyReduction_4
happyReduction_4 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 ([happy_var_1]
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_2  7 happyReduction_5
happyReduction_5 (HappyAbsSyn11  happy_var_2)
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (NodeSet happy_var_1 happy_var_2
	)
happyReduction_5 _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  8 happyReduction_6
happyReduction_6 (HappyAbsSyn9  happy_var_3)
	_
	_
	 =  HappyAbsSyn8
		 (NodeHeader happy_var_3 False
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happyReduce 5 8 happyReduction_7
happyReduction_7 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (NodeHeader happy_var_3 True
	) `HappyStk` happyRest

happyReduce_8 = happySpecReduce_1  8 happyReduction_8
happyReduction_8 _
	 =  HappyAbsSyn8
		 (NodeHeader [] False
	)

happyReduce_9 = happySpecReduce_3  8 happyReduction_9
happyReduction_9 _
	_
	_
	 =  HappyAbsSyn8
		 (NodeHeader [] True
	)

happyReduce_10 = happySpecReduce_3  9 happyReduction_10
happyReduction_10 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_3 : happy_var_1
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  9 happyReduction_11
happyReduction_11 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 ([happy_var_1]
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  10 happyReduction_12
happyReduction_12 (HappyTerminal (Tok _ (TokenFieldType happy_var_3)))
	_
	(HappyTerminal (Tok _ (TokenString happy_var_1)))
	 =  HappyAbsSyn10
		 (Field happy_var_1 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  11 happyReduction_13
happyReduction_13 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (reverse happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_2  12 happyReduction_14
happyReduction_14 (HappyAbsSyn13  happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_2 : happy_var_1
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  12 happyReduction_15
happyReduction_15 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn12
		 ([happy_var_1]
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happyReduce 5 13 happyReduction_16
happyReduction_16 ((HappyAbsSyn16  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Tok _ (TokenString happy_var_1))) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (NodeEntry happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_17 = happySpecReduce_3  13 happyReduction_17
happyReduction_17 (HappyAbsSyn14  happy_var_3)
	_
	(HappyTerminal (Tok _ (TokenString happy_var_1)))
	 =  HappyAbsSyn13
		 (NodeEntry happy_var_1 happy_var_3 []
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  13 happyReduction_18
happyReduction_18 (HappyAbsSyn16  happy_var_3)
	_
	(HappyTerminal (Tok _ (TokenString happy_var_1)))
	 =  HappyAbsSyn13
		 (NodeEntry happy_var_1 [] happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  13 happyReduction_19
happyReduction_19 (HappyTerminal (Tok _ (TokenString happy_var_1)))
	 =  HappyAbsSyn13
		 (NodeEntry happy_var_1 [] []
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  14 happyReduction_20
happyReduction_20 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_3 : happy_var_1
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  14 happyReduction_21
happyReduction_21 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn14
		 ([happy_var_1]
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  15 happyReduction_22
happyReduction_22 (HappyTerminal (Tok _ (TokenStrVal happy_var_1)))
	 =  HappyAbsSyn15
		 (LiteralStr happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  15 happyReduction_23
happyReduction_23 (HappyTerminal (Tok _ (TokenIntVal happy_var_1)))
	 =  HappyAbsSyn15
		 (LiteralInt happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  15 happyReduction_24
happyReduction_24 (HappyTerminal (Tok _ (TokenBoolVal happy_var_1)))
	 =  HappyAbsSyn15
		 (LiteralBool happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  15 happyReduction_25
happyReduction_25 _
	 =  HappyAbsSyn15
		 (LiteralNull
	)

happyReduce_26 = happySpecReduce_1  16 happyReduction_26
happyReduction_26 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn16
		 (reverse happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  17 happyReduction_27
happyReduction_27 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_3 : happy_var_1
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  17 happyReduction_28
happyReduction_28 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn17
		 ([happy_var_1]
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  18 happyReduction_29
happyReduction_29 (HappyTerminal (Tok _ (TokenString happy_var_1)))
	 =  HappyAbsSyn18
		 (Label happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  19 happyReduction_30
happyReduction_30 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn19
		 (reverse happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_2  20 happyReduction_31
happyReduction_31 (HappyAbsSyn21  happy_var_2)
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_2 : happy_var_1
	)
happyReduction_31 _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  20 happyReduction_32
happyReduction_32 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn20
		 ([happy_var_1]
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_2  21 happyReduction_33
happyReduction_33 (HappyAbsSyn23  happy_var_2)
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn21
		 (RelationshipSet happy_var_1 happy_var_2
	)
happyReduction_33 _ _  = notHappyAtAll 

happyReduce_34 = happyReduce 7 22 happyReduction_34
happyReduction_34 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 (RelationshipHeader happy_var_3
	) `HappyStk` happyRest

happyReduce_35 = happyReduce 5 22 happyReduction_35
happyReduction_35 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 (RelationshipHeader []
	) `HappyStk` happyRest

happyReduce_36 = happySpecReduce_1  23 happyReduction_36
happyReduction_36 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn23
		 (reverse happy_var_1
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_2  24 happyReduction_37
happyReduction_37 (HappyAbsSyn25  happy_var_2)
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_2 : happy_var_1
	)
happyReduction_37 _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  24 happyReduction_38
happyReduction_38 (HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn24
		 ([happy_var_1]
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happyReduce 7 25 happyReduction_39
happyReduction_39 ((HappyTerminal (Tok _ (TokenString happy_var_7))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Tok _ (TokenString happy_var_5))) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Tok _ (TokenString happy_var_1))) `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (RelationshipEntry happy_var_1 happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_40 = happyReduce 5 25 happyReduction_40
happyReduction_40 ((HappyTerminal (Tok _ (TokenString happy_var_5))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Tok _ (TokenString happy_var_3))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Tok _ (TokenString happy_var_1))) `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (RelationshipEntry happy_var_1 [] happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	action 40 40 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	Tok _ TokenComma -> cont 26;
	Tok _ TokenColon -> cont 27;
	Tok _ TokenSemiColon -> cont 28;
	Tok _ (TokenStrVal happy_dollar_dollar) -> cont 29;
	Tok _ (TokenIntVal happy_dollar_dollar) -> cont 30;
	Tok _ (TokenBoolVal happy_dollar_dollar) -> cont 31;
	Tok _ TokenNullVal -> cont 32;
	Tok _ (TokenString happy_dollar_dollar) -> cont 33;
	Tok _ TokenID -> cont 34;
	Tok _ (TokenFieldType happy_dollar_dollar) -> cont 35;
	Tok _ TokenLabel -> cont 36;
	Tok _ TokenStartID -> cont 37;
	Tok _ TokenEndID -> cont 38;
	Tok _ TokenType -> cont 39;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 40 tk tks = happyError' (tks, explist)
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
parseError [] = error "uknown error"
parseError ts = error $ "Parse error at line " ++ (show ln) ++ " column " ++ (show col) ++ " previous character " ++ (show ch) ++ " tokens " ++ (show ts)
    where Tok p cl = head ts
          AlexPn ch ln col = p

data File       = File NodeSets RelationshipSets
                deriving (Show,Eq)

type NodeSets   = [NodeSet]

data NodeSet    = NodeSet NodeHeader NodeEntries
                deriving (Show,Eq)
-- Bool in nodeheader describes whether there is a Label field
data NodeHeader = NodeHeader Fields Bool
                deriving (Show,Eq)

type Fields     = [Field]
data Field      = Field String FieldType
                deriving (Show,Eq)


type NodeEntries    = [NodeEntry]
data NodeEntry      = NodeEntry String Literals Labels
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
type RelationshipEntries    = [RelationshipEntry] 
-- Relationship entry is IDs, (fields)*, StartID, EndID
data RelationshipEntry      = RelationshipEntry String Literals String String
                            deriving (Show,Eq)
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
