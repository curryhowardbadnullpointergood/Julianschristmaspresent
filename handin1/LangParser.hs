{-# OPTIONS_GHC -w #-}
module LangParser where
import LangLexer
import InputParser (Literal(..))
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17
	= HappyTerminal (LangToken)
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

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,157) ([0,0,8,0,0,4096,0,0,0,64,0,32768,0,0,0,0,0,0,0,0,0,4,0,0,0,0,8,0,0,0,0,32768,0,0,0,0,0,32768,0,0,64,0,0,0,10,0,0,3072,0,4096,0,8,0,32,0,0,16384,0,0,192,0,8192,0,0,0,0,4112,512,0,0,0,0,256,0,0,0,2,0,0,0,0,0,0,0,0,64,8192,0,0,0,0,0,256,0,0,0,2,768,0,0,0,0,0,8,0,0,4096,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,512,0,0,0,4,0,0,0,0,24576,0,0,0,32768,252,0,0,514,64,0,1024,32772,0,61444,1,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,1008,0,0,57344,7,0,0,4032,0,0,32768,31,0,0,16128,0,0,0,126,0,0,64512,0,0,2056,256,0,4096,16,2,0,1024,0,32768,0,0,0,128,0,0,32768,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,16,0,0,8192,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,448,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,224,0,0,0,16384,0,0,0,128,0,0,0,1,0,0,0,0,0,0,32768,0,0,0,256,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_langParser","Query","Read","Match","Patterns","Pattern","Return","Return1","Outputs","Output","Where","WhereExp1","WhereFunc","WhereDot","WhereLit","\".\"","\",\"","\"|\"","\"=\"","\"-\"","\"->\"","\"<-\"","intField","strField","boolField","labelField","idField","startField","endField","typeField","or","and","not","read","match","where","getNode","getRelation","as","starts","\"(\"","\")\"","\"<=\"","\">=\"","\"<\"","\">\"","\"==\"","\"/=\"","null","true","false","string","int","name","%eof"]
        bit_start = st Prelude.* 57
        bit_end = (st Prelude.+ 1) Prelude.* 57
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..56]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (36) = happyShift action_3
action_0 (4) = happyGoto action_4
action_0 (5) = happyGoto action_2
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (36) = happyShift action_3
action_1 (5) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (37) = happyShift action_7
action_2 (6) = happyGoto action_6
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (21) = happyShift action_5
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (57) = happyAccept
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (54) = happyShift action_9
action_5 _ = happyFail (happyExpListPerState 5)

action_6 _ = happyReduce_1

action_7 (21) = happyShift action_8
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (56) = happyShift action_12
action_8 (7) = happyGoto action_10
action_8 (8) = happyGoto action_11
action_8 _ = happyFail (happyExpListPerState 8)

action_9 _ = happyReduce_2

action_10 (38) = happyShift action_17
action_10 (13) = happyGoto action_16
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (20) = happyShift action_15
action_11 _ = happyReduce_5

action_12 (22) = happyShift action_13
action_12 (24) = happyShift action_14
action_12 _ = happyReduce_12

action_13 (22) = happyShift action_25
action_13 (23) = happyShift action_26
action_13 (56) = happyShift action_27
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (22) = happyShift action_23
action_14 (56) = happyShift action_24
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (56) = happyShift action_12
action_15 (7) = happyGoto action_22
action_15 (8) = happyGoto action_11
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (39) = happyShift action_20
action_16 (40) = happyShift action_21
action_16 (9) = happyGoto action_19
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (21) = happyShift action_18
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (35) = happyShift action_39
action_18 (43) = happyShift action_40
action_18 (56) = happyShift action_41
action_18 (14) = happyGoto action_36
action_18 (15) = happyGoto action_37
action_18 (16) = happyGoto action_38
action_18 _ = happyFail (happyExpListPerState 18)

action_19 _ = happyReduce_3

action_20 (21) = happyShift action_35
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (21) = happyShift action_34
action_21 _ = happyFail (happyExpListPerState 21)

action_22 _ = happyReduce_4

action_23 (56) = happyShift action_33
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (22) = happyShift action_32
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (56) = happyShift action_31
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (56) = happyShift action_30
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (22) = happyShift action_28
action_27 (23) = happyShift action_29
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (56) = happyShift action_66
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (56) = happyShift action_65
action_29 _ = happyFail (happyExpListPerState 29)

action_30 _ = happyReduce_6

action_31 _ = happyReduce_10

action_32 (56) = happyShift action_64
action_32 _ = happyFail (happyExpListPerState 32)

action_33 _ = happyReduce_8

action_34 (56) = happyShift action_62
action_34 (10) = happyGoto action_63
action_34 (11) = happyGoto action_60
action_34 (12) = happyGoto action_61
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (56) = happyShift action_62
action_35 (10) = happyGoto action_59
action_35 (11) = happyGoto action_60
action_35 (12) = happyGoto action_61
action_35 _ = happyFail (happyExpListPerState 35)

action_36 _ = happyReduce_24

action_37 (33) = happyShift action_57
action_37 (34) = happyShift action_58
action_37 _ = happyReduce_29

action_38 (42) = happyShift action_50
action_38 (45) = happyShift action_51
action_38 (46) = happyShift action_52
action_38 (47) = happyShift action_53
action_38 (48) = happyShift action_54
action_38 (49) = happyShift action_55
action_38 (50) = happyShift action_56
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (35) = happyShift action_39
action_39 (43) = happyShift action_40
action_39 (56) = happyShift action_41
action_39 (14) = happyGoto action_49
action_39 (15) = happyGoto action_37
action_39 (16) = happyGoto action_38
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (35) = happyShift action_39
action_40 (43) = happyShift action_40
action_40 (56) = happyShift action_41
action_40 (14) = happyGoto action_48
action_40 (15) = happyGoto action_37
action_40 (16) = happyGoto action_38
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (18) = happyShift action_42
action_41 (28) = happyShift action_43
action_41 (29) = happyShift action_44
action_41 (30) = happyShift action_45
action_41 (31) = happyShift action_46
action_41 (32) = happyShift action_47
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (56) = happyShift action_94
action_42 _ = happyFail (happyExpListPerState 42)

action_43 _ = happyReduce_48

action_44 _ = happyReduce_44

action_45 _ = happyReduce_46

action_46 _ = happyReduce_47

action_47 _ = happyReduce_45

action_48 (44) = happyShift action_93
action_48 _ = happyFail (happyExpListPerState 48)

action_49 _ = happyReduce_27

action_50 (51) = happyShift action_76
action_50 (52) = happyShift action_77
action_50 (53) = happyShift action_78
action_50 (54) = happyShift action_79
action_50 (55) = happyShift action_80
action_50 (56) = happyShift action_41
action_50 (16) = happyGoto action_91
action_50 (17) = happyGoto action_92
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (51) = happyShift action_76
action_51 (52) = happyShift action_77
action_51 (53) = happyShift action_78
action_51 (54) = happyShift action_79
action_51 (55) = happyShift action_80
action_51 (56) = happyShift action_41
action_51 (16) = happyGoto action_89
action_51 (17) = happyGoto action_90
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (51) = happyShift action_76
action_52 (52) = happyShift action_77
action_52 (53) = happyShift action_78
action_52 (54) = happyShift action_79
action_52 (55) = happyShift action_80
action_52 (56) = happyShift action_41
action_52 (16) = happyGoto action_87
action_52 (17) = happyGoto action_88
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (51) = happyShift action_76
action_53 (52) = happyShift action_77
action_53 (53) = happyShift action_78
action_53 (54) = happyShift action_79
action_53 (55) = happyShift action_80
action_53 (56) = happyShift action_41
action_53 (16) = happyGoto action_85
action_53 (17) = happyGoto action_86
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (51) = happyShift action_76
action_54 (52) = happyShift action_77
action_54 (53) = happyShift action_78
action_54 (54) = happyShift action_79
action_54 (55) = happyShift action_80
action_54 (56) = happyShift action_41
action_54 (16) = happyGoto action_83
action_54 (17) = happyGoto action_84
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (51) = happyShift action_76
action_55 (52) = happyShift action_77
action_55 (53) = happyShift action_78
action_55 (54) = happyShift action_79
action_55 (55) = happyShift action_80
action_55 (56) = happyShift action_41
action_55 (16) = happyGoto action_81
action_55 (17) = happyGoto action_82
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (51) = happyShift action_76
action_56 (52) = happyShift action_77
action_56 (53) = happyShift action_78
action_56 (54) = happyShift action_79
action_56 (55) = happyShift action_80
action_56 (56) = happyShift action_41
action_56 (16) = happyGoto action_74
action_56 (17) = happyGoto action_75
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (35) = happyShift action_39
action_57 (43) = happyShift action_40
action_57 (56) = happyShift action_41
action_57 (14) = happyGoto action_73
action_57 (15) = happyGoto action_37
action_57 (16) = happyGoto action_38
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (35) = happyShift action_39
action_58 (43) = happyShift action_40
action_58 (56) = happyShift action_41
action_58 (14) = happyGoto action_72
action_58 (15) = happyGoto action_37
action_58 (16) = happyGoto action_38
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (40) = happyShift action_71
action_59 _ = happyReduce_14

action_60 (20) = happyShift action_70
action_60 _ = happyReduce_17

action_61 (19) = happyShift action_69
action_61 _ = happyReduce_19

action_62 (18) = happyShift action_67
action_62 (28) = happyShift action_68
action_62 _ = happyFail (happyExpListPerState 62)

action_63 _ = happyReduce_15

action_64 _ = happyReduce_9

action_65 _ = happyReduce_7

action_66 _ = happyReduce_11

action_67 (56) = happyShift action_101
action_67 _ = happyFail (happyExpListPerState 67)

action_68 _ = happyReduce_23

action_69 (56) = happyShift action_62
action_69 (11) = happyGoto action_100
action_69 (12) = happyGoto action_61
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (56) = happyShift action_62
action_70 (10) = happyGoto action_99
action_70 (11) = happyGoto action_60
action_70 (12) = happyGoto action_61
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (21) = happyShift action_98
action_71 _ = happyFail (happyExpListPerState 71)

action_72 _ = happyReduce_25

action_73 _ = happyReduce_26

action_74 _ = happyReduce_38

action_75 _ = happyReduce_31

action_76 _ = happyReduce_53

action_77 _ = happyReduce_55

action_78 _ = happyReduce_56

action_79 _ = happyReduce_52

action_80 _ = happyReduce_54

action_81 _ = happyReduce_37

action_82 _ = happyReduce_30

action_83 _ = happyReduce_40

action_84 _ = happyReduce_33

action_85 _ = happyReduce_39

action_86 _ = happyReduce_32

action_87 _ = happyReduce_42

action_88 _ = happyReduce_35

action_89 _ = happyReduce_41

action_90 _ = happyReduce_34

action_91 _ = happyReduce_43

action_92 _ = happyReduce_36

action_93 _ = happyReduce_28

action_94 (25) = happyShift action_95
action_94 (26) = happyShift action_96
action_94 (27) = happyShift action_97
action_94 _ = happyFail (happyExpListPerState 94)

action_95 _ = happyReduce_49

action_96 _ = happyReduce_50

action_97 _ = happyReduce_51

action_98 (56) = happyShift action_62
action_98 (10) = happyGoto action_105
action_98 (11) = happyGoto action_60
action_98 (12) = happyGoto action_61
action_98 _ = happyFail (happyExpListPerState 98)

action_99 _ = happyReduce_16

action_100 _ = happyReduce_18

action_101 (25) = happyShift action_102
action_101 (26) = happyShift action_103
action_101 (27) = happyShift action_104
action_101 _ = happyFail (happyExpListPerState 101)

action_102 (41) = happyShift action_108
action_102 _ = happyFail (happyExpListPerState 102)

action_103 (41) = happyShift action_107
action_103 _ = happyFail (happyExpListPerState 103)

action_104 (41) = happyShift action_106
action_104 _ = happyFail (happyExpListPerState 104)

action_105 _ = happyReduce_13

action_106 (54) = happyShift action_111
action_106 _ = happyFail (happyExpListPerState 106)

action_107 (54) = happyShift action_110
action_107 _ = happyFail (happyExpListPerState 107)

action_108 (54) = happyShift action_109
action_108 _ = happyFail (happyExpListPerState 108)

action_109 _ = happyReduce_20

action_110 _ = happyReduce_21

action_111 _ = happyReduce_22

happyReduce_1 = happySpecReduce_2  4 happyReduction_1
happyReduction_1 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (Query happy_var_1 happy_var_2
	)
happyReduction_1 _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_3  5 happyReduction_2
happyReduction_2 (HappyTerminal (LTok _ (LTokenString happy_var_3)))
	_
	_
	 =  HappyAbsSyn5
		 (ReadFile happy_var_3
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happyReduce 5 6 happyReduction_3
happyReduction_3 ((HappyAbsSyn9  happy_var_5) `HappyStk`
	(HappyAbsSyn13  happy_var_4) `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (Match happy_var_3 happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_4 = happySpecReduce_3  7 happyReduction_4
happyReduction_4 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1 : happy_var_3
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  7 happyReduction_5
happyReduction_5 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 ([happy_var_1]
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happyReduce 4 8 happyReduction_6
happyReduction_6 ((HappyTerminal (LTok _ (LTokenName happy_var_4))) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTok _ (LTokenName happy_var_1))) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (PatternRelatedTo happy_var_1 happy_var_4
	) `HappyStk` happyRest

happyReduce_7 = happyReduce 5 8 happyReduction_7
happyReduction_7 ((HappyTerminal (LTok _ (LTokenName happy_var_5))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTok _ (LTokenName happy_var_3))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTok _ (LTokenName happy_var_1))) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (PatternRelatedToVar happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_8 = happyReduce 4 8 happyReduction_8
happyReduction_8 ((HappyTerminal (LTok _ (LTokenName happy_var_4))) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTok _ (LTokenName happy_var_1))) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (PatternRelatedBy happy_var_1 happy_var_4
	) `HappyStk` happyRest

happyReduce_9 = happyReduce 5 8 happyReduction_9
happyReduction_9 ((HappyTerminal (LTok _ (LTokenName happy_var_5))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTok _ (LTokenName happy_var_3))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTok _ (LTokenName happy_var_1))) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (PatternRelatedByVar happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_10 = happyReduce 4 8 happyReduction_10
happyReduction_10 ((HappyTerminal (LTok _ (LTokenName happy_var_4))) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTok _ (LTokenName happy_var_1))) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (PatternRelated happy_var_1 happy_var_4
	) `HappyStk` happyRest

happyReduce_11 = happyReduce 5 8 happyReduction_11
happyReduction_11 ((HappyTerminal (LTok _ (LTokenName happy_var_5))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTok _ (LTokenName happy_var_3))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTok _ (LTokenName happy_var_1))) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (PatternRelatedVar happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_12 = happySpecReduce_1  8 happyReduction_12
happyReduction_12 (HappyTerminal (LTok _ (LTokenName happy_var_1)))
	 =  HappyAbsSyn8
		 (PatternFinal happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happyReduce 6 9 happyReduction_13
happyReduction_13 ((HappyAbsSyn10  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (ReturnNodeRelation happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_14 = happySpecReduce_3  9 happyReduction_14
happyReduction_14 (HappyAbsSyn10  happy_var_3)
	_
	_
	 =  HappyAbsSyn9
		 (ReturnNode happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  9 happyReduction_15
happyReduction_15 (HappyAbsSyn10  happy_var_3)
	_
	_
	 =  HappyAbsSyn9
		 (ReturnRelation happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  10 happyReduction_16
happyReduction_16 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1 : happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  10 happyReduction_17
happyReduction_17 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 ([happy_var_1]
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  11 happyReduction_18
happyReduction_18 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 : happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  11 happyReduction_19
happyReduction_19 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 ([happy_var_1]
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happyReduce 6 12 happyReduction_20
happyReduction_20 ((HappyTerminal (LTok _ (LTokenString happy_var_6))) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTok _ (LTokenName happy_var_3))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTok _ (LTokenName happy_var_1))) `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (IntOutput happy_var_1 happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_21 = happyReduce 6 12 happyReduction_21
happyReduction_21 ((HappyTerminal (LTok _ (LTokenString happy_var_6))) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTok _ (LTokenName happy_var_3))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTok _ (LTokenName happy_var_1))) `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (StrOutput happy_var_1 happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_22 = happyReduce 6 12 happyReduction_22
happyReduction_22 ((HappyTerminal (LTok _ (LTokenString happy_var_6))) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTok _ (LTokenName happy_var_3))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTok _ (LTokenName happy_var_1))) `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (BoolOutput happy_var_1 happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_23 = happySpecReduce_2  12 happyReduction_23
happyReduction_23 _
	(HappyTerminal (LTok _ (LTokenName happy_var_1)))
	 =  HappyAbsSyn12
		 (LabelOutput happy_var_1
	)
happyReduction_23 _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  13 happyReduction_24
happyReduction_24 (HappyAbsSyn14  happy_var_3)
	_
	_
	 =  HappyAbsSyn13
		 (Where happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  14 happyReduction_25
happyReduction_25 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn14
		 (WAnd happy_var_1 happy_var_3
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  14 happyReduction_26
happyReduction_26 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn14
		 (WOr happy_var_1 happy_var_3
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_2  14 happyReduction_27
happyReduction_27 (HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (WNot happy_var_2
	)
happyReduction_27 _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  14 happyReduction_28
happyReduction_28 _
	(HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (happy_var_2
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  14 happyReduction_29
happyReduction_29 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn14
		 (WFinal happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  15 happyReduction_30
happyReduction_30 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn15
		 (WEqual happy_var_1 happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  15 happyReduction_31
happyReduction_31 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn15
		 (WNotEqual happy_var_1 happy_var_3
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  15 happyReduction_32
happyReduction_32 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn15
		 (WLessThan happy_var_1 happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  15 happyReduction_33
happyReduction_33 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn15
		 (WGreaterThan happy_var_1 happy_var_3
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  15 happyReduction_34
happyReduction_34 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn15
		 (WLessOrEqualThan happy_var_1 happy_var_3
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  15 happyReduction_35
happyReduction_35 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn15
		 (WGreaterOrEqualThan happy_var_1 happy_var_3
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  15 happyReduction_36
happyReduction_36 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn15
		 (WStartsWith happy_var_1 happy_var_3
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  15 happyReduction_37
happyReduction_37 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn15
		 (WEqualDot happy_var_1 happy_var_3
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  15 happyReduction_38
happyReduction_38 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn15
		 (WNotEqualDot happy_var_1 happy_var_3
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  15 happyReduction_39
happyReduction_39 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn15
		 (WLessThanDot happy_var_1 happy_var_3
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_3  15 happyReduction_40
happyReduction_40 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn15
		 (WGreaterThanDot happy_var_1 happy_var_3
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_3  15 happyReduction_41
happyReduction_41 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn15
		 (WLessOrEqualThanDot happy_var_1 happy_var_3
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_3  15 happyReduction_42
happyReduction_42 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn15
		 (WGreaterOrEqualThanDot happy_var_1 happy_var_3
	)
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_3  15 happyReduction_43
happyReduction_43 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn15
		 (WStartsWithDot happy_var_1 happy_var_3
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_2  16 happyReduction_44
happyReduction_44 _
	(HappyTerminal (LTok _ (LTokenName happy_var_1)))
	 =  HappyAbsSyn16
		 (WDot happy_var_1 WId
	)
happyReduction_44 _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_2  16 happyReduction_45
happyReduction_45 _
	(HappyTerminal (LTok _ (LTokenName happy_var_1)))
	 =  HappyAbsSyn16
		 (WDot happy_var_1 WType
	)
happyReduction_45 _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_2  16 happyReduction_46
happyReduction_46 _
	(HappyTerminal (LTok _ (LTokenName happy_var_1)))
	 =  HappyAbsSyn16
		 (WDot happy_var_1 WStartField
	)
happyReduction_46 _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_2  16 happyReduction_47
happyReduction_47 _
	(HappyTerminal (LTok _ (LTokenName happy_var_1)))
	 =  HappyAbsSyn16
		 (WDot happy_var_1 WEndField
	)
happyReduction_47 _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_2  16 happyReduction_48
happyReduction_48 _
	(HappyTerminal (LTok _ (LTokenName happy_var_1)))
	 =  HappyAbsSyn16
		 (WDot happy_var_1 WLabelField
	)
happyReduction_48 _ _  = notHappyAtAll 

happyReduce_49 = happyReduce 4 16 happyReduction_49
happyReduction_49 (_ `HappyStk`
	(HappyTerminal (LTok _ (LTokenName happy_var_3))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTok _ (LTokenName happy_var_1))) `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (WDot happy_var_1 (WFieldName happy_var_3)
	) `HappyStk` happyRest

happyReduce_50 = happyReduce 4 16 happyReduction_50
happyReduction_50 (_ `HappyStk`
	(HappyTerminal (LTok _ (LTokenName happy_var_3))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTok _ (LTokenName happy_var_1))) `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (WDot happy_var_1 (WFieldName happy_var_3)
	) `HappyStk` happyRest

happyReduce_51 = happyReduce 4 16 happyReduction_51
happyReduction_51 (_ `HappyStk`
	(HappyTerminal (LTok _ (LTokenName happy_var_3))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTok _ (LTokenName happy_var_1))) `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (WDot happy_var_1 (WFieldName happy_var_3)
	) `HappyStk` happyRest

happyReduce_52 = happySpecReduce_1  17 happyReduction_52
happyReduction_52 (HappyTerminal (LTok _ (LTokenString happy_var_1)))
	 =  HappyAbsSyn17
		 (LiteralStr happy_var_1
	)
happyReduction_52 _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_1  17 happyReduction_53
happyReduction_53 _
	 =  HappyAbsSyn17
		 (LiteralNull
	)

happyReduce_54 = happySpecReduce_1  17 happyReduction_54
happyReduction_54 (HappyTerminal (LTok _ (LTokenInt happy_var_1)))
	 =  HappyAbsSyn17
		 (LiteralInt happy_var_1
	)
happyReduction_54 _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_1  17 happyReduction_55
happyReduction_55 _
	 =  HappyAbsSyn17
		 (LiteralBool True
	)

happyReduce_56 = happySpecReduce_1  17 happyReduction_56
happyReduction_56 _
	 =  HappyAbsSyn17
		 (LiteralBool False
	)

happyNewToken action sts stk [] =
	action 57 57 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	LTok _ LTokenFullStop -> cont 18;
	LTok _ LTokenComma -> cont 19;
	LTok _ LTokenBar -> cont 20;
	LTok _ LTokenAssignment -> cont 21;
	LTok _ LTokenRelated -> cont 22;
	LTok _ LTokenRelatedRight -> cont 23;
	LTok _ LTokenRelatedLeft -> cont 24;
	LTok _ LTokenIntField -> cont 25;
	LTok _ LTokenStrField -> cont 26;
	LTok _ LTokenBoolField -> cont 27;
	LTok _ LTokenLabelField -> cont 28;
	LTok _ LTokenIdField -> cont 29;
	LTok _ LTokenStartField -> cont 30;
	LTok _ LTokenEndField -> cont 31;
	LTok _ LTokenTypeField -> cont 32;
	LTok _ LTokenOr -> cont 33;
	LTok _ LTokenAnd -> cont 34;
	LTok _ LTokenNot -> cont 35;
	LTok _ LTokenRead -> cont 36;
	LTok _ LTokenMatch -> cont 37;
	LTok _ LTokenWhere -> cont 38;
	LTok _ LTokenGetNode -> cont 39;
	LTok _ LTokenGetRelation -> cont 40;
	LTok _ LTokenAs -> cont 41;
	LTok _ LTokenStartWith -> cont 42;
	LTok _ LTokenLParen -> cont 43;
	LTok _ LTokenRParen -> cont 44;
	LTok _ LTokenLessThanEqual -> cont 45;
	LTok _ LTokenGreaterThanEqual -> cont 46;
	LTok _ LTokenLessThan -> cont 47;
	LTok _ LTokenGreaterThan -> cont 48;
	LTok _ LTokenEquals -> cont 49;
	LTok _ LTokenNotEquals -> cont 50;
	LTok _ LTokenNull -> cont 51;
	LTok _ LTokenTrue -> cont 52;
	LTok _ LTokenFalse -> cont 53;
	LTok _ (LTokenString happy_dollar_dollar) -> cont 54;
	LTok _ (LTokenInt happy_dollar_dollar) -> cont 55;
	LTok _ (LTokenName happy_dollar_dollar) -> cont 56;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 57 tk tks = happyError' (tks, explist)
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
happyError' :: () => ([(LangToken)], [Prelude.String]) -> HappyIdentity a
happyError' = HappyIdentity Prelude.. (\(tokens, _) -> parseError tokens)
langParser tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [LangToken] -> a
parseError [] = error "uknown error"
parseError ts = error $ "Parse error at line " ++ (show ln) ++ " column " ++ (show col) ++ " previous character " ++ (show ch) ++ " tokens " ++ (show ts)
    where LTok p cl = head ts
          AlexPn ch ln col = p

data Query 
    = Query ReadFile Match
    deriving (Eq, Show)

data ReadFile
    = ReadFile String
    deriving (Show, Eq)

data Match
    = Match Patterns Where Return
    deriving (Eq, Show)

type Patterns
    = [Pattern]

data Pattern 
    = PatternFinal String
    | PatternRelated String String
    | PatternRelatedVar String String String
    | PatternRelatedTo String String
    | PatternRelatedToVar String String String
    | PatternRelatedBy String String
    | PatternRelatedByVar String String String
    deriving (Eq, Show)


data Where
    = Where WhereExp
    deriving (Show, Eq)

data WhereExp
    = WAnd WhereFunc WhereExp 
    | WOr WhereFunc WhereExp 
    | WNot WhereExp
    | WFinal WhereFunc
    deriving (Show, Eq)

data WhereFunc
    = WEqual WhereDot Literal
    | WNotEqual WhereDot Literal
    | WLessThan WhereDot Literal
    | WGreaterThan WhereDot Literal
    | WLessOrEqualThan WhereDot Literal
    | WGreaterOrEqualThan WhereDot Literal
    | WStartsWith WhereDot Literal
    
    | WEqualDot WhereDot WhereDot
    | WNotEqualDot WhereDot WhereDot
    | WLessThanDot WhereDot WhereDot
    | WGreaterThanDot WhereDot WhereDot
    | WLessOrEqualThanDot WhereDot WhereDot
    | WGreaterOrEqualThanDot WhereDot WhereDot
    | WStartsWithDot WhereDot WhereDot
    deriving (Show, Eq)

data WhereDot
    = WDot String WhereDotOptions
    deriving (Show, Eq)

data WhereDotOptions
    = WFieldName String
    | WId
    | WLabelField
    | WStartField
    | WEndField
    | WType
    deriving (Show, Eq)

data WhereLit
    = WStr String
    | WInt Int
    | WBool Bool
    | WNull
    deriving (Show, Eq)

data Return
    = ReturnNode [Outputs]
    | ReturnNodeRelation [Outputs] [Outputs]
    | ReturnRelation [Outputs]
    deriving (Eq, Show)

type Outputs
    = [Output]

data Output
    = StrOutput String String String 
    | IntOutput String String String 
    | BoolOutput String String String 
    -- | IdOutput String
    -- | StartOutput String
    -- | EndOutput String
    | LabelOutput String
    deriving (Eq, Show)
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
