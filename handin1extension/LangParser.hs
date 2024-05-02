{-# OPTIONS_GHC -w #-}
module LangParser where
import LangLexer
import InputParser (Literal(..))
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28
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
	| HappyAbsSyn18 t18
	| HappyAbsSyn19 t19
	| HappyAbsSyn20 t20
	| HappyAbsSyn21 t21
	| HappyAbsSyn22 t22
	| HappyAbsSyn23 t23
	| HappyAbsSyn24 t24
	| HappyAbsSyn25 t25
	| HappyAbsSyn26 t26
	| HappyAbsSyn27 t27
	| HappyAbsSyn28 t28

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,249) ([0,16384,0,0,0,0,64,0,0,0,32768,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,4096,0,0,8,0,0,0,16,0,0,0,0,0,0,64,0,0,15360,0,0,4096,0,0,0,0,0,0,0,0,0,1024,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,28,0,0,0,3072,0,0,4096,0,0,0,0,16,0,0,0,4096,0,0,0,0,16,0,0,0,0,0,0,0,0,0,32768,0,0,0,5,0,0,0,0,0,16384,0,0,0,0,64,0,0,0,16384,0,0,0,0,64,0,0,0,16384,0,0,0,0,64,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,12,0,0,12288,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1011,0,0,0,1024,0,64,0,32,0,496,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,48,0,0,0,0,8,0,0,0,0,0,126,0,0,0,32256,0,0,0,0,126,0,0,0,32256,0,0,0,0,126,0,0,0,32256,0,0,0,0,126,0,0,0,32256,0,0,1024,0,64,0,0,4,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,64,0,0,0,0,8,0,8192,0,61440,1,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,8192,0,61440,1,0,0,0,0,0,0,0,128,0,0,32,0,0,0,0,0,0,0,0,256,0,0,0,0,2,0,0,0,0,0,16384,0,0,0,0,64,0,0,0,16384,0,0,0,0,64,0,0,0,16384,0,0,0,0,16,0,0,0,4096,0,0,0,0,16,0,0,0,4096,0,0,0,0,16,0,0,0,16384,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,16384,0,0,0,0,64,0,12288,0,0,0,0,48,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,62,0,0,0,0,0,0,0,1536,0,0,0,0,0,0,0,0,0,0,0,0,0,24576,0,0,0,0,96,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,16384,0,0,0,0,0,0,0,0,496,0,0,0,0,0,0,16,0,0,0,8192,0,0,0,0,16,0,0,0,8192,0,0,0,0,0,0,16384,0,0,0,0,64,0,0,0,16384,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,16,0,0,0,8192,0,0,0,0,16,0,0,0,0,0,0,64,0,0,0,16384,0,0,0,0,64,0,0,0,16384,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,64,0,0,0,16384,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_langParser","Query","Read","Match","Patterns","Pattern","Where","WhereExp1","WhereExp2","WhereExp3","WhereFunc","WhereDot","WhereLit","Print","Return","Append","Print1","PrintExps","PrintExp","PDot","Update","Update1","UpdateExp","Delete","Delete1","DeleteExp","\"=\"","\".\"","read","match","\"-\"","\"->\"","\"<-\"","where","or","and","not","is","starts","ends","\"(\"","\")\"","\"<=\"","\">=\"","\"<\"","\">\"","\"==\"","\"/=\"","return","append","delete","update","\",\"","\"|\"","as","add","minus","\"--\"","labelField","idField","startField","endField","typeField","null","true","false","string","int","name","%eof"]
        bit_start = st Prelude.* 72
        bit_end = (st Prelude.+ 1) Prelude.* 72
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..71]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (31) = happyShift action_3
action_0 (4) = happyGoto action_4
action_0 (5) = happyGoto action_2
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (31) = happyShift action_3
action_1 (5) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (32) = happyShift action_7
action_2 (6) = happyGoto action_6
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (29) = happyShift action_5
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (72) = happyAccept
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (69) = happyShift action_11
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (36) = happyShift action_10
action_6 (9) = happyGoto action_9
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (29) = happyShift action_8
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (71) = happyShift action_24
action_8 (7) = happyGoto action_22
action_8 (8) = happyGoto action_23
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (51) = happyShift action_18
action_9 (52) = happyShift action_19
action_9 (53) = happyShift action_20
action_9 (54) = happyShift action_21
action_9 (16) = happyGoto action_13
action_9 (17) = happyGoto action_14
action_9 (18) = happyGoto action_15
action_9 (23) = happyGoto action_16
action_9 (26) = happyGoto action_17
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (29) = happyShift action_12
action_10 _ = happyFail (happyExpListPerState 10)

action_11 _ = happyReduce_2

action_12 (43) = happyShift action_42
action_12 (71) = happyShift action_43
action_12 (10) = happyGoto action_37
action_12 (11) = happyGoto action_38
action_12 (12) = happyGoto action_39
action_12 (13) = happyGoto action_40
action_12 (14) = happyGoto action_41
action_12 _ = happyFail (happyExpListPerState 12)

action_13 _ = happyReduce_1

action_14 _ = happyReduce_49

action_15 _ = happyReduce_50

action_16 (51) = happyShift action_18
action_16 (52) = happyShift action_19
action_16 (53) = happyShift action_20
action_16 (17) = happyGoto action_34
action_16 (18) = happyGoto action_35
action_16 (26) = happyGoto action_36
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (51) = happyShift action_18
action_17 (52) = happyShift action_19
action_17 (17) = happyGoto action_32
action_17 (18) = happyGoto action_33
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (29) = happyShift action_31
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (29) = happyShift action_30
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (29) = happyShift action_29
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (29) = happyShift action_28
action_21 _ = happyFail (happyExpListPerState 21)

action_22 _ = happyReduce_3

action_23 (56) = happyShift action_27
action_23 _ = happyReduce_5

action_24 (33) = happyShift action_25
action_24 (35) = happyShift action_26
action_24 _ = happyReduce_8

action_25 (71) = happyShift action_78
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (71) = happyShift action_77
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (71) = happyShift action_24
action_27 (7) = happyGoto action_76
action_27 (8) = happyGoto action_23
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (71) = happyShift action_75
action_28 (24) = happyGoto action_73
action_28 (25) = happyGoto action_74
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (71) = happyShift action_72
action_29 (27) = happyGoto action_70
action_29 (28) = happyGoto action_71
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (71) = happyShift action_68
action_30 (19) = happyGoto action_69
action_30 (20) = happyGoto action_65
action_30 (21) = happyGoto action_66
action_30 (22) = happyGoto action_67
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (71) = happyShift action_68
action_31 (19) = happyGoto action_64
action_31 (20) = happyGoto action_65
action_31 (21) = happyGoto action_66
action_31 (22) = happyGoto action_67
action_31 _ = happyFail (happyExpListPerState 31)

action_32 _ = happyReduce_47

action_33 _ = happyReduce_48

action_34 _ = happyReduce_45

action_35 _ = happyReduce_46

action_36 (51) = happyShift action_18
action_36 (52) = happyShift action_19
action_36 (17) = happyGoto action_62
action_36 (18) = happyGoto action_63
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (37) = happyShift action_60
action_37 (38) = happyShift action_61
action_37 _ = happyReduce_9

action_38 _ = happyReduce_11

action_39 _ = happyReduce_13

action_40 _ = happyReduce_15

action_41 (41) = happyShift action_52
action_41 (42) = happyShift action_53
action_41 (45) = happyShift action_54
action_41 (46) = happyShift action_55
action_41 (47) = happyShift action_56
action_41 (48) = happyShift action_57
action_41 (49) = happyShift action_58
action_41 (50) = happyShift action_59
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (43) = happyShift action_42
action_42 (71) = happyShift action_43
action_42 (10) = happyGoto action_50
action_42 (11) = happyGoto action_51
action_42 (12) = happyGoto action_39
action_42 (13) = happyGoto action_40
action_42 (14) = happyGoto action_41
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (30) = happyShift action_44
action_43 (61) = happyShift action_45
action_43 (62) = happyShift action_46
action_43 (63) = happyShift action_47
action_43 (64) = happyShift action_48
action_43 (65) = happyShift action_49
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (71) = happyShift action_123
action_44 _ = happyFail (happyExpListPerState 44)

action_45 _ = happyReduce_36

action_46 _ = happyReduce_32

action_47 _ = happyReduce_34

action_48 _ = happyReduce_35

action_49 _ = happyReduce_33

action_50 (37) = happyShift action_60
action_50 (38) = happyShift action_61
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (44) = happyShift action_122
action_51 _ = happyReduce_11

action_52 (66) = happyShift action_103
action_52 (67) = happyShift action_104
action_52 (68) = happyShift action_105
action_52 (69) = happyShift action_106
action_52 (70) = happyShift action_107
action_52 (71) = happyShift action_43
action_52 (14) = happyGoto action_120
action_52 (15) = happyGoto action_121
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (66) = happyShift action_103
action_53 (67) = happyShift action_104
action_53 (68) = happyShift action_105
action_53 (69) = happyShift action_106
action_53 (70) = happyShift action_107
action_53 (71) = happyShift action_43
action_53 (14) = happyGoto action_118
action_53 (15) = happyGoto action_119
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (66) = happyShift action_103
action_54 (67) = happyShift action_104
action_54 (68) = happyShift action_105
action_54 (69) = happyShift action_106
action_54 (70) = happyShift action_107
action_54 (71) = happyShift action_43
action_54 (14) = happyGoto action_116
action_54 (15) = happyGoto action_117
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (66) = happyShift action_103
action_55 (67) = happyShift action_104
action_55 (68) = happyShift action_105
action_55 (69) = happyShift action_106
action_55 (70) = happyShift action_107
action_55 (71) = happyShift action_43
action_55 (14) = happyGoto action_114
action_55 (15) = happyGoto action_115
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (66) = happyShift action_103
action_56 (67) = happyShift action_104
action_56 (68) = happyShift action_105
action_56 (69) = happyShift action_106
action_56 (70) = happyShift action_107
action_56 (71) = happyShift action_43
action_56 (14) = happyGoto action_112
action_56 (15) = happyGoto action_113
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (66) = happyShift action_103
action_57 (67) = happyShift action_104
action_57 (68) = happyShift action_105
action_57 (69) = happyShift action_106
action_57 (70) = happyShift action_107
action_57 (71) = happyShift action_43
action_57 (14) = happyGoto action_110
action_57 (15) = happyGoto action_111
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (66) = happyShift action_103
action_58 (67) = happyShift action_104
action_58 (68) = happyShift action_105
action_58 (69) = happyShift action_106
action_58 (70) = happyShift action_107
action_58 (71) = happyShift action_43
action_58 (14) = happyGoto action_108
action_58 (15) = happyGoto action_109
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (66) = happyShift action_103
action_59 (67) = happyShift action_104
action_59 (68) = happyShift action_105
action_59 (69) = happyShift action_106
action_59 (70) = happyShift action_107
action_59 (71) = happyShift action_43
action_59 (14) = happyGoto action_101
action_59 (15) = happyGoto action_102
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (43) = happyShift action_42
action_60 (71) = happyShift action_43
action_60 (10) = happyGoto action_100
action_60 (11) = happyGoto action_38
action_60 (12) = happyGoto action_39
action_60 (13) = happyGoto action_40
action_60 (14) = happyGoto action_41
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (43) = happyShift action_42
action_61 (71) = happyShift action_43
action_61 (10) = happyGoto action_99
action_61 (11) = happyGoto action_38
action_61 (12) = happyGoto action_39
action_61 (13) = happyGoto action_40
action_61 (14) = happyGoto action_41
action_61 _ = happyFail (happyExpListPerState 61)

action_62 _ = happyReduce_43

action_63 _ = happyReduce_44

action_64 _ = happyReduce_51

action_65 (56) = happyShift action_98
action_65 _ = happyReduce_54

action_66 (55) = happyShift action_97
action_66 _ = happyReduce_56

action_67 (60) = happyShift action_96
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (30) = happyShift action_90
action_68 (61) = happyShift action_91
action_68 (62) = happyShift action_92
action_68 (63) = happyShift action_93
action_68 (64) = happyShift action_94
action_68 (65) = happyShift action_95
action_68 _ = happyFail (happyExpListPerState 68)

action_69 _ = happyReduce_52

action_70 _ = happyReduce_76

action_71 (56) = happyShift action_89
action_71 _ = happyReduce_78

action_72 (30) = happyShift action_83
action_72 (61) = happyShift action_84
action_72 (62) = happyShift action_85
action_72 (63) = happyShift action_86
action_72 (64) = happyShift action_87
action_72 (65) = happyShift action_88
action_72 _ = happyFail (happyExpListPerState 72)

action_73 _ = happyReduce_69

action_74 (56) = happyShift action_82
action_74 _ = happyReduce_71

action_75 (30) = happyShift action_81
action_75 _ = happyFail (happyExpListPerState 75)

action_76 _ = happyReduce_4

action_77 (33) = happyShift action_80
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (34) = happyShift action_79
action_78 _ = happyFail (happyExpListPerState 78)

action_79 (71) = happyShift action_138
action_79 _ = happyFail (happyExpListPerState 79)

action_80 (71) = happyShift action_137
action_80 _ = happyFail (happyExpListPerState 80)

action_81 (71) = happyShift action_136
action_81 _ = happyFail (happyExpListPerState 81)

action_82 (71) = happyShift action_75
action_82 (24) = happyGoto action_135
action_82 (25) = happyGoto action_74
action_82 _ = happyFail (happyExpListPerState 82)

action_83 (71) = happyShift action_134
action_83 _ = happyFail (happyExpListPerState 83)

action_84 (69) = happyShift action_133
action_84 _ = happyFail (happyExpListPerState 84)

action_85 (69) = happyShift action_132
action_85 _ = happyFail (happyExpListPerState 85)

action_86 (69) = happyShift action_131
action_86 _ = happyFail (happyExpListPerState 86)

action_87 (69) = happyShift action_130
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (69) = happyShift action_129
action_88 _ = happyFail (happyExpListPerState 88)

action_89 (71) = happyShift action_72
action_89 (27) = happyGoto action_128
action_89 (28) = happyGoto action_71
action_89 _ = happyFail (happyExpListPerState 89)

action_90 (71) = happyShift action_127
action_90 _ = happyFail (happyExpListPerState 90)

action_91 (60) = happyReduce_64
action_91 _ = happyReduce_58

action_92 (60) = happyReduce_68
action_92 _ = happyReduce_62

action_93 (60) = happyReduce_66
action_93 _ = happyReduce_60

action_94 (60) = happyReduce_67
action_94 _ = happyReduce_61

action_95 (60) = happyReduce_65
action_95 _ = happyReduce_59

action_96 (69) = happyShift action_126
action_96 _ = happyFail (happyExpListPerState 96)

action_97 (71) = happyShift action_68
action_97 (20) = happyGoto action_125
action_97 (21) = happyGoto action_66
action_97 (22) = happyGoto action_67
action_97 _ = happyFail (happyExpListPerState 97)

action_98 (71) = happyShift action_68
action_98 (19) = happyGoto action_124
action_98 (20) = happyGoto action_65
action_98 (21) = happyGoto action_66
action_98 (22) = happyGoto action_67
action_98 _ = happyFail (happyExpListPerState 98)

action_99 (37) = happyShift action_60
action_99 (38) = happyShift action_61
action_99 _ = happyReduce_12

action_100 (37) = happyShift action_60
action_100 (38) = happyShift action_61
action_100 _ = happyReduce_14

action_101 _ = happyReduce_25

action_102 _ = happyReduce_17

action_103 _ = happyReduce_42

action_104 _ = happyReduce_40

action_105 _ = happyReduce_41

action_106 _ = happyReduce_38

action_107 _ = happyReduce_39

action_108 _ = happyReduce_24

action_109 _ = happyReduce_16

action_110 _ = happyReduce_27

action_111 _ = happyReduce_19

action_112 _ = happyReduce_26

action_113 _ = happyReduce_18

action_114 _ = happyReduce_29

action_115 _ = happyReduce_21

action_116 _ = happyReduce_28

action_117 _ = happyReduce_20

action_118 _ = happyReduce_31

action_119 _ = happyReduce_23

action_120 _ = happyReduce_30

action_121 _ = happyReduce_22

action_122 _ = happyReduce_10

action_123 _ = happyReduce_37

action_124 _ = happyReduce_53

action_125 _ = happyReduce_55

action_126 (60) = happyShift action_147
action_126 _ = happyFail (happyExpListPerState 126)

action_127 (57) = happyShift action_146
action_127 _ = happyFail (happyExpListPerState 127)

action_128 _ = happyReduce_77

action_129 _ = happyReduce_85

action_130 _ = happyReduce_87

action_131 _ = happyReduce_86

action_132 _ = happyReduce_84

action_133 _ = happyReduce_88

action_134 (66) = happyShift action_141
action_134 (67) = happyShift action_142
action_134 (68) = happyShift action_143
action_134 (69) = happyShift action_144
action_134 (70) = happyShift action_145
action_134 _ = happyFail (happyExpListPerState 134)

action_135 _ = happyReduce_70

action_136 (58) = happyShift action_139
action_136 (59) = happyShift action_140
action_136 _ = happyFail (happyExpListPerState 136)

action_137 _ = happyReduce_7

action_138 _ = happyReduce_6

action_139 (70) = happyShift action_153
action_139 (71) = happyShift action_154
action_139 _ = happyFail (happyExpListPerState 139)

action_140 (70) = happyShift action_151
action_140 (71) = happyShift action_152
action_140 _ = happyFail (happyExpListPerState 140)

action_141 _ = happyReduce_83

action_142 _ = happyReduce_81

action_143 _ = happyReduce_82

action_144 _ = happyReduce_80

action_145 _ = happyReduce_79

action_146 (69) = happyShift action_150
action_146 _ = happyFail (happyExpListPerState 146)

action_147 (71) = happyShift action_149
action_147 (22) = happyGoto action_148
action_147 _ = happyFail (happyExpListPerState 147)

action_148 _ = happyReduce_63

action_149 (61) = happyShift action_159
action_149 (62) = happyShift action_160
action_149 (63) = happyShift action_161
action_149 (64) = happyShift action_162
action_149 (65) = happyShift action_163
action_149 _ = happyFail (happyExpListPerState 149)

action_150 _ = happyReduce_57

action_151 (29) = happyShift action_158
action_151 _ = happyFail (happyExpListPerState 151)

action_152 (30) = happyShift action_157
action_152 _ = happyFail (happyExpListPerState 152)

action_153 (29) = happyShift action_156
action_153 _ = happyFail (happyExpListPerState 153)

action_154 (30) = happyShift action_155
action_154 _ = happyFail (happyExpListPerState 154)

action_155 (71) = happyShift action_167
action_155 _ = happyFail (happyExpListPerState 155)

action_156 (71) = happyShift action_166
action_156 _ = happyFail (happyExpListPerState 156)

action_157 (71) = happyShift action_165
action_157 _ = happyFail (happyExpListPerState 157)

action_158 (71) = happyShift action_164
action_158 _ = happyFail (happyExpListPerState 158)

action_159 _ = happyReduce_64

action_160 _ = happyReduce_68

action_161 _ = happyReduce_66

action_162 _ = happyReduce_67

action_163 _ = happyReduce_65

action_164 (30) = happyShift action_171
action_164 _ = happyFail (happyExpListPerState 164)

action_165 (29) = happyShift action_170
action_165 _ = happyFail (happyExpListPerState 165)

action_166 (30) = happyShift action_169
action_166 _ = happyFail (happyExpListPerState 166)

action_167 (29) = happyShift action_168
action_167 _ = happyFail (happyExpListPerState 167)

action_168 (71) = happyShift action_175
action_168 _ = happyFail (happyExpListPerState 168)

action_169 (71) = happyShift action_174
action_169 _ = happyFail (happyExpListPerState 169)

action_170 (71) = happyShift action_173
action_170 _ = happyFail (happyExpListPerState 170)

action_171 (71) = happyShift action_172
action_171 _ = happyFail (happyExpListPerState 171)

action_172 _ = happyReduce_74

action_173 (30) = happyShift action_177
action_173 _ = happyFail (happyExpListPerState 173)

action_174 _ = happyReduce_72

action_175 (30) = happyShift action_176
action_175 _ = happyFail (happyExpListPerState 175)

action_176 (71) = happyShift action_179
action_176 _ = happyFail (happyExpListPerState 176)

action_177 (71) = happyShift action_178
action_177 _ = happyFail (happyExpListPerState 177)

action_178 _ = happyReduce_75

action_179 _ = happyReduce_73

happyReduce_1 = happyReduce 4 4 happyReduction_1
happyReduction_1 ((HappyAbsSyn16  happy_var_4) `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	(HappyAbsSyn5  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Query happy_var_1 happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_2 = happySpecReduce_3  5 happyReduction_2
happyReduction_2 (HappyTerminal (LTok _ (LTokenString happy_var_3)))
	_
	_
	 =  HappyAbsSyn5
		 (ReadFile happy_var_3
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_3  6 happyReduction_3
happyReduction_3 (HappyAbsSyn7  happy_var_3)
	_
	_
	 =  HappyAbsSyn6
		 (Match happy_var_3
	)
happyReduction_3 _ _ _  = notHappyAtAll 

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

happyReduce_6 = happyReduce 5 8 happyReduction_6
happyReduction_6 ((HappyTerminal (LTok _ (LTokenName happy_var_5))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTok _ (LTokenName happy_var_3))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTok _ (LTokenName happy_var_1))) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (PatternRelatedTo happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_7 = happyReduce 5 8 happyReduction_7
happyReduction_7 ((HappyTerminal (LTok _ (LTokenName happy_var_5))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTok _ (LTokenName happy_var_3))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTok _ (LTokenName happy_var_1))) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (PatternRelatedBy happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_8 = happySpecReduce_1  8 happyReduction_8
happyReduction_8 (HappyTerminal (LTok _ (LTokenName happy_var_1)))
	 =  HappyAbsSyn8
		 (Pattern happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  9 happyReduction_9
happyReduction_9 (HappyAbsSyn10  happy_var_3)
	_
	_
	 =  HappyAbsSyn9
		 (Where happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  10 happyReduction_10
happyReduction_10 _
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (happy_var_2
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  10 happyReduction_11
happyReduction_11 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  11 happyReduction_12
happyReduction_12 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn11
		 (WAnd happy_var_1 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  11 happyReduction_13
happyReduction_13 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  12 happyReduction_14
happyReduction_14 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn12
		 (WOr happy_var_1 happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  12 happyReduction_15
happyReduction_15 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  13 happyReduction_16
happyReduction_16 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (WEqual happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  13 happyReduction_17
happyReduction_17 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (WNotEqual happy_var_1 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  13 happyReduction_18
happyReduction_18 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (WLessThan happy_var_1 happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  13 happyReduction_19
happyReduction_19 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (WGreaterThan happy_var_1 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  13 happyReduction_20
happyReduction_20 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (WLessOrEqualThan happy_var_1 happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  13 happyReduction_21
happyReduction_21 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (WGreaterOrEqualThan happy_var_1 happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  13 happyReduction_22
happyReduction_22 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (WStartsWith happy_var_1 happy_var_3
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  13 happyReduction_23
happyReduction_23 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (WEndsWith happy_var_1 happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  13 happyReduction_24
happyReduction_24 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (WEqualDot happy_var_1 happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  13 happyReduction_25
happyReduction_25 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (WNotEqualDot happy_var_1 happy_var_3
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  13 happyReduction_26
happyReduction_26 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (WLessThanDot happy_var_1 happy_var_3
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  13 happyReduction_27
happyReduction_27 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (WGreaterThanDot happy_var_1 happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  13 happyReduction_28
happyReduction_28 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (WLessOrEqualThanDot happy_var_1 happy_var_3
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  13 happyReduction_29
happyReduction_29 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (WGreaterOrEqualThanDot happy_var_1 happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  13 happyReduction_30
happyReduction_30 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (WStartsWithDot happy_var_1 happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  13 happyReduction_31
happyReduction_31 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (WEndsWithDot happy_var_1 happy_var_3
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_2  14 happyReduction_32
happyReduction_32 _
	(HappyTerminal (LTok _ (LTokenName happy_var_1)))
	 =  HappyAbsSyn14
		 (WDot happy_var_1 ":ID"
	)
happyReduction_32 _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_2  14 happyReduction_33
happyReduction_33 _
	(HappyTerminal (LTok _ (LTokenName happy_var_1)))
	 =  HappyAbsSyn14
		 (WDot happy_var_1 ":TYPE"
	)
happyReduction_33 _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_2  14 happyReduction_34
happyReduction_34 _
	(HappyTerminal (LTok _ (LTokenName happy_var_1)))
	 =  HappyAbsSyn14
		 (WDot happy_var_1 ":START_ID"
	)
happyReduction_34 _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_2  14 happyReduction_35
happyReduction_35 _
	(HappyTerminal (LTok _ (LTokenName happy_var_1)))
	 =  HappyAbsSyn14
		 (WDot happy_var_1 ":END_ID"
	)
happyReduction_35 _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_2  14 happyReduction_36
happyReduction_36 _
	(HappyTerminal (LTok _ (LTokenName happy_var_1)))
	 =  HappyAbsSyn14
		 (WDot happy_var_1 ":LABEL"
	)
happyReduction_36 _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  14 happyReduction_37
happyReduction_37 (HappyTerminal (LTok _ (LTokenName happy_var_3)))
	_
	(HappyTerminal (LTok _ (LTokenName happy_var_1)))
	 =  HappyAbsSyn14
		 (WDot happy_var_1 happy_var_3
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  15 happyReduction_38
happyReduction_38 (HappyTerminal (LTok _ (LTokenString happy_var_1)))
	 =  HappyAbsSyn15
		 (WStr happy_var_1
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_1  15 happyReduction_39
happyReduction_39 (HappyTerminal (LTok _ (LTokenInt happy_var_1)))
	 =  HappyAbsSyn15
		 (WInt happy_var_1
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  15 happyReduction_40
happyReduction_40 _
	 =  HappyAbsSyn15
		 (WBool True
	)

happyReduce_41 = happySpecReduce_1  15 happyReduction_41
happyReduction_41 _
	 =  HappyAbsSyn15
		 (WBool False
	)

happyReduce_42 = happySpecReduce_1  15 happyReduction_42
happyReduction_42 _
	 =  HappyAbsSyn15
		 (WNull
	)

happyReduce_43 = happySpecReduce_3  16 happyReduction_43
happyReduction_43 (HappyAbsSyn17  happy_var_3)
	(HappyAbsSyn26  happy_var_2)
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn16
		 (Print1 happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_3  16 happyReduction_44
happyReduction_44 (HappyAbsSyn18  happy_var_3)
	(HappyAbsSyn26  happy_var_2)
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn16
		 (Print2 happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_2  16 happyReduction_45
happyReduction_45 (HappyAbsSyn17  happy_var_2)
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn16
		 (Print3 happy_var_1 happy_var_2
	)
happyReduction_45 _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_2  16 happyReduction_46
happyReduction_46 (HappyAbsSyn18  happy_var_2)
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn16
		 (Print4 happy_var_1 happy_var_2
	)
happyReduction_46 _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_2  16 happyReduction_47
happyReduction_47 (HappyAbsSyn17  happy_var_2)
	(HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn16
		 (Print5 happy_var_1 happy_var_2
	)
happyReduction_47 _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_2  16 happyReduction_48
happyReduction_48 (HappyAbsSyn18  happy_var_2)
	(HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn16
		 (Print6 happy_var_1 happy_var_2
	)
happyReduction_48 _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_1  16 happyReduction_49
happyReduction_49 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn16
		 (Print7 happy_var_1
	)
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_1  16 happyReduction_50
happyReduction_50 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn16
		 (Print8 happy_var_1
	)
happyReduction_50 _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_3  17 happyReduction_51
happyReduction_51 (HappyAbsSyn19  happy_var_3)
	_
	_
	 =  HappyAbsSyn17
		 (Return happy_var_3
	)
happyReduction_51 _ _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_3  18 happyReduction_52
happyReduction_52 (HappyAbsSyn19  happy_var_3)
	_
	_
	 =  HappyAbsSyn18
		 (Append happy_var_3
	)
happyReduction_52 _ _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_3  19 happyReduction_53
happyReduction_53 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1 : happy_var_3
	)
happyReduction_53 _ _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_1  19 happyReduction_54
happyReduction_54 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn19
		 ([happy_var_1]
	)
happyReduction_54 _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_3  20 happyReduction_55
happyReduction_55 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1 : happy_var_3
	)
happyReduction_55 _ _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_1  20 happyReduction_56
happyReduction_56 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn20
		 ([happy_var_1]
	)
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happyReduce 5 21 happyReduction_57
happyReduction_57 ((HappyTerminal (LTok _ (LTokenString happy_var_5))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTok _ (LTokenName happy_var_3))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTok _ (LTokenName happy_var_1))) `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 (Output happy_var_1 happy_var_3          happy_var_5
	) `HappyStk` happyRest

happyReduce_58 = happySpecReduce_2  21 happyReduction_58
happyReduction_58 _
	(HappyTerminal (LTok _ (LTokenName happy_var_1)))
	 =  HappyAbsSyn21
		 (Output happy_var_1 ":LABEL"    ":LABEL"
	)
happyReduction_58 _ _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_2  21 happyReduction_59
happyReduction_59 _
	(HappyTerminal (LTok _ (LTokenName happy_var_1)))
	 =  HappyAbsSyn21
		 (Output happy_var_1 ":TYPE"     ":TYPE"
	)
happyReduction_59 _ _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_2  21 happyReduction_60
happyReduction_60 _
	(HappyTerminal (LTok _ (LTokenName happy_var_1)))
	 =  HappyAbsSyn21
		 (Output happy_var_1 ":START_ID" ":START_ID"
	)
happyReduction_60 _ _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_2  21 happyReduction_61
happyReduction_61 _
	(HappyTerminal (LTok _ (LTokenName happy_var_1)))
	 =  HappyAbsSyn21
		 (Output happy_var_1 ":END_ID"   ":END_ID"
	)
happyReduction_61 _ _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_2  21 happyReduction_62
happyReduction_62 _
	(HappyTerminal (LTok _ (LTokenName happy_var_1)))
	 =  HappyAbsSyn21
		 (Output happy_var_1 ":ID"        ":ID"
	)
happyReduction_62 _ _  = notHappyAtAll 

happyReduce_63 = happyReduce 5 21 happyReduction_63
happyReduction_63 ((HappyAbsSyn22  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTok _ (LTokenString happy_var_3))) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn22  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 (NewRelation (fst happy_var_1) (snd happy_var_1) (fst happy_var_5) (snd happy_var_5) happy_var_3
	) `HappyStk` happyRest

happyReduce_64 = happySpecReduce_2  22 happyReduction_64
happyReduction_64 _
	(HappyTerminal (LTok _ (LTokenName happy_var_1)))
	 =  HappyAbsSyn22
		 ((happy_var_1,":LABEL")
	)
happyReduction_64 _ _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_2  22 happyReduction_65
happyReduction_65 _
	(HappyTerminal (LTok _ (LTokenName happy_var_1)))
	 =  HappyAbsSyn22
		 ((happy_var_1,":TYPE")
	)
happyReduction_65 _ _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_2  22 happyReduction_66
happyReduction_66 _
	(HappyTerminal (LTok _ (LTokenName happy_var_1)))
	 =  HappyAbsSyn22
		 ((happy_var_1,":START_ID")
	)
happyReduction_66 _ _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_2  22 happyReduction_67
happyReduction_67 _
	(HappyTerminal (LTok _ (LTokenName happy_var_1)))
	 =  HappyAbsSyn22
		 ((happy_var_1,":END_ID")
	)
happyReduction_67 _ _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_2  22 happyReduction_68
happyReduction_68 _
	(HappyTerminal (LTok _ (LTokenName happy_var_1)))
	 =  HappyAbsSyn22
		 ((happy_var_1,":ID")
	)
happyReduction_68 _ _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_3  23 happyReduction_69
happyReduction_69 (HappyAbsSyn24  happy_var_3)
	_
	_
	 =  HappyAbsSyn23
		 (Update happy_var_3
	)
happyReduction_69 _ _ _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_3  24 happyReduction_70
happyReduction_70 (HappyAbsSyn24  happy_var_3)
	_
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1 : happy_var_3
	)
happyReduction_70 _ _ _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_1  24 happyReduction_71
happyReduction_71 (HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn24
		 ([happy_var_1]
	)
happyReduction_71 _  = notHappyAtAll 

happyReduce_72 = happyReduce 9 25 happyReduction_72
happyReduction_72 ((HappyTerminal (LTok _ (LTokenName happy_var_9))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTok _ (LTokenName happy_var_7))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTok _ (LTokenInt happy_var_5))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTok _ (LTokenName happy_var_3))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTok _ (LTokenName happy_var_1))) `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (UAdd      happy_var_1 happy_var_3 happy_var_5 happy_var_7 happy_var_9
	) `HappyStk` happyRest

happyReduce_73 = happyReduce 11 25 happyReduction_73
happyReduction_73 ((HappyTerminal (LTok _ (LTokenName happy_var_11))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTok _ (LTokenName happy_var_9))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTok _ (LTokenName happy_var_7))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTok _ (LTokenName happy_var_5))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTok _ (LTokenName happy_var_3))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTok _ (LTokenName happy_var_1))) `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (UAddDot   happy_var_1 happy_var_3 happy_var_5 happy_var_7 happy_var_9 happy_var_11
	) `HappyStk` happyRest

happyReduce_74 = happyReduce 9 25 happyReduction_74
happyReduction_74 ((HappyTerminal (LTok _ (LTokenName happy_var_9))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTok _ (LTokenName happy_var_7))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTok _ (LTokenInt happy_var_5))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTok _ (LTokenName happy_var_3))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTok _ (LTokenName happy_var_1))) `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (UMinus    happy_var_1 happy_var_3 happy_var_5 happy_var_7 happy_var_9
	) `HappyStk` happyRest

happyReduce_75 = happyReduce 11 25 happyReduction_75
happyReduction_75 ((HappyTerminal (LTok _ (LTokenName happy_var_11))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTok _ (LTokenName happy_var_9))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTok _ (LTokenName happy_var_7))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTok _ (LTokenName happy_var_5))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTok _ (LTokenName happy_var_3))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTok _ (LTokenName happy_var_1))) `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (UMinusDot happy_var_1 happy_var_3 happy_var_5 happy_var_7 happy_var_9 happy_var_11
	) `HappyStk` happyRest

happyReduce_76 = happySpecReduce_3  26 happyReduction_76
happyReduction_76 (HappyAbsSyn27  happy_var_3)
	_
	_
	 =  HappyAbsSyn26
		 (Delete happy_var_3
	)
happyReduction_76 _ _ _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_3  27 happyReduction_77
happyReduction_77 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn27
		 (happy_var_1 : happy_var_3
	)
happyReduction_77 _ _ _  = notHappyAtAll 

happyReduce_78 = happySpecReduce_1  27 happyReduction_78
happyReduction_78 (HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn27
		 ([happy_var_1]
	)
happyReduction_78 _  = notHappyAtAll 

happyReduce_79 = happyReduce 4 28 happyReduction_79
happyReduction_79 ((HappyTerminal (LTok _ (LTokenInt happy_var_4))) `HappyStk`
	(HappyTerminal (LTok _ (LTokenName happy_var_3))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTok _ (LTokenName happy_var_1))) `HappyStk`
	happyRest)
	 = HappyAbsSyn28
		 (Del happy_var_1 happy_var_3  (show happy_var_4)
	) `HappyStk` happyRest

happyReduce_80 = happyReduce 4 28 happyReduction_80
happyReduction_80 ((HappyTerminal (LTok _ (LTokenString happy_var_4))) `HappyStk`
	(HappyTerminal (LTok _ (LTokenName happy_var_3))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTok _ (LTokenName happy_var_1))) `HappyStk`
	happyRest)
	 = HappyAbsSyn28
		 (Del happy_var_1 happy_var_3  happy_var_4
	) `HappyStk` happyRest

happyReduce_81 = happyReduce 4 28 happyReduction_81
happyReduction_81 (_ `HappyStk`
	(HappyTerminal (LTok _ (LTokenName happy_var_3))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTok _ (LTokenName happy_var_1))) `HappyStk`
	happyRest)
	 = HappyAbsSyn28
		 (Del happy_var_1 happy_var_3  "True"
	) `HappyStk` happyRest

happyReduce_82 = happyReduce 4 28 happyReduction_82
happyReduction_82 (_ `HappyStk`
	(HappyTerminal (LTok _ (LTokenName happy_var_3))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTok _ (LTokenName happy_var_1))) `HappyStk`
	happyRest)
	 = HappyAbsSyn28
		 (Del happy_var_1 happy_var_3  "False"
	) `HappyStk` happyRest

happyReduce_83 = happyReduce 4 28 happyReduction_83
happyReduction_83 (_ `HappyStk`
	(HappyTerminal (LTok _ (LTokenName happy_var_3))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LTok _ (LTokenName happy_var_1))) `HappyStk`
	happyRest)
	 = HappyAbsSyn28
		 (Del happy_var_1 happy_var_3  "null"
	) `HappyStk` happyRest

happyReduce_84 = happySpecReduce_3  28 happyReduction_84
happyReduction_84 (HappyTerminal (LTok _ (LTokenString happy_var_3)))
	_
	(HappyTerminal (LTok _ (LTokenName happy_var_1)))
	 =  HappyAbsSyn28
		 (Del happy_var_1 ":ID"       happy_var_3
	)
happyReduction_84 _ _ _  = notHappyAtAll 

happyReduce_85 = happySpecReduce_3  28 happyReduction_85
happyReduction_85 (HappyTerminal (LTok _ (LTokenString happy_var_3)))
	_
	(HappyTerminal (LTok _ (LTokenName happy_var_1)))
	 =  HappyAbsSyn28
		 (Del happy_var_1 ":TYPE"     happy_var_3
	)
happyReduction_85 _ _ _  = notHappyAtAll 

happyReduce_86 = happySpecReduce_3  28 happyReduction_86
happyReduction_86 (HappyTerminal (LTok _ (LTokenString happy_var_3)))
	_
	(HappyTerminal (LTok _ (LTokenName happy_var_1)))
	 =  HappyAbsSyn28
		 (Del happy_var_1 ":START_ID" happy_var_3
	)
happyReduction_86 _ _ _  = notHappyAtAll 

happyReduce_87 = happySpecReduce_3  28 happyReduction_87
happyReduction_87 (HappyTerminal (LTok _ (LTokenString happy_var_3)))
	_
	(HappyTerminal (LTok _ (LTokenName happy_var_1)))
	 =  HappyAbsSyn28
		 (Del happy_var_1 ":END_ID"   happy_var_3
	)
happyReduction_87 _ _ _  = notHappyAtAll 

happyReduce_88 = happySpecReduce_3  28 happyReduction_88
happyReduction_88 (HappyTerminal (LTok _ (LTokenString happy_var_3)))
	_
	(HappyTerminal (LTok _ (LTokenName happy_var_1)))
	 =  HappyAbsSyn28
		 (Del happy_var_1 ":LABEL"    happy_var_3
	)
happyReduction_88 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 72 72 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	LTok _ LTokenAssignment -> cont 29;
	LTok _ LTokenFullStop -> cont 30;
	LTok _ LTokenRead -> cont 31;
	LTok _ LTokenMatch -> cont 32;
	LTok _ LTokenRelated -> cont 33;
	LTok _ LTokenRelatedRight -> cont 34;
	LTok _ LTokenRelatedLeft -> cont 35;
	LTok _ LTokenWhere -> cont 36;
	LTok _ LTokenOr -> cont 37;
	LTok _ LTokenAnd -> cont 38;
	LTok _ LTokenNot -> cont 39;
	LTok _ LTokenIs -> cont 40;
	LTok _ LTokenStartWith -> cont 41;
	LTok _ LTokenEndWith -> cont 42;
	LTok _ LTokenLParen -> cont 43;
	LTok _ LTokenRParen -> cont 44;
	LTok _ LTokenLessThanEqual -> cont 45;
	LTok _ LTokenGreaterThanEqual -> cont 46;
	LTok _ LTokenLessThan -> cont 47;
	LTok _ LTokenGreaterThan -> cont 48;
	LTok _ LTokenEquals -> cont 49;
	LTok _ LTokenNotEquals -> cont 50;
	LTok _ LTokenReturn -> cont 51;
	LTok _ LTokenAppend -> cont 52;
	LTok _ LTokenDelete -> cont 53;
	LTok _ LTokenUpdate -> cont 54;
	LTok _ LTokenComma -> cont 55;
	LTok _ LTokenBar -> cont 56;
	LTok _ LTokenAs -> cont 57;
	LTok _ LTokenAdd -> cont 58;
	LTok _ LTokenMinus -> cont 59;
	LTok _ LTokenNewRelation -> cont 60;
	LTok _ LTokenLabelField -> cont 61;
	LTok _ LTokenIdField -> cont 62;
	LTok _ LTokenStartField -> cont 63;
	LTok _ LTokenEndField -> cont 64;
	LTok _ LTokenTypeField -> cont 65;
	LTok _ LTokenNull -> cont 66;
	LTok _ LTokenTrue -> cont 67;
	LTok _ LTokenFalse -> cont 68;
	LTok _ (LTokenString happy_dollar_dollar) -> cont 69;
	LTok _ (LTokenInt happy_dollar_dollar) -> cont 70;
	LTok _ (LTokenName happy_dollar_dollar) -> cont 71;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 72 tk tks = happyError' (tks, explist)
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
    = Query ReadFile Match Where Print
    deriving (Eq, Show)

data ReadFile
    = ReadFile String
    deriving (Show, Eq)

data Match
    = Match Patterns
    deriving (Eq, Show)

type Patterns
    = [Pattern]

data Pattern 
    = Pattern String
    | PatternRelatedTo String String String
    | PatternRelatedBy String String String
    deriving (Eq, Show)


data Where
    = Where WhereExp
    deriving (Show, Eq)

data WhereExp
    = WAnd WhereExp WhereExp 
    | WOr WhereExp WhereExp 
    | WNot WhereExp
    | WFinal WhereExp
    | WEqual WhereDot WhereLit
    | WNotEqual WhereDot WhereLit
    | WLessThan WhereDot WhereLit
    | WGreaterThan WhereDot WhereLit
    | WLessOrEqualThan WhereDot WhereLit
    | WGreaterOrEqualThan WhereDot WhereLit
    | WStartsWith WhereDot WhereLit
    | WEndsWith WhereDot WhereLit

    | WEqualDot WhereDot WhereDot
    | WNotEqualDot WhereDot WhereDot
    | WLessThanDot WhereDot WhereDot
    | WGreaterThanDot WhereDot WhereDot
    | WLessOrEqualThanDot WhereDot WhereDot
    | WGreaterOrEqualThanDot WhereDot WhereDot
    | WStartsWithDot WhereDot WhereDot
    | WEndsWithDot WhereDot WhereDot
    deriving (Show, Eq)



data WhereDot
    = WDot String String
    deriving (Show, Eq)

data WhereLit
    = WStr String
    | WInt Int
    | WBool Bool
    | WNull
    deriving (Show, Eq)


data Print
    = Print1 Update Delete Return
    | Print2 Update Delete Append
    | Print3 Update Return
    | Print4 Update Append
    | Print5 Delete Return
    | Print6 Delete Append
    | Print7 Return
    | Print8 Append
    deriving (Eq, Show)

data Return 
    = Return [PrintExps]
    deriving (Eq, Show)

data Append
    = Append [PrintExps]
    deriving (Eq, Show)

type PrintExps
    = [PrintExp]

data PrintExp
    = Output String String String
    | NewRelation String String String String String
    deriving (Eq, Show)

data Update
    = Update [UpdateExp]
    deriving (Eq, Show)

data UpdateExp
    = UAdd String String Int String String 
    | UAddDot String String String String String String
    | UMinus String String Int String String 
    | UMinusDot String String String String String String
    deriving (Eq, Show)

data Delete
    = Delete [DeleteExp]
    deriving (Eq, Show)

data DeleteExp
    = Del String String String
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
