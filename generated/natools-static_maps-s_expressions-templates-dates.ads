--  Generated at 2015-06-24 18:19:13 +0000 by Natools.Static_Hash_Maps
--  from src/natools-s_expressions-templates-dates-maps.sx

package Natools.Static_Maps.S_Expressions.Templates.Dates is
   pragma Pure;

   type Main_Command is
     (Error,
      Year, Month, Day, Hour, Minute, Second,
      Padded_Month, Padded_Day,
      Padded_Hour, Padded_Minute, Padded_Second,
      Day_Of_Week,
      Big_Endian_Date, Little_Endian_Date,
      Big_Endian_Time, Little_Endian_Time,
      RFC_3339, With_Offset);

   function Main (Key : String) return Main_Command;
   function To_Time_Offset (Key : String) return Integer;

private

   Map_1_Key_0 : aliased constant String := "YYYYMMDD";
   Map_1_Key_1 : aliased constant String := "big-endian-date";
   Map_1_Key_2 : aliased constant String := "HHMMSS";
   Map_1_Key_3 : aliased constant String := "time";
   Map_1_Key_4 : aliased constant String := "big-endian-time";
   Map_1_Key_5 : aliased constant String := "day";
   Map_1_Key_6 : aliased constant String := "dow";
   Map_1_Key_7 : aliased constant String := "day-of-week";
   Map_1_Key_8 : aliased constant String := "hour";
   Map_1_Key_9 : aliased constant String := "DDMMYYYY";
   Map_1_Key_10 : aliased constant String := "little-endian-date";
   Map_1_Key_11 : aliased constant String := "SSMMHH";
   Map_1_Key_12 : aliased constant String := "little-endian-time";
   Map_1_Key_13 : aliased constant String := "minute";
   Map_1_Key_14 : aliased constant String := "month";
   Map_1_Key_15 : aliased constant String := "0day";
   Map_1_Key_16 : aliased constant String := "padded-day";
   Map_1_Key_17 : aliased constant String := "0hour";
   Map_1_Key_18 : aliased constant String := "padded-hour";
   Map_1_Key_19 : aliased constant String := "0minute";
   Map_1_Key_20 : aliased constant String := "padded-minute";
   Map_1_Key_21 : aliased constant String := "0month";
   Map_1_Key_22 : aliased constant String := "padded-month";
   Map_1_Key_23 : aliased constant String := "0second";
   Map_1_Key_24 : aliased constant String := "padded-second";
   Map_1_Key_25 : aliased constant String := "rfc-3339";
   Map_1_Key_26 : aliased constant String := "second";
   Map_1_Key_27 : aliased constant String := "with-offset";
   Map_1_Key_28 : aliased constant String := "in-zone";
   Map_1_Key_29 : aliased constant String := "year";
   Map_1_Keys : constant array (0 .. 29) of access constant String
     := (Map_1_Key_0'Access,
         Map_1_Key_1'Access,
         Map_1_Key_2'Access,
         Map_1_Key_3'Access,
         Map_1_Key_4'Access,
         Map_1_Key_5'Access,
         Map_1_Key_6'Access,
         Map_1_Key_7'Access,
         Map_1_Key_8'Access,
         Map_1_Key_9'Access,
         Map_1_Key_10'Access,
         Map_1_Key_11'Access,
         Map_1_Key_12'Access,
         Map_1_Key_13'Access,
         Map_1_Key_14'Access,
         Map_1_Key_15'Access,
         Map_1_Key_16'Access,
         Map_1_Key_17'Access,
         Map_1_Key_18'Access,
         Map_1_Key_19'Access,
         Map_1_Key_20'Access,
         Map_1_Key_21'Access,
         Map_1_Key_22'Access,
         Map_1_Key_23'Access,
         Map_1_Key_24'Access,
         Map_1_Key_25'Access,
         Map_1_Key_26'Access,
         Map_1_Key_27'Access,
         Map_1_Key_28'Access,
         Map_1_Key_29'Access);
   Map_1_Elements : constant array (0 .. 29) of Main_Command
     := (Big_Endian_Date,
         Big_Endian_Date,
         Big_Endian_Time,
         Big_Endian_Time,
         Big_Endian_Time,
         Day,
         Day_Of_Week,
         Day_Of_Week,
         Hour,
         Little_Endian_Date,
         Little_Endian_Date,
         Little_Endian_Time,
         Little_Endian_Time,
         Minute,
         Month,
         Padded_Day,
         Padded_Day,
         Padded_Hour,
         Padded_Hour,
         Padded_Minute,
         Padded_Minute,
         Padded_Month,
         Padded_Month,
         Padded_Second,
         Padded_Second,
         RFC_3339,
         Second,
         With_Offset,
         With_Offset,
         Year);

   Map_2_Key_0 : aliased constant String := "ACDT";
   Map_2_Key_1 : aliased constant String := "ACST";
   Map_2_Key_2 : aliased constant String := "ADT";
   Map_2_Key_3 : aliased constant String := "AEDT";
   Map_2_Key_4 : aliased constant String := "AEST";
   Map_2_Key_5 : aliased constant String := "AFT";
   Map_2_Key_6 : aliased constant String := "AKDT";
   Map_2_Key_7 : aliased constant String := "AKST";
   Map_2_Key_8 : aliased constant String := "ART";
   Map_2_Key_9 : aliased constant String := "AWDT";
   Map_2_Key_10 : aliased constant String := "AWST";
   Map_2_Key_11 : aliased constant String := "AZOST";
   Map_2_Key_12 : aliased constant String := "AZT";
   Map_2_Key_13 : aliased constant String := "BDT";
   Map_2_Key_14 : aliased constant String := "BIOT";
   Map_2_Key_15 : aliased constant String := "BIT";
   Map_2_Key_16 : aliased constant String := "BOT";
   Map_2_Key_17 : aliased constant String := "BRST";
   Map_2_Key_18 : aliased constant String := "BRT";
   Map_2_Key_19 : aliased constant String := "BTT";
   Map_2_Key_20 : aliased constant String := "CAT";
   Map_2_Key_21 : aliased constant String := "CCT";
   Map_2_Key_22 : aliased constant String := "CEDT";
   Map_2_Key_23 : aliased constant String := "CEST";
   Map_2_Key_24 : aliased constant String := "CET";
   Map_2_Key_25 : aliased constant String := "CHADT";
   Map_2_Key_26 : aliased constant String := "CHAST";
   Map_2_Key_27 : aliased constant String := "CHOT";
   Map_2_Key_28 : aliased constant String := "ChST";
   Map_2_Key_29 : aliased constant String := "CHUT";
   Map_2_Key_30 : aliased constant String := "CIST";
   Map_2_Key_31 : aliased constant String := "CIT";
   Map_2_Key_32 : aliased constant String := "CKT";
   Map_2_Key_33 : aliased constant String := "CLST";
   Map_2_Key_34 : aliased constant String := "CLT";
   Map_2_Key_35 : aliased constant String := "COST";
   Map_2_Key_36 : aliased constant String := "COT";
   Map_2_Key_37 : aliased constant String := "CT";
   Map_2_Key_38 : aliased constant String := "CVT";
   Map_2_Key_39 : aliased constant String := "CWST";
   Map_2_Key_40 : aliased constant String := "CXT";
   Map_2_Key_41 : aliased constant String := "DAVT";
   Map_2_Key_42 : aliased constant String := "DDUT";
   Map_2_Key_43 : aliased constant String := "DFT";
   Map_2_Key_44 : aliased constant String := "EASST";
   Map_2_Key_45 : aliased constant String := "EAST";
   Map_2_Key_46 : aliased constant String := "EAT";
   Map_2_Key_47 : aliased constant String := "EDT";
   Map_2_Key_48 : aliased constant String := "EEDT";
   Map_2_Key_49 : aliased constant String := "EEST";
   Map_2_Key_50 : aliased constant String := "EET";
   Map_2_Key_51 : aliased constant String := "EGST";
   Map_2_Key_52 : aliased constant String := "EGT";
   Map_2_Key_53 : aliased constant String := "EIT";
   Map_2_Key_54 : aliased constant String := "FET";
   Map_2_Key_55 : aliased constant String := "FJT";
   Map_2_Key_56 : aliased constant String := "FKST";
   Map_2_Key_57 : aliased constant String := "FKT";
   Map_2_Key_58 : aliased constant String := "FNT";
   Map_2_Key_59 : aliased constant String := "GALT";
   Map_2_Key_60 : aliased constant String := "GAMT";
   Map_2_Key_61 : aliased constant String := "GET";
   Map_2_Key_62 : aliased constant String := "GFT";
   Map_2_Key_63 : aliased constant String := "GILT";
   Map_2_Key_64 : aliased constant String := "GIT";
   Map_2_Key_65 : aliased constant String := "GMT";
   Map_2_Key_66 : aliased constant String := "GYT";
   Map_2_Key_67 : aliased constant String := "HADT";
   Map_2_Key_68 : aliased constant String := "HAEC";
   Map_2_Key_69 : aliased constant String := "HAST";
   Map_2_Key_70 : aliased constant String := "HKT";
   Map_2_Key_71 : aliased constant String := "HMT";
   Map_2_Key_72 : aliased constant String := "HOVT";
   Map_2_Key_73 : aliased constant String := "HST";
   Map_2_Key_74 : aliased constant String := "ICT";
   Map_2_Key_75 : aliased constant String := "IDT";
   Map_2_Key_76 : aliased constant String := "IOT";
   Map_2_Key_77 : aliased constant String := "IRDT";
   Map_2_Key_78 : aliased constant String := "IRKT";
   Map_2_Key_79 : aliased constant String := "IRST";
   Map_2_Key_80 : aliased constant String := "JST";
   Map_2_Key_81 : aliased constant String := "KGT";
   Map_2_Key_82 : aliased constant String := "KOST";
   Map_2_Key_83 : aliased constant String := "KRAT";
   Map_2_Key_84 : aliased constant String := "KST";
   Map_2_Key_85 : aliased constant String := "LINT";
   Map_2_Key_86 : aliased constant String := "MAGT";
   Map_2_Key_87 : aliased constant String := "MART";
   Map_2_Key_88 : aliased constant String := "MAWT";
   Map_2_Key_89 : aliased constant String := "MDT";
   Map_2_Key_90 : aliased constant String := "MET";
   Map_2_Key_91 : aliased constant String := "MEST";
   Map_2_Key_92 : aliased constant String := "MHT";
   Map_2_Key_93 : aliased constant String := "MIST";
   Map_2_Key_94 : aliased constant String := "MIT";
   Map_2_Key_95 : aliased constant String := "MMT";
   Map_2_Key_96 : aliased constant String := "MSK";
   Map_2_Key_97 : aliased constant String := "MUT";
   Map_2_Key_98 : aliased constant String := "MVT";
   Map_2_Key_99 : aliased constant String := "MYT";
   Map_2_Key_100 : aliased constant String := "NCT";
   Map_2_Key_101 : aliased constant String := "NDT";
   Map_2_Key_102 : aliased constant String := "NFT";
   Map_2_Key_103 : aliased constant String := "NPT";
   Map_2_Key_104 : aliased constant String := "NST";
   Map_2_Key_105 : aliased constant String := "NT";
   Map_2_Key_106 : aliased constant String := "NUT";
   Map_2_Key_107 : aliased constant String := "NZDT";
   Map_2_Key_108 : aliased constant String := "NZST";
   Map_2_Key_109 : aliased constant String := "OMST";
   Map_2_Key_110 : aliased constant String := "ORAT";
   Map_2_Key_111 : aliased constant String := "PDT";
   Map_2_Key_112 : aliased constant String := "PET";
   Map_2_Key_113 : aliased constant String := "PETT";
   Map_2_Key_114 : aliased constant String := "PGT";
   Map_2_Key_115 : aliased constant String := "PHOT";
   Map_2_Key_116 : aliased constant String := "PKT";
   Map_2_Key_117 : aliased constant String := "PMDT";
   Map_2_Key_118 : aliased constant String := "PMST";
   Map_2_Key_119 : aliased constant String := "PONT";
   Map_2_Key_120 : aliased constant String := "PYST";
   Map_2_Key_121 : aliased constant String := "PYT";
   Map_2_Key_122 : aliased constant String := "RET";
   Map_2_Key_123 : aliased constant String := "ROTT";
   Map_2_Key_124 : aliased constant String := "SAKT";
   Map_2_Key_125 : aliased constant String := "SAMT";
   Map_2_Key_126 : aliased constant String := "SAST";
   Map_2_Key_127 : aliased constant String := "SBT";
   Map_2_Key_128 : aliased constant String := "SCT";
   Map_2_Key_129 : aliased constant String := "SGT";
   Map_2_Key_130 : aliased constant String := "SLST";
   Map_2_Key_131 : aliased constant String := "SRET";
   Map_2_Key_132 : aliased constant String := "SRT";
   Map_2_Key_133 : aliased constant String := "SYOT";
   Map_2_Key_134 : aliased constant String := "TAHT";
   Map_2_Key_135 : aliased constant String := "THA";
   Map_2_Key_136 : aliased constant String := "TFT";
   Map_2_Key_137 : aliased constant String := "TJT";
   Map_2_Key_138 : aliased constant String := "TKT";
   Map_2_Key_139 : aliased constant String := "TLT";
   Map_2_Key_140 : aliased constant String := "TMT";
   Map_2_Key_141 : aliased constant String := "TOT";
   Map_2_Key_142 : aliased constant String := "TVT";
   Map_2_Key_143 : aliased constant String := "UCT";
   Map_2_Key_144 : aliased constant String := "ULAT";
   Map_2_Key_145 : aliased constant String := "USZ1";
   Map_2_Key_146 : aliased constant String := "UTC";
   Map_2_Key_147 : aliased constant String := "UYST";
   Map_2_Key_148 : aliased constant String := "UYT";
   Map_2_Key_149 : aliased constant String := "UZT";
   Map_2_Key_150 : aliased constant String := "VET";
   Map_2_Key_151 : aliased constant String := "VLAT";
   Map_2_Key_152 : aliased constant String := "VOLT";
   Map_2_Key_153 : aliased constant String := "VOST";
   Map_2_Key_154 : aliased constant String := "VUT";
   Map_2_Key_155 : aliased constant String := "WAKT";
   Map_2_Key_156 : aliased constant String := "WAST";
   Map_2_Key_157 : aliased constant String := "WAT";
   Map_2_Key_158 : aliased constant String := "WEDT";
   Map_2_Key_159 : aliased constant String := "WEST";
   Map_2_Key_160 : aliased constant String := "WET";
   Map_2_Key_161 : aliased constant String := "WIT";
   Map_2_Key_162 : aliased constant String := "WST";
   Map_2_Key_163 : aliased constant String := "YAKT";
   Map_2_Key_164 : aliased constant String := "YEKT";
   Map_2_Key_165 : aliased constant String := "Z";
   Map_2_Keys : constant array (0 .. 165) of access constant String
     := (Map_2_Key_0'Access,
         Map_2_Key_1'Access,
         Map_2_Key_2'Access,
         Map_2_Key_3'Access,
         Map_2_Key_4'Access,
         Map_2_Key_5'Access,
         Map_2_Key_6'Access,
         Map_2_Key_7'Access,
         Map_2_Key_8'Access,
         Map_2_Key_9'Access,
         Map_2_Key_10'Access,
         Map_2_Key_11'Access,
         Map_2_Key_12'Access,
         Map_2_Key_13'Access,
         Map_2_Key_14'Access,
         Map_2_Key_15'Access,
         Map_2_Key_16'Access,
         Map_2_Key_17'Access,
         Map_2_Key_18'Access,
         Map_2_Key_19'Access,
         Map_2_Key_20'Access,
         Map_2_Key_21'Access,
         Map_2_Key_22'Access,
         Map_2_Key_23'Access,
         Map_2_Key_24'Access,
         Map_2_Key_25'Access,
         Map_2_Key_26'Access,
         Map_2_Key_27'Access,
         Map_2_Key_28'Access,
         Map_2_Key_29'Access,
         Map_2_Key_30'Access,
         Map_2_Key_31'Access,
         Map_2_Key_32'Access,
         Map_2_Key_33'Access,
         Map_2_Key_34'Access,
         Map_2_Key_35'Access,
         Map_2_Key_36'Access,
         Map_2_Key_37'Access,
         Map_2_Key_38'Access,
         Map_2_Key_39'Access,
         Map_2_Key_40'Access,
         Map_2_Key_41'Access,
         Map_2_Key_42'Access,
         Map_2_Key_43'Access,
         Map_2_Key_44'Access,
         Map_2_Key_45'Access,
         Map_2_Key_46'Access,
         Map_2_Key_47'Access,
         Map_2_Key_48'Access,
         Map_2_Key_49'Access,
         Map_2_Key_50'Access,
         Map_2_Key_51'Access,
         Map_2_Key_52'Access,
         Map_2_Key_53'Access,
         Map_2_Key_54'Access,
         Map_2_Key_55'Access,
         Map_2_Key_56'Access,
         Map_2_Key_57'Access,
         Map_2_Key_58'Access,
         Map_2_Key_59'Access,
         Map_2_Key_60'Access,
         Map_2_Key_61'Access,
         Map_2_Key_62'Access,
         Map_2_Key_63'Access,
         Map_2_Key_64'Access,
         Map_2_Key_65'Access,
         Map_2_Key_66'Access,
         Map_2_Key_67'Access,
         Map_2_Key_68'Access,
         Map_2_Key_69'Access,
         Map_2_Key_70'Access,
         Map_2_Key_71'Access,
         Map_2_Key_72'Access,
         Map_2_Key_73'Access,
         Map_2_Key_74'Access,
         Map_2_Key_75'Access,
         Map_2_Key_76'Access,
         Map_2_Key_77'Access,
         Map_2_Key_78'Access,
         Map_2_Key_79'Access,
         Map_2_Key_80'Access,
         Map_2_Key_81'Access,
         Map_2_Key_82'Access,
         Map_2_Key_83'Access,
         Map_2_Key_84'Access,
         Map_2_Key_85'Access,
         Map_2_Key_86'Access,
         Map_2_Key_87'Access,
         Map_2_Key_88'Access,
         Map_2_Key_89'Access,
         Map_2_Key_90'Access,
         Map_2_Key_91'Access,
         Map_2_Key_92'Access,
         Map_2_Key_93'Access,
         Map_2_Key_94'Access,
         Map_2_Key_95'Access,
         Map_2_Key_96'Access,
         Map_2_Key_97'Access,
         Map_2_Key_98'Access,
         Map_2_Key_99'Access,
         Map_2_Key_100'Access,
         Map_2_Key_101'Access,
         Map_2_Key_102'Access,
         Map_2_Key_103'Access,
         Map_2_Key_104'Access,
         Map_2_Key_105'Access,
         Map_2_Key_106'Access,
         Map_2_Key_107'Access,
         Map_2_Key_108'Access,
         Map_2_Key_109'Access,
         Map_2_Key_110'Access,
         Map_2_Key_111'Access,
         Map_2_Key_112'Access,
         Map_2_Key_113'Access,
         Map_2_Key_114'Access,
         Map_2_Key_115'Access,
         Map_2_Key_116'Access,
         Map_2_Key_117'Access,
         Map_2_Key_118'Access,
         Map_2_Key_119'Access,
         Map_2_Key_120'Access,
         Map_2_Key_121'Access,
         Map_2_Key_122'Access,
         Map_2_Key_123'Access,
         Map_2_Key_124'Access,
         Map_2_Key_125'Access,
         Map_2_Key_126'Access,
         Map_2_Key_127'Access,
         Map_2_Key_128'Access,
         Map_2_Key_129'Access,
         Map_2_Key_130'Access,
         Map_2_Key_131'Access,
         Map_2_Key_132'Access,
         Map_2_Key_133'Access,
         Map_2_Key_134'Access,
         Map_2_Key_135'Access,
         Map_2_Key_136'Access,
         Map_2_Key_137'Access,
         Map_2_Key_138'Access,
         Map_2_Key_139'Access,
         Map_2_Key_140'Access,
         Map_2_Key_141'Access,
         Map_2_Key_142'Access,
         Map_2_Key_143'Access,
         Map_2_Key_144'Access,
         Map_2_Key_145'Access,
         Map_2_Key_146'Access,
         Map_2_Key_147'Access,
         Map_2_Key_148'Access,
         Map_2_Key_149'Access,
         Map_2_Key_150'Access,
         Map_2_Key_151'Access,
         Map_2_Key_152'Access,
         Map_2_Key_153'Access,
         Map_2_Key_154'Access,
         Map_2_Key_155'Access,
         Map_2_Key_156'Access,
         Map_2_Key_157'Access,
         Map_2_Key_158'Access,
         Map_2_Key_159'Access,
         Map_2_Key_160'Access,
         Map_2_Key_161'Access,
         Map_2_Key_162'Access,
         Map_2_Key_163'Access,
         Map_2_Key_164'Access,
         Map_2_Key_165'Access);
   Map_2_Elements : constant array (0 .. 165) of Integer
     := (+10 * 60 + 30,
         +09 * 60 + 30,
         -03 * 60,
         +11 * 60,
         +10 * 60,
         +04 * 60 + 30,
         -08 * 60,
         -09 * 60,
         -03 * 60,
         +09 * 60,
         +08 * 60,
         -01 * 60,
         +04 * 60,
         +08 * 60,
         +06 * 60,
         -12 * 60,
         -04 * 60,
         -02 * 60,
         -03 * 60,
         +06 * 60,
         +02 * 60,
         +06 * 60 + 30,
         +02 * 60,
         +02 * 60,
         +01 * 60,
         +13 * 60 + 45,
         +12 * 60 + 45,
         +08 * 60,
         +10 * 60,
         +10 * 60,
         -08 * 60,
         +08 * 60,
         -10 * 60,
         -03 * 60,
         -04 * 60,
         -04 * 60,
         -05 * 60,
         +08 * 60,
         -01 * 60,
         +08 * 60 + 45,
         +07 * 60,
         +07 * 60,
         +10 * 60,
         +01 * 60,
         -05 * 60,
         -06 * 60,
         +03 * 60,
         -04 * 60,
         +03 * 60,
         +03 * 60,
         +02 * 60,
         +00 * 60,
         -01 * 60,
         +09 * 60,
         +03 * 60,
         +12 * 60,
         -03 * 60,
         -04 * 60,
         -02 * 60,
         -06 * 60,
         -09 * 60,
         +04 * 60,
         -03 * 60,
         +12 * 60,
         -09 * 60,
         0,
         -04 * 60,
         -09 * 60,
         +02 * 60,
         -10 * 60,
         +08 * 60,
         +05 * 60,
         +07 * 60,
         -10 * 60,
         +07 * 60,
         +03 * 60,
         +03 * 60,
         +04 * 60 + 30,
         +08 * 60,
         +03 * 60 + 30,
         +09 * 60,
         +06 * 60,
         +11 * 60,
         +07 * 60,
         +09 * 60,
         +14 * 60,
         +12 * 60,
         -09 * 60 + 30,
         +05 * 60,
         -06 * 60,
         +01 * 60,
         +02 * 60,
         +12 * 60,
         +11 * 60,
         -09 * 60 + 30,
         +06 * 60 + 30,
         +03 * 60,
         +04 * 60,
         +05 * 60,
         +08 * 60,
         +11 * 60,
         -02 * 60 + 30,
         +11 * 60 + 30,
         +05 * 60 + 45,
         -03 * 60 + 30,
         -03 * 60 + 30,
         -11 * 60,
         +13 * 60,
         +12 * 60,
         +06 * 60,
         +05 * 60,
         -07 * 60,
         -05 * 60,
         +12 * 60,
         +10 * 60,
         +13 * 60,
         +05 * 60,
         -02 * 60,
         -03 * 60,
         +11 * 60,
         -03 * 60,
         -04 * 60,
         +04 * 60,
         -03 * 60,
         +11 * 60,
         +04 * 60,
         +02 * 60,
         +11 * 60,
         +04 * 60,
         +08 * 60,
         +05 * 60 + 30,
         +11 * 60,
         -03 * 60,
         +03 * 60,
         -10 * 60,
         +07 * 60,
         +05 * 60,
         +05 * 60,
         +13 * 60,
         +09 * 60,
         +05 * 60,
         +13 * 60,
         +12 * 60,
         0,
         +08 * 60,
         +02 * 60,
         0,
         -02 * 60,
         -03 * 60,
         +05 * 60,
         -04 * 60 + 30,
         +10 * 60,
         +04 * 60,
         +06 * 60,
         +11 * 60,
         +12 * 60,
         +02 * 60,
         +01 * 60,
         +01 * 60,
         +01 * 60,
         0,
         +07 * 60,
         +08 * 60,
         +09 * 60,
         +05 * 60,
         0);

end Natools.Static_Maps.S_Expressions.Templates.Dates;
