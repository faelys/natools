--  Generated at 2014-10-03 21:03:09 +0000 by Natools.Static_Hash_Maps
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
      RFC_3339);

   function Main (Key : String) return Main_Command;

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
   Map_1_Key_27 : aliased constant String := "year";
   Map_1_Keys : constant array (0 .. 27) of access constant String
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
         Map_1_Key_27'Access);
   Map_1_Elements : constant array (0 .. 27) of Main_Command
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
         Year);

end Natools.Static_Maps.S_Expressions.Templates.Dates;
