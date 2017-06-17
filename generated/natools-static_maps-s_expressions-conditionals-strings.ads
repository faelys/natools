--  Generated at 2017-06-17 18:23:38 +0000 by Natools.Static_Hash_Maps
--  from src/natools-s_expressions-conditionals-strings-maps.sx

package Natools.Static_Maps.S_Expressions.Conditionals.Strings is
   pragma Pure;

   type Parametric_Condition is
     (Unknown_Parametric_Condition,
      Case_Insensitive,
      Case_Sensitive,
      Contains_All,
      Contains_Any,
      Is_Equal_To,
      Starts_With);

   type Simple_Condition is
     (Unknown_Simple_Condition,
      Is_ASCII,
      Is_Empty);

   function To_Parametric (Key : String) return Parametric_Condition;
   function To_Simple (Key : String) return Simple_Condition;

private

   Map_1_Key_0 : aliased constant String := "case-insensitive";
   Map_1_Key_1 : aliased constant String := "case-sensitive";
   Map_1_Key_2 : aliased constant String := "contains";
   Map_1_Key_3 : aliased constant String := "contains-all";
   Map_1_Key_4 : aliased constant String := "contains-all-of";
   Map_1_Key_5 : aliased constant String := "contains-any";
   Map_1_Key_6 : aliased constant String := "contains-any-of";
   Map_1_Key_7 : aliased constant String := "is";
   Map_1_Key_8 : aliased constant String := "is-equal-to";
   Map_1_Key_9 : aliased constant String := "=";
   Map_1_Key_10 : aliased constant String := "starts-with";
   Map_1_Keys : constant array (0 .. 10) of access constant String
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
         Map_1_Key_10'Access);
   Map_1_Elements : constant array (0 .. 10) of Parametric_Condition
     := (Case_Insensitive,
         Case_Sensitive,
         Contains_All,
         Contains_All,
         Contains_All,
         Contains_Any,
         Contains_Any,
         Is_Equal_To,
         Is_Equal_To,
         Is_Equal_To,
         Starts_With);

   Map_2_Key_0 : aliased constant String := "is-ascii";
   Map_2_Key_1 : aliased constant String := "is-empty";
   Map_2_Keys : constant array (0 .. 1) of access constant String
     := (Map_2_Key_0'Access,
         Map_2_Key_1'Access);
   Map_2_Elements : constant array (0 .. 1) of Simple_Condition
     := (Is_ASCII,
         Is_Empty);

end Natools.Static_Maps.S_Expressions.Conditionals.Strings;
