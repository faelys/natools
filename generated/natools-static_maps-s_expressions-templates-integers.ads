--  Generated at 2014-09-14 21:29:47 +0000 by Natools.Static_Hash_Maps
--  from src/natools-s_expressions-templates-generic_integers-maps.sx

package Natools.Static_Maps.S_Expressions.Templates.Integers is
   pragma Pure;

   type Main_Command is
     (Error,
      Align,
      Align_Center,
      Align_Left,
      Align_Right,
      Base,
      Padding,
      Padding_Left,
      Padding_Right,
      Sign,
      Width,
      Width_Max,
      Width_Min);

   type Align_Command is (Unknown_Align, Set_Left, Set_Center, Set_Right);

   function Main (Key : String) return Main_Command;
   function To_Align_Command (Key : String) return Align_Command;

private

   Map_1_Key_0 : aliased constant String := "align";
   Map_1_Key_1 : aliased constant String := "align-center";
   Map_1_Key_2 : aliased constant String := "centered";
   Map_1_Key_3 : aliased constant String := "align-left";
   Map_1_Key_4 : aliased constant String := "left-align";
   Map_1_Key_5 : aliased constant String := "align-right";
   Map_1_Key_6 : aliased constant String := "right-align";
   Map_1_Key_7 : aliased constant String := "base";
   Map_1_Key_8 : aliased constant String := "padding";
   Map_1_Key_9 : aliased constant String := "padding-left";
   Map_1_Key_10 : aliased constant String := "left-padding";
   Map_1_Key_11 : aliased constant String := "padding-right";
   Map_1_Key_12 : aliased constant String := "right-padding";
   Map_1_Key_13 : aliased constant String := "sign";
   Map_1_Key_14 : aliased constant String := "signs";
   Map_1_Key_15 : aliased constant String := "width";
   Map_1_Key_16 : aliased constant String := "width-max";
   Map_1_Key_17 : aliased constant String := "max-width";
   Map_1_Key_18 : aliased constant String := "width-min";
   Map_1_Key_19 : aliased constant String := "min-width";
   Map_1_Keys : constant array (0 .. 19) of access constant String
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
         Map_1_Key_19'Access);
   Map_1_Elements : constant array (0 .. 19) of Main_Command
     := (Align,
         Align_Center,
         Align_Center,
         Align_Left,
         Align_Left,
         Align_Right,
         Align_Right,
         Base,
         Padding,
         Padding_Left,
         Padding_Left,
         Padding_Right,
         Padding_Right,
         Sign,
         Sign,
         Width,
         Width_Max,
         Width_Max,
         Width_Min,
         Width_Min);

   Map_2_Key_0 : aliased constant String := "left";
   Map_2_Key_1 : aliased constant String := "center";
   Map_2_Key_2 : aliased constant String := "right";
   Map_2_Keys : constant array (0 .. 2) of access constant String
     := (Map_2_Key_0'Access,
         Map_2_Key_1'Access,
         Map_2_Key_2'Access);
   Map_2_Elements : constant array (0 .. 2) of Align_Command
     := (Set_Left,
         Set_Center,
         Set_Right);

end Natools.Static_Maps.S_Expressions.Templates.Integers;
