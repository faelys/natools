--  Generated at 2014-11-09 20:46:38 +0000 by Natools.Static_Hash_Maps
--  from src/natools-static_hash_maps-s_expressions-hash_maps.sx

private package Natools.Static_Hash_Maps.S_Expressions.Command_Maps is

   function To_Package_Command (Key : String) return Package_Command;
   function To_Map_Command (Key : String) return Map_Command;

private

   Map_1_Key_0 : aliased constant String := "extra-declarations";
   Map_1_Key_1 : aliased constant String := "extra-decl";
   Map_1_Key_2 : aliased constant String := "private";
   Map_1_Key_3 : aliased constant String := "public";
   Map_1_Key_4 : aliased constant String := "pure";
   Map_1_Key_5 : aliased constant String := "preelaborate";
   Map_1_Key_6 : aliased constant String := "test-function";
   Map_1_Keys : constant array (0 .. 6) of access constant String
     := (Map_1_Key_0'Access,
         Map_1_Key_1'Access,
         Map_1_Key_2'Access,
         Map_1_Key_3'Access,
         Map_1_Key_4'Access,
         Map_1_Key_5'Access,
         Map_1_Key_6'Access);
   Map_1_Elements : constant array (0 .. 6) of Package_Command
     := (Extra_Declarations,
         Extra_Declarations,
         Private_Child,
         Public_Child,
         Pure_Package,
         Preelaborate_Package,
         Test_Function);

   Map_2_Key_0 : aliased constant String := "definite";
   Map_2_Key_1 : aliased constant String := "definite-elements";
   Map_2_Key_2 : aliased constant String := "indefinite";
   Map_2_Key_3 : aliased constant String := "indefinite-elements";
   Map_2_Key_4 : aliased constant String := "hash-package";
   Map_2_Key_5 : aliased constant String := "nodes";
   Map_2_Key_6 : aliased constant String := "function";
   Map_2_Key_7 : aliased constant String := "not-found";
   Map_2_Keys : constant array (0 .. 7) of access constant String
     := (Map_2_Key_0'Access,
         Map_2_Key_1'Access,
         Map_2_Key_2'Access,
         Map_2_Key_3'Access,
         Map_2_Key_4'Access,
         Map_2_Key_5'Access,
         Map_2_Key_6'Access,
         Map_2_Key_7'Access);
   Map_2_Elements : constant array (0 .. 7) of Map_Command
     := (Definite_Elements,
         Definite_Elements,
         Indefinite_Elements,
         Indefinite_Elements,
         Hash_Package,
         Nodes,
         Function_Name,
         Not_Found);

end Natools.Static_Hash_Maps.S_Expressions.Command_Maps;
