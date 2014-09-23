--  Generated at 2014-09-23 18:25:16 +0000 by Natools.Static_Hash_Maps
--  from src/natools-s_expressions-templates-generic_integers-maps.sx

with Natools.Static_Maps.S_Expressions.Templates.Integers.MC;
with Natools.Static_Maps.S_Expressions.Templates.Integers.AC;

package body Natools.Static_Maps.S_Expressions.Templates.Integers is

   function Main (Key : String) return Main_Command is
      N : constant Natural
        := Natools.Static_Maps.S_Expressions.Templates.Integers.MC.Hash (Key);
   begin
      if Map_1_Keys (N).all = Key then
         return Map_1_Elements (N);
      else
         return Error;
      end if;
   end Main;


   function To_Align_Command (Key : String) return Align_Command is
      N : constant Natural
        := Natools.Static_Maps.S_Expressions.Templates.Integers.AC.Hash (Key);
   begin
      if Map_2_Keys (N).all = Key then
         return Map_2_Elements (N);
      else
         return Unknown_Align;
      end if;
   end To_Align_Command;

end Natools.Static_Maps.S_Expressions.Templates.Integers;
