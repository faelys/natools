--  Generated at 2015-06-24 18:19:13 +0000 by Natools.Static_Hash_Maps
--  from src/natools-s_expressions-templates-dates-maps.sx

with Natools.Static_Maps.S_Expressions.Templates.Dates.Cmds;
with Natools.Static_Maps.S_Expressions.Templates.Dates.Zones;

package body Natools.Static_Maps.S_Expressions.Templates.Dates is

   function Main (Key : String) return Main_Command is
      N : constant Natural
        := Natools.Static_Maps.S_Expressions.Templates.Dates.Cmds.Hash (Key);
   begin
      if Map_1_Keys (N).all = Key then
         return Map_1_Elements (N);
      else
         return Error;
      end if;
   end Main;


   function To_Time_Offset (Key : String) return Integer is
      N : constant Natural
        := Natools.Static_Maps.S_Expressions.Templates.Dates.Zones.Hash (Key);
   begin
      if Map_2_Keys (N).all = Key then
         return Map_2_Elements (N);
      else
         raise Constraint_Error with "Key """ & Key & """ not in map";
      end if;
   end To_Time_Offset;

end Natools.Static_Maps.S_Expressions.Templates.Dates;
