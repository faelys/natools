--  Generated at 2014-10-03 21:03:09 +0000 by Natools.Static_Hash_Maps
--  from src/natools-s_expressions-templates-dates-maps.sx

with Natools.Static_Maps.S_Expressions.Templates.Dates.Cmds;

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

end Natools.Static_Maps.S_Expressions.Templates.Dates;
