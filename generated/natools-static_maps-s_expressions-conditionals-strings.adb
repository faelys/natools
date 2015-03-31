--  Generated at 2015-03-31 18:55:08 +0000 by Natools.Static_Hash_Maps
--  from src/natools-s_expressions-conditionals-strings-maps.sx

with Natools.Static_Maps.S_Expressions.Conditionals.Strings.P;
with Natools.Static_Maps.S_Expressions.Conditionals.Strings.S;

package body Natools.Static_Maps.S_Expressions.Conditionals.Strings is

   function To_Parametric (Key : String) return Parametric_Condition is
      N : constant Natural
        := Natools.Static_Maps.S_Expressions.Conditionals.Strings.P.Hash (Key);
   begin
      if Map_1_Keys (N).all = Key then
         return Map_1_Elements (N);
      else
         return Unknown_Parametric_Condition;
      end if;
   end To_Parametric;


   function To_Simple (Key : String) return Simple_Condition is
      N : constant Natural
        := Natools.Static_Maps.S_Expressions.Conditionals.Strings.S.Hash (Key);
   begin
      if Map_2_Keys (N).all = Key then
         return Map_2_Elements (N);
      else
         return Unknown_Simple_Condition;
      end if;
   end To_Simple;

end Natools.Static_Maps.S_Expressions.Conditionals.Strings;
