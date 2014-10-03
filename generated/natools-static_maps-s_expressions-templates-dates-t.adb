--  Generated at 2014-10-03 21:03:09 +0000 by Natools.Static_Hash_Maps
--  from src/natools-s_expressions-templates-dates-maps.sx

with Natools.Static_Maps.S_Expressions.Templates.Dates.Cmds;
function Natools.Static_Maps.S_Expressions.Templates.Dates.T
  return Boolean is
begin
   for I in Map_1_Keys'Range loop
      if Natools.Static_Maps.S_Expressions.Templates.Dates.Cmds.Hash
           (Map_1_Keys (I).all) /= I
      then
         return False;
      end if;
   end loop;

   return True;
end Natools.Static_Maps.S_Expressions.Templates.Dates.T;
