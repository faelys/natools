--  Generated at 2014-10-01 17:18:35 +0000 by Natools.Static_Hash_Maps
--  from src/natools-s_expressions-templates-generic_integers-maps.sx

with Natools.Static_Maps.S_Expressions.Templates.Integers.MC;
with Natools.Static_Maps.S_Expressions.Templates.Integers.AC;
function Natools.Static_Maps.S_Expressions.Templates.Integers.T
  return Boolean is
begin
   for I in Map_1_Keys'Range loop
      if Natools.Static_Maps.S_Expressions.Templates.Integers.MC.Hash
           (Map_1_Keys (I).all) /= I
      then
         return False;
      end if;
   end loop;

   for I in Map_2_Keys'Range loop
      if Natools.Static_Maps.S_Expressions.Templates.Integers.AC.Hash
           (Map_2_Keys (I).all) /= I
      then
         return False;
      end if;
   end loop;

   return True;
end Natools.Static_Maps.S_Expressions.Templates.Integers.T;
