--  Generated at 2015-03-31 18:55:08 +0000 by Natools.Static_Hash_Maps
--  from src/natools-s_expressions-conditionals-strings-maps.sx

with Natools.Static_Maps.S_Expressions.Conditionals.Strings.P;
with Natools.Static_Maps.S_Expressions.Conditionals.Strings.S;
function Natools.Static_Maps.S_Expressions.Conditionals.Strings.T
  return Boolean is
begin
   for I in Map_1_Keys'Range loop
      if Natools.Static_Maps.S_Expressions.Conditionals.Strings.P.Hash
           (Map_1_Keys (I).all) /= I
      then
         return False;
      end if;
   end loop;

   for I in Map_2_Keys'Range loop
      if Natools.Static_Maps.S_Expressions.Conditionals.Strings.S.Hash
           (Map_2_Keys (I).all) /= I
      then
         return False;
      end if;
   end loop;

   return True;
end Natools.Static_Maps.S_Expressions.Conditionals.Strings.T;
