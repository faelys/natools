--  Generated at 2014-11-09 20:46:38 +0000 by Natools.Static_Hash_Maps
--  from src/natools-static_hash_maps-s_expressions-hash_maps.sx

with Natools.Static_Hash_Maps.S_Expressions.Command_Pkg;
with Natools.Static_Hash_Maps.S_Expressions.Command_Map;
function Natools.Static_Hash_Maps.S_Expressions.Command_Maps.Test
  return Boolean is
begin
   for I in Map_1_Keys'Range loop
      if Natools.Static_Hash_Maps.S_Expressions.Command_Pkg.Hash
           (Map_1_Keys (I).all) /= I
      then
         return False;
      end if;
   end loop;

   for I in Map_2_Keys'Range loop
      if Natools.Static_Hash_Maps.S_Expressions.Command_Map.Hash
           (Map_2_Keys (I).all) /= I
      then
         return False;
      end if;
   end loop;

   return True;
end Natools.Static_Hash_Maps.S_Expressions.Command_Maps.Test;
