--  Generated at 2014-05-28 17:27:24 +0000 by Natools.Static_Hash_Maps
--  from natools-static_hash_maps-s_expressions-hash_maps.sx

with Natools.Static_Hash_Maps.S_Expressions.Command_Pkg;
with Natools.Static_Hash_Maps.S_Expressions.Command_Map;

package body Natools.Static_Hash_Maps.S_Expressions.Command_Maps is

   function To_Package_Command (Key : String) return Package_Command is
      N : constant Natural
        := Natools.Static_Hash_Maps.S_Expressions.Command_Pkg.Hash (Key);
   begin
      if Map_1_Keys (N).all = Key then
         return Map_1_Elements (N);
      else
         return Extra_Declarations;
      end if;
   end To_Package_Command;


   function To_Map_Command (Key : String) return Map_Command is
      N : constant Natural
        := Natools.Static_Hash_Maps.S_Expressions.Command_Map.Hash (Key);
   begin
      if Map_2_Keys (N).all = Key then
         return Map_2_Elements (N);
      else
         raise Constraint_Error with "Key """ & Key & """ not in map";
      end if;
   end To_Map_Command;

end Natools.Static_Hash_Maps.S_Expressions.Command_Maps;
