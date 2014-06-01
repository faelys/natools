with Interfaces; use Interfaces;

package body Natools.Static_Hash_Maps.S_Expressions.Command_Pkg is

   P : constant array (0 .. 1) of Natural :=
     (3, 11);

   T1 : constant array (0 .. 1) of Unsigned_8 :=
     (0, 1);

   T2 : constant array (0 .. 1) of Unsigned_8 :=
     (6, 12);

   G : constant array (0 .. 16) of Unsigned_8 :=
     (0, 2, 0, 6, 4, 0, 0, 0, 0, 0, 3, 0, 0, 0, 5, 0, 1);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 17;
         F2 := (F2 + Natural (T2 (K)) * J) mod 17;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 7;
   end Hash;

end Natools.Static_Hash_Maps.S_Expressions.Command_Pkg;
