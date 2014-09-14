with Interfaces; use Interfaces;

package body Natools.Static_Maps.S_Expressions.Templates.Integers.MC is

   P : constant array (0 .. 3) of Natural :=
     (1, 2, 5, 9);

   T1 : constant array (0 .. 3) of Unsigned_8 :=
     (31, 27, 5, 36);

   T2 : constant array (0 .. 3) of Unsigned_8 :=
     (3, 33, 25, 27);

   G : constant array (0 .. 40) of Unsigned_8 :=
     (0, 0, 17, 16, 0, 17, 0, 0, 0, 0, 0, 12, 1, 12, 0, 7, 2, 9, 11, 4, 6,
      0, 17, 0, 0, 0, 0, 16, 5, 0, 1, 0, 0, 15, 0, 0, 0, 0, 13, 0, 7);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 41;
         F2 := (F2 + Natural (T2 (K)) * J) mod 41;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 20;
   end Hash;

end Natools.Static_Maps.S_Expressions.Templates.Integers.MC;
