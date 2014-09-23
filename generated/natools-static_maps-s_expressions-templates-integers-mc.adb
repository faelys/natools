with Interfaces; use Interfaces;

package body Natools.Static_Maps.S_Expressions.Templates.Integers.MC is

   P : constant array (0 .. 4) of Natural :=
     (1, 2, 5, 6, 9);

   T1 : constant array (0 .. 4) of Unsigned_8 :=
     (15, 31, 41, 38, 11);

   T2 : constant array (0 .. 4) of Unsigned_8 :=
     (10, 33, 15, 23, 26);

   G : constant array (0 .. 44) of Unsigned_8 :=
     (0, 0, 0, 0, 5, 17, 14, 0, 5, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 13, 21,
      0, 0, 0, 11, 18, 7, 0, 16, 4, 2, 9, 0, 0, 15, 0, 0, 0, 2, 3, 7, 13, 0,
      1);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 45;
         F2 := (F2 + Natural (T2 (K)) * J) mod 45;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 22;
   end Hash;

end Natools.Static_Maps.S_Expressions.Templates.Integers.MC;
