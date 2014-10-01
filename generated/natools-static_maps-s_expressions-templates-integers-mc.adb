with Interfaces; use Interfaces;

package body Natools.Static_Maps.S_Expressions.Templates.Integers.MC is

   P : constant array (0 .. 4) of Natural :=
     (1, 2, 5, 6, 9);

   T1 : constant array (0 .. 4) of Unsigned_8 :=
     (10, 36, 34, 8, 27);

   T2 : constant array (0 .. 4) of Unsigned_8 :=
     (6, 49, 31, 26, 30);

   G : constant array (0 .. 50) of Unsigned_8 :=
     (0, 10, 0, 6, 0, 0, 0, 0, 8, 0, 0, 4, 0, 0, 11, 0, 4, 14, 1, 0, 0, 0,
      0, 0, 7, 0, 6, 0, 0, 5, 0, 0, 21, 0, 17, 0, 16, 7, 23, 0, 20, 10, 0,
      0, 2, 1, 10, 2, 0, 19, 3);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 51;
         F2 := (F2 + Natural (T2 (K)) * J) mod 51;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 25;
   end Hash;

end Natools.Static_Maps.S_Expressions.Templates.Integers.MC;
