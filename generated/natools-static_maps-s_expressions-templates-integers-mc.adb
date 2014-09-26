with Interfaces; use Interfaces;

package body Natools.Static_Maps.S_Expressions.Templates.Integers.MC is

   P : constant array (0 .. 4) of Natural :=
     (1, 2, 5, 6, 9);

   T1 : constant array (0 .. 4) of Unsigned_8 :=
     (0, 40, 7, 12, 15);

   T2 : constant array (0 .. 4) of Unsigned_8 :=
     (31, 48, 43, 13, 1);

   G : constant array (0 .. 48) of Unsigned_8 :=
     (0, 14, 0, 0, 0, 0, 0, 18, 0, 17, 10, 5, 0, 18, 0, 0, 18, 8, 0, 16, 18,
      0, 0, 0, 0, 15, 23, 0, 1, 4, 13, 0, 0, 0, 0, 3, 5, 17, 0, 2, 0, 14, 6,
      0, 10, 21, 0, 0, 0);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 49;
         F2 := (F2 + Natural (T2 (K)) * J) mod 49;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 24;
   end Hash;

end Natools.Static_Maps.S_Expressions.Templates.Integers.MC;
