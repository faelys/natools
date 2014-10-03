with Interfaces; use Interfaces;

package body Natools.Static_Maps.S_Expressions.Templates.Dates.Cmds is

   P : constant array (0 .. 5) of Natural :=
     (1, 2, 5, 11, 12, 15);

   T1 : constant array (0 .. 5) of Unsigned_8 :=
     (5, 22, 23, 31, 6, 10);

   T2 : constant array (0 .. 5) of Unsigned_8 :=
     (42, 40, 11, 44, 54, 41);

   G : constant array (0 .. 57) of Unsigned_8 :=
     (0, 0, 0, 0, 9, 0, 22, 3, 0, 0, 1, 11, 4, 0, 0, 0, 17, 0, 5, 0, 13, 0,
      0, 0, 0, 0, 0, 0, 0, 16, 0, 2, 0, 15, 0, 0, 5, 18, 0, 0, 0, 0, 6, 10,
      10, 20, 23, 0, 3, 3, 27, 16, 0, 20, 21, 9, 0, 22);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 58;
         F2 := (F2 + Natural (T2 (K)) * J) mod 58;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 28;
   end Hash;

end Natools.Static_Maps.S_Expressions.Templates.Dates.Cmds;
