with Interfaces; use Interfaces;

package body Natools.Static_Maps.S_Expressions.Templates.Dates.Cmds is

   P : constant array (0 .. 5) of Natural :=
     (1, 2, 5, 11, 12, 15);

   T1 : constant array (0 .. 5) of Unsigned_8 :=
     (3, 35, 48, 42, 15, 1);

   T2 : constant array (0 .. 5) of Unsigned_8 :=
     (30, 30, 25, 29, 48, 10);

   G : constant array (0 .. 60) of Unsigned_8 :=
     (0, 0, 0, 0, 0, 22, 0, 0, 12, 11, 0, 0, 0, 0, 0, 0, 13, 5, 0, 26, 17,
      0, 0, 6, 9, 0, 0, 0, 0, 10, 0, 24, 0, 0, 0, 0, 7, 6, 28, 16, 23, 0, 0,
      4, 28, 7, 21, 0, 8, 1, 9, 2, 0, 1, 5, 29, 0, 0, 3, 19, 0);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 61;
         F2 := (F2 + Natural (T2 (K)) * J) mod 61;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 30;
   end Hash;

end Natools.Static_Maps.S_Expressions.Templates.Dates.Cmds;
