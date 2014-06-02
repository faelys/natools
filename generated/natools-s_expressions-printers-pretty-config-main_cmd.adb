with Interfaces; use Interfaces;

package body Natools.S_Expressions.Printers.Pretty.Config.Main_Cmd is

   P : constant array (0 .. 4) of Natural :=
     (1, 4, 6, 10, 19);

   T1 : constant array (0 .. 4) of Unsigned_8 :=
     (90, 49, 2, 41, 29);

   T2 : constant array (0 .. 4) of Unsigned_8 :=
     (4, 57, 54, 101, 3);

   G : constant array (0 .. 114) of Unsigned_8 :=
     (42, 53, 32, 0, 0, 0, 0, 0, 49, 0, 0, 5, 0, 0, 24, 15, 0, 52, 0, 0, 0,
      0, 0, 0, 13, 0, 24, 0, 0, 0, 33, 0, 0, 40, 0, 0, 16, 0, 0, 0, 21, 43,
      0, 39, 41, 0, 0, 51, 0, 52, 30, 0, 52, 0, 0, 29, 0, 35, 0, 0, 0, 0, 0,
      0, 28, 13, 0, 0, 0, 7, 0, 14, 0, 27, 3, 0, 0, 55, 44, 0, 31, 24, 25,
      41, 0, 24, 5, 31, 56, 0, 0, 21, 27, 49, 49, 13, 0, 0, 39, 45, 0, 0,
      40, 0, 0, 41, 13, 2, 50, 12, 41, 23, 1, 0, 0);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 115;
         F2 := (F2 + Natural (T2 (K)) * J) mod 115;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 57;
   end Hash;

end Natools.S_Expressions.Printers.Pretty.Config.Main_Cmd;
