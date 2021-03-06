with Interfaces; use Interfaces;

package body Natools.S_Expressions.Printers.Pretty.Config.Newline_Cmd is

   P : constant array (0 .. 2) of Natural :=
     (2, 3, 7);

   T1 : constant array (0 .. 2) of Unsigned_8 :=
     (11, 29, 8);

   T2 : constant array (0 .. 2) of Unsigned_8 :=
     (43, 5, 24);

   G : constant array (0 .. 48) of Unsigned_8 :=
     (0, 6, 0, 6, 0, 23, 0, 0, 0, 0, 0, 0, 0, 21, 0, 0, 0, 4, 14, 1, 0, 0,
      15, 15, 12, 0, 9, 5, 14, 0, 21, 7, 0, 0, 0, 3, 0, 0, 1, 12, 0, 0, 16,
      0, 2, 0, 19, 1, 12);

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

end Natools.S_Expressions.Printers.Pretty.Config.Newline_Cmd;
