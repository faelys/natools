with Interfaces; use Interfaces;

package body Natools.S_Expressions.Printers.Pretty.Config.Newline_Enc is

   P : constant array (0 .. 1) of Natural :=
     (1, 3);

   T1 : constant array (0 .. 1) of Unsigned_8 :=
     (24, 21);

   T2 : constant array (0 .. 1) of Unsigned_8 :=
     (5, 8);

   G : constant array (0 .. 24) of Unsigned_8 :=
     (0, 0, 0, 4, 0, 0, 0, 3, 0, 9, 0, 0, 11, 0, 0, 0, 8, 3, 4, 9, 1, 7, 0,
      0, 2);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 25;
         F2 := (F2 + Natural (T2 (K)) * J) mod 25;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 12;
   end Hash;

end Natools.S_Expressions.Printers.Pretty.Config.Newline_Enc;
