with Interfaces; use Interfaces;

package body Natools.S_Expressions.Printers.Pretty.Config.Commands.CE is

   P : constant array (0 .. 2) of Natural :=
     (1, 4, 6);

   T1 : constant array (0 .. 2) of Unsigned_8 :=
     (20, 19, 0);

   T2 : constant array (0 .. 2) of Unsigned_8 :=
     (9, 10, 8);

   G : constant array (0 .. 20) of Unsigned_8 :=
     (0, 0, 9, 6, 0, 5, 1, 0, 0, 0, 0, 0, 0, 0, 5, 0, 4, 9, 2, 0, 1);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 21;
         F2 := (F2 + Natural (T2 (K)) * J) mod 21;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 10;
   end Hash;

end Natools.S_Expressions.Printers.Pretty.Config.Commands.CE;
