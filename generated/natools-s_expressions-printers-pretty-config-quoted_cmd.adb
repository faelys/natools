with Interfaces; use Interfaces;

package body Natools.S_Expressions.Printers.Pretty.Config.Quoted_Cmd is

   P : constant array (0 .. 2) of Natural :=
     (1, 4, 10);

   T1 : constant array (0 .. 2) of Unsigned_8 :=
     (11, 3, 3);

   T2 : constant array (0 .. 2) of Unsigned_8 :=
     (14, 19, 16);

   G : constant array (0 .. 22) of Unsigned_8 :=
     (2, 0, 0, 10, 0, 0, 0, 1, 0, 0, 5, 9, 0, 0, 1, 10, 0, 3, 0, 7, 0, 6, 0);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 23;
         F2 := (F2 + Natural (T2 (K)) * J) mod 23;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 11;
   end Hash;

end Natools.S_Expressions.Printers.Pretty.Config.Quoted_Cmd;
