with Interfaces; use Interfaces;

package body Natools.S_Expressions.Printers.Pretty.Config.Quoted_Esc is

   P : constant array (0 .. 2) of Natural :=
     (1, 4, 10);

   T1 : constant array (0 .. 2) of Unsigned_8 :=
     (5, 4, 2);

   T2 : constant array (0 .. 2) of Unsigned_8 :=
     (8, 2, 3);

   G : constant array (0 .. 16) of Unsigned_8 :=
     (0, 0, 0, 4, 0, 0, 0, 2, 0, 0, 7, 0, 4, 0, 3, 0, 2);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 17;
         F2 := (F2 + Natural (T2 (K)) * J) mod 17;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 8;
   end Hash;

end Natools.S_Expressions.Printers.Pretty.Config.Quoted_Esc;
