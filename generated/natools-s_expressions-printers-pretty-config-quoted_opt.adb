with Interfaces; use Interfaces;

package body Natools.S_Expressions.Printers.Pretty.Config.Quoted_Opt is

   P : constant array (0 .. 2) of Natural :=
     (2, 12, 19);

   T1 : constant array (0 .. 2) of Unsigned_8 :=
     (3, 14, 14);

   T2 : constant array (0 .. 2) of Unsigned_8 :=
     (15, 16, 7);

   G : constant array (0 .. 18) of Unsigned_8 :=
     (0, 0, 0, 0, 0, 0, 8, 2, 0, 0, 6, 0, 1, 2, 0, 4, 0, 3, 6);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 19;
         F2 := (F2 + Natural (T2 (K)) * J) mod 19;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 9;
   end Hash;

end Natools.S_Expressions.Printers.Pretty.Config.Quoted_Opt;
