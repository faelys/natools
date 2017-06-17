with Interfaces; use Interfaces;

package body Natools.Static_Maps.S_Expressions.Conditionals.Strings.P is

   P : constant array (0 .. 2) of Natural :=
     (1, 11, 13);

   T1 : constant array (0 .. 2) of Unsigned_8 :=
     (11, 3, 3);

   T2 : constant array (0 .. 2) of Unsigned_8 :=
     (14, 19, 16);

   G : constant array (0 .. 22) of Unsigned_8 :=
     (0, 0, 0, 9, 0, 0, 0, 0, 2, 0, 3, 0, 0, 3, 1, 0, 7, 0, 4, 0, 1, 7, 0);

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

end Natools.Static_Maps.S_Expressions.Conditionals.Strings.P;
