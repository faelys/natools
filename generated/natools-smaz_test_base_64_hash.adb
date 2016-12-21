with Interfaces; use Interfaces;

package body Natools.Smaz_Test_Base_64_Hash is

   P : constant array (0 .. 3) of Natural :=
     (1, 2, 3, 4);

   T1 : constant array (0 .. 3) of Unsigned_8 :=
     (97, 46, 76, 56);

   T2 : constant array (0 .. 3) of Unsigned_8 :=
     (81, 17, 92, 120);

   G : constant array (0 .. 120) of Unsigned_8 :=
     (0, 0, 0, 0, 0, 0, 0, 0, 22, 0, 9, 0, 42, 0, 0, 0, 2, 0, 0, 14, 0, 0,
      0, 0, 0, 0, 0, 0, 5, 0, 14, 0, 52, 0, 39, 6, 6, 7, 18, 0, 24, 0, 36,
      0, 17, 38, 44, 50, 0, 0, 0, 0, 0, 0, 0, 0, 0, 30, 11, 56, 0, 32, 0, 0,
      0, 0, 0, 0, 25, 41, 6, 0, 47, 0, 27, 0, 33, 7, 0, 0, 33, 43, 36, 0,
      28, 0, 39, 0, 39, 48, 41, 0, 0, 16, 18, 53, 9, 29, 43, 0, 0, 34, 49,
      0, 0, 0, 5, 0, 47, 59, 53, 23, 0, 5, 22, 46, 29, 34, 28, 3, 4);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 121;
         F2 := (F2 + Natural (T2 (K)) * J) mod 121;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 60;
   end Hash;

end Natools.Smaz_Test_Base_64_Hash;
