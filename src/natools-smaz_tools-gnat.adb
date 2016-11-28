------------------------------------------------------------------------------
-- Copyright (c) 2016, Natacha Porté                                        --
--                                                                          --
-- Permission to use, copy, modify, and distribute this software for any    --
-- purpose with or without fee is hereby granted, provided that the above   --
-- copyright notice and this permission notice appear in all copies.        --
--                                                                          --
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES --
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF         --
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR  --
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES   --
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN    --
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF  --
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.           --
------------------------------------------------------------------------------

with GNAT.Perfect_Hash_Generators;

package body Natools.Smaz_Tools.GNAT is

   package Perfect_Hash_Generators
     renames Standard.GNAT.Perfect_Hash_Generators;


   procedure Build_Perfect_Hash
     (List : in String_Lists.List;
      Package_Name : in String)
   is
      Seed : Natural := 2;
      NK : constant Float := Float (String_Lists.Length (List));
      NV : Natural := Natural (String_Lists.Length (List)) * 2 + 1;
      Retries_Before_Expand : constant := 2;
   begin
      for S of List loop
         Perfect_Hash_Generators.Insert (S);
      end loop;

      Expanding_Retries :
      loop
         Retires_Without_Expand :
         for I in 1 .. Retries_Before_Expand loop
            begin
               Perfect_Hash_Generators.Initialize (Seed, Float (NV) / NK);
               Perfect_Hash_Generators.Compute;
               exit Expanding_Retries;
            exception
               when Perfect_Hash_Generators.Too_Many_Tries => null;
            end;

            Seed := Seed * NV;
         end loop Retires_Without_Expand;

         NV := NV + 1;
         Seed := NV;
      end loop Expanding_Retries;

      Perfect_Hash_Generators.Produce (Package_Name);
      Perfect_Hash_Generators.Finalize;
   exception
      when others =>
         Perfect_Hash_Generators.Finalize;
         raise;
   end Build_Perfect_Hash;

end Natools.Smaz_Tools.GNAT;
