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

------------------------------------------------------------------------------
-- Natools.Smaz.Tools.GNAT gather tools to handle Smaz diction aries that   --
-- depend so heavily on GNAT that they cannot be considered portable.       --
------------------------------------------------------------------------------

package Natools.Smaz.Tools.GNAT is

   procedure Build_Perfect_Hash
     (List : in String_Lists.List;
      Package_Name : in String)
     with Pre => (for all S of List => S'First = 1);
      --  Generate a static hash function that can be used with a dictionary
      --  built from List, using GNAT.Perfect_Hash_Generators.
      --  The precondition reflects an assumption made by
      --  GNAT.Perfect_Hash_Generators but not enforced,
      --  which leads to nasty bugs.

end Natools.Smaz.Tools.GNAT;
