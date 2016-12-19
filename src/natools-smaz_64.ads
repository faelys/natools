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
-- Natools.Smaz_Implementations.Base_64 provides the subprograms needed to  --
-- instantiate Natools.Smaz_Generic into a variant of the Smaz compression  --
-- algorithm that output directly base-64 printable symbols, but with a     --
-- dictionary containing at most 61 elements.                               --
-- See Natools.Smaz_Implementations.Base_64 comments for the implementation --
-- details.                                                                 --
------------------------------------------------------------------------------

with Natools.Smaz_Generic;
with Natools.Smaz_Implementations.Base_64;
with Natools.Smaz_Implementations.Base_64_Tools;

package Natools.Smaz_64 is new Natools.Smaz_Generic
  (Dictionary_Code => Natools.Smaz_Implementations.Base_64_Tools.Base_64_Digit,
   Read_Code => Natools.Smaz_Implementations.Base_64.Read_Code,
   Read_Verbatim => Natools.Smaz_Implementations.Base_64.Read_Verbatim,
   Skip_Verbatim => Natools.Smaz_Implementations.Base_64.Skip_Verbatim,
   Verbatim_Size => Natools.Smaz_Implementations.Base_64.Verbatim_Size,
   Write_Code => Natools.Smaz_Implementations.Base_64.Write_Code,
   Write_Verbatim => Natools.Smaz_Implementations.Base_64.Write_Verbatim);

pragma Pure (Natools.Smaz_64);
