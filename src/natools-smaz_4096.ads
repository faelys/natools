------------------------------------------------------------------------------
-- Copyright (c) 2016-2017, Natacha Porté                                   --
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
-- Natools.Smaz_4096 provides the subprograms needed to instantiate         --
-- Natools.Smaz_Generic into a variant of the Smaz compression algorithm    --
-- that output directly base-64 printable symbols, but with a dictionary    --
-- indexed by two symbols, allowing a maximum size of 4095 entries.         --
------------------------------------------------------------------------------

with Natools.Smaz_Generic;
with Natools.Smaz_Implementations.Base_4096;

package Natools.Smaz_4096 is new Natools.Smaz_Generic
  (Dictionary_Code => Natools.Smaz_Implementations.Base_4096.Base_4096_Digit,
   Read_Code => Natools.Smaz_Implementations.Base_4096.Read_Code,
   Read_Verbatim => Natools.Smaz_Implementations.Base_4096.Read_Verbatim,
   Skip_Verbatim => Natools.Smaz_Implementations.Base_4096.Skip_Verbatim,
   Verbatim_Size => Natools.Smaz_Implementations.Base_4096.Verbatim_Size,
   Write_Code => Natools.Smaz_Implementations.Base_4096.Write_Code,
   Write_Verbatim => Natools.Smaz_Implementations.Base_4096.Write_Verbatim);

pragma Pure (Natools.Smaz_4096);
