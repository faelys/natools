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
-- Natools.Smaz_256 is the instantion of Natools.Smaz_Generic equivalent    --
-- to the original byte-based Smaz compression algorithm.                   --
------------------------------------------------------------------------------

with Ada.Streams;
with Natools.Smaz_Generic;
with Natools.Smaz_Implementations.Base_256;

package Natools.Smaz_256 is new Natools.Smaz_Generic
  (Dictionary_Code => Ada.Streams.Stream_Element,
   Read_Code => Natools.Smaz_Implementations.Base_256.Read_Code,
   Read_Verbatim => Natools.Smaz_Implementations.Base_256.Read_Verbatim,
   Skip_Verbatim => Natools.Smaz_Implementations.Base_256.Skip_Verbatim,
   Verbatim_Size => Natools.Smaz_Implementations.Base_256.Verbatim_Size,
   Write_Code => Natools.Smaz_Implementations.Base_256.Write_Code,
   Write_Verbatim => Natools.Smaz_Implementations.Base_256.Write_Verbatim);

pragma Pure (Natools.Smaz_256);
