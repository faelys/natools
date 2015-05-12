------------------------------------------------------------------------------
-- Copyright (c) 2015, Natacha Porté                                        --
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
-- Natools.S_Expressions.Enumeration_IO provides subprograms to help with   --
-- I/O of enumerated type values. It involves mostly the conversion between --
-- Ada-style names (underscore-separated uppercase words) and S-expression  --
-- style (hyphen-separated lowercase words).                                --
------------------------------------------------------------------------------

package Natools.S_Expressions.Enumeration_IO is
   pragma Preelaborate;

   function To_Atom (Enumeration_Image : in String) return Atom;
      --  Convert the output of a 'Image function into a S-expression atom

   function To_Image (Data : in Atom) return String;
      --  Convert an atom into a string similar to 'Image output


   generic
      type Enum is (<>);
   package Typed_IO is

      function Image (T : Enum) return Atom;
         --  Convert an enumeration value into an atom

      function Value (Data : Atom) return Enum;
         --  Convert an atom into an enumeration value

   end Typed_IO;

end Natools.S_Expressions.Enumeration_IO;
