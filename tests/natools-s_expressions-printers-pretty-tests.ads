------------------------------------------------------------------------------
-- Copyright (c) 2014, Natacha Porté                                        --
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
-- Natools.S_Expressions.Printers.Pretty.Tests provides a test suite for    --
-- the S-expression pretty printer.                                         --
------------------------------------------------------------------------------

with Natools.Tests;

package Natools.S_Expressions.Printers.Pretty.Tests is
   pragma Preelaborate (Tests);

   package NT renames Natools.Tests;

   procedure All_Tests (Report : in out NT.Reporter'Class);

   procedure Atom_Encodings (Report : in out NT.Reporter'Class);
   procedure Atom_Width (Report : in out NT.Reporter'Class);
   procedure Basic_Printing (Report : in out NT.Reporter'Class);
   procedure Expression_Width (Report : in out NT.Reporter'Class);
   procedure Indentation (Report : in out NT.Reporter'Class);
   procedure Newline_Formats (Report : in out NT.Reporter'Class);
   procedure Parameter_Mutators (Report : in out NT.Reporter'Class);
   procedure Quoted_String_Escapes (Report : in out NT.Reporter'Class);
   procedure Separators (Report : in out NT.Reporter'Class);
   procedure Tabulation_Width (Report : in out NT.Reporter'Class);
   procedure Token_Separation (Report : in out NT.Reporter'Class);

end Natools.S_Expressions.Printers.Pretty.Tests;
