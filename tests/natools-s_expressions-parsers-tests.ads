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
-- Natools.S_Expressions.Parsers.Tests provides a test suite for the        --
-- S-expression stream parser.                                              --
------------------------------------------------------------------------------

with Natools.Tests;

package Natools.S_Expressions.Parsers.Tests is
   pragma Preelaborate (Tests);

   package NT renames Natools.Tests;

   procedure All_Tests (Report : in out NT.Reporter'Class);

   procedure Atom_Encodings (Report : in out NT.Reporter'Class);
   procedure Base64_Subexpression (Report : in out NT.Reporter'Class);
   procedure Canonical_Encoding (Report : in out NT.Reporter'Class);
   procedure Lockable_Interface (Report : in out NT.Reporter'Class);
   procedure Nested_Subpexression (Report : in out NT.Reporter'Class);
   procedure Number_Prefixes (Report : in out NT.Reporter'Class);
   procedure Quoted_Escapes (Report : in out NT.Reporter'Class);
   procedure Special_Subexpression (Report : in out NT.Reporter'Class);

end Natools.S_Expressions.Parsers.Tests;
