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
-- Natools.S_Expressions.Templates.Tests.Integers provides a test suite for --
-- integer S-expression template system.                                    --
------------------------------------------------------------------------------

package Natools.S_Expressions.Templates.Tests.Integers is
   pragma Preelaborate;

   procedure All_Tests (Report : in out NT.Reporter'Class);

   procedure Alignment (Report : in out NT.Reporter'Class);
   procedure Default_Format (Report : in out NT.Reporter'Class);
   procedure Explicit_Default_Format (Report : in out NT.Reporter'Class);
   procedure Explicit_Images (Report : in out NT.Reporter'Class);
   procedure Explicit_Sign (Report : in out NT.Reporter'Class);
   procedure Hexadecimal (Report : in out NT.Reporter'Class);
   procedure Overflow (Report : in out NT.Reporter'Class);
   procedure Parse_Errors (Report : in out NT.Reporter'Class);
   procedure Static_Hash_Map (Report : in out NT.Reporter'Class);

end Natools.S_Expressions.Templates.Tests.Integers;
