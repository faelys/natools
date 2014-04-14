------------------------------------------------------------------------------
-- Copyright (c) 2013, Natacha Porté                                        --
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
-- Natools.S_Expressions.Encodings.Tests is a test suite for hexadecimal    --
-- and base-64 encoding and decoding in S-expressions                       --
------------------------------------------------------------------------------

with Natools.Tests;

package Natools.S_Expressions.Encodings.Tests is
   pragma Preelaborate (Tests);

   package NT renames Natools.Tests;

   procedure All_Tests (Report : in out NT.Reporter'Class);
      --  Run all available tests

   procedure Hexadecimal_Test (Report : in out NT.Reporter'Class);
   procedure Base64_Test (Report : in out NT.Reporter'Class);
   procedure User_Base64_Test (Report : in out NT.Reporter'Class);
      --  Run test suite for each encoding type

end Natools.S_Expressions.Encodings.Tests;
