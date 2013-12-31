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
-- Natools.S_Expressions.Test_Tools provides tools used in S-expression     --
-- test suites.                                                             --
------------------------------------------------------------------------------

with Natools.Tests;

package Natools.S_Expressions.Test_Tools is
   pragma Preelaborate (Test_Tools);

   package NT renames Natools.Tests;

   procedure Dump_Atom
     (Report : in out NT.Reporter'Class;
      Data : in Atom;
      Label : in String := "");
      --  Dump contents on Data as info in Report

   procedure Test_Atom
     (Report : in out NT.Reporter'Class;
      Test_Name : in String;
      Expected : in Atom;
      Found : in Atom);
      --  Report success when Found is equal to Expected, and failure
      --  with diagnostics otherwise.

end Natools.S_Expressions.Test_Tools;
