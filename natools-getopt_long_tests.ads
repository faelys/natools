------------------------------------------------------------------------------
-- Copyright (c) 2011, Natacha Porté                                        --
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
-- Natools.Getopt_Long_Tests is a test suite for Natools.Getopt_Long        --
-- command-line argument processing facilities.                             --
------------------------------------------------------------------------------

with Natools.Tests;

package Natools.Getopt_Long_Tests is
   pragma Preelaborate (Getopt_Long_Tests);

   package NT renames Natools.Tests;

   procedure All_Tests (Report : in out NT.Reporter'Class);

   procedure Test_Arguments (Report : in out NT.Reporter'Class);
   procedure Test_Empty (Report : in out NT.Reporter'Class);
   procedure Test_Error_Callbacks (Report : in out NT.Reporter'Class);
   procedure Test_Everything (Report : in out NT.Reporter'Class);
   procedure Test_Long (Report : in out NT.Reporter'Class);
   procedure Test_Long_Only (Report : in out NT.Reporter'Class);
   procedure Test_Long_Partial (Report : in out NT.Reporter'Class);
   procedure Test_Long_Partial_Ambiguous (Report : in out NT.Reporter'Class);
   procedure Test_Missing_Argument_Long (Report : in out NT.Reporter'Class);
   procedure Test_Missing_Argument_Short (Report : in out NT.Reporter'Class);
   procedure Test_Mixed_Arg (Report : in out NT.Reporter'Class);
   procedure Test_Mixed_No_Arg (Report : in out NT.Reporter'Class);
   procedure Test_Posixly_Correct (Report : in out NT.Reporter'Class);
   procedure Test_Short_Argument (Report : in out NT.Reporter'Class);
   procedure Test_Short_Compact (Report : in out NT.Reporter'Class);
   procedure Test_Short_Expanded (Report : in out NT.Reporter'Class);
   procedure Test_Unexpected_Argument (Report : in out NT.Reporter'Class);
   procedure Test_Unknown_Long (Report : in out NT.Reporter'Class);
   procedure Test_Unknown_Short (Report : in out NT.Reporter'Class);

end Natools.Getopt_Long_Tests;
