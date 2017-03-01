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

with Natools.Tests;

package Natools.Smaz_Tests is

   package NT renames Natools.Tests;

   procedure All_Tests (Report : in out NT.Reporter'Class);

   procedure All_Tests_256 (Report : in out NT.Reporter'Class);
   procedure All_Tests_4096 (Report : in out NT.Reporter'Class);
   procedure All_Tests_64 (Report : in out NT.Reporter'Class);

   procedure Sample_Strings_256 (Report : in out NT.Reporter'Class);
   procedure Sample_Strings_VLV_256 (Report : in out NT.Reporter'Class);
   procedure Test_Validity_256 (Report : in out NT.Reporter'Class);

   procedure Sample_Strings_4096 (Report : in out NT.Reporter'Class);
   procedure Sample_Strings_VLV_4096 (Report : in out NT.Reporter'Class);
   procedure Test_Validity_4096 (Report : in out NT.Reporter'Class);

   procedure Impure_Stream_64 (Report : in out NT.Reporter'Class);
   procedure Sample_Strings_64 (Report : in out NT.Reporter'Class);
   procedure Sample_Strings_VLV_64 (Report : in out NT.Reporter'Class);
   procedure Test_Validity_64 (Report : in out NT.Reporter'Class);

end Natools.Smaz_Tests;
