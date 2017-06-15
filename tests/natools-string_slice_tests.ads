------------------------------------------------------------------------------
-- Copyright (c) 2013-2017, Natacha Porté                                   --
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
-- Natools.String_Slice_Tests is a test suite for Natools.String_Slices.    --
------------------------------------------------------------------------------

with Natools.Tests;

package Natools.String_Slice_Tests is
   pragma Preelaborate (String_Slice_Tests);

   package NT renames Natools.Tests;

   procedure All_Tests (Report : in out NT.Reporter'Class);

   procedure Range_Tests (Report : in out NT.Reporter'Class);
   procedure Slice_Tests (Report : in out NT.Reporter'Class);

   procedure Test_Is_In (Report : in out NT.Reporter'Class);
   procedure Test_Is_Subrange (Report : in out NT.Reporter'Class);
   procedure Test_Range_Image (Report : in out NT.Reporter'Class);
   procedure Test_Set_Length (Report : in out NT.Reporter'Class);

   procedure Test_Conversions (Report : in out NT.Reporter'Class);
   procedure Test_Extensions (Report : in out NT.Reporter'Class);
   procedure Test_Incoming_Range (Report : in out NT.Reporter'Class);
   procedure Test_Invalid_Extensions (Report : in out NT.Reporter'Class);
   procedure Test_Invalid_Subslices (Report : in out NT.Reporter'Class);
   procedure Test_New_Slice (Report : in out NT.Reporter'Class);
   procedure Test_Null_Slice (Report : in out NT.Reporter'Class);
   procedure Test_Outgoing_Range (Report : in out NT.Reporter'Class);
   procedure Test_Slice_Relations (Report : in out NT.Reporter'Class);
   procedure Test_Subslices (Report : in out NT.Reporter'Class);

end Natools.String_Slice_Tests;
