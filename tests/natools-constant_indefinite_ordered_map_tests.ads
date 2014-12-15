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

with Natools.Tests;

package Natools.Constant_Indefinite_Ordered_Map_Tests is
   pragma Preelaborate;

   package NT renames Natools.Tests;

   procedure All_Tests (Report : in out NT.Reporter'Class);

   procedure Ada_2012_Errors (Report : in out NT.Reporter'Class);
   procedure Ada_2012_Indexing (Report : in out NT.Reporter'Class);
   procedure Ada_2012_Iteration (Report : in out NT.Reporter'Class);
   procedure Consistency (Report : in out NT.Reporter'Class);
   procedure Cursor_Operations (Report : in out NT.Reporter'Class);
   procedure Direct_Access (Report : in out NT.Reporter'Class);
   procedure Empty_Map (Report : in out NT.Reporter'Class);
   procedure Iterations (Report : in out NT.Reporter'Class);
   procedure Map_Updates (Report : in out NT.Reporter'Class);
   procedure Range_Iteratiors (Report : in out NT.Reporter'Class);
   procedure Unsafe_Map_Roundtrip (Report : in out NT.Reporter'Class);

end Natools.Constant_Indefinite_Ordered_Map_Tests;
