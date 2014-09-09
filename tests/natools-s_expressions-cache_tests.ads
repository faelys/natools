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
-- Natools.S_Expressions.Cache_Tests provides tests for generic memeory     --
-- cache in Natools.S_Expressions.Generic_Caches and its default            --
-- instantiation.                                                           --
------------------------------------------------------------------------------

with Natools.Tests;

package Natools.S_Expressions.Cache_Tests is

   package NT renames Natools.Tests;

   procedure All_Tests (Report : in out NT.Reporter'Class);

   procedure Debug_Instantiation (Report : in out NT.Reporter'Class);
   procedure Default_Instantiation (Report : in out NT.Reporter'Class);
   procedure Descriptor_Interface (Report : in out NT.Reporter'Class);
   procedure Lockable_Interface (Report : in out NT.Reporter'Class);
   procedure Replayable_Interface (Report : in out NT.Reporter'Class);

end Natools.S_Expressions.Cache_Tests;
