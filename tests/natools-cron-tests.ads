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
-- Natools.Cron.Tests provides a test suite for Natools.Cron.               --
------------------------------------------------------------------------------

with Natools.Tests;

package Natools.Cron.Tests is

   package NT renames Natools.Tests;

   procedure All_Tests (Report : in out NT.Reporter'Class);

   procedure Basic_Usage (Report : in out NT.Reporter'Class);
   procedure Delete_While_Busy (Report : in out NT.Reporter'Class);
   procedure Insert_While_Busy (Report : in out NT.Reporter'Class);
   procedure Time_Collision (Report : in out NT.Reporter'Class);

private

   type Bounded_String (Max_Size : Natural) is record
      Data : String (1 .. Max_Size);
      Size : Natural := 0;
   end record;

   procedure Append (S : in out Bounded_String; C : Character);
   function Get (S : Bounded_String) return String;
   procedure Reset (S : in out Bounded_String);


   type Test_Callback (Backend : access Bounded_String) is new Callback with
   record
      Symbol : Character;
   end record;

   overriding procedure Run (Self : in out Test_Callback);


   type Long_Callback (Backend : access Bounded_String) is new Callback with
   record
      Open, Close : Character;
      Wait : Duration;
   end record;

   overriding procedure Run (Self : in out Long_Callback);

end Natools.Cron.Tests;
