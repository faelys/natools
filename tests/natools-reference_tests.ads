------------------------------------------------------------------------------
-- Copyright (c) 2013-2014, Natacha Porté                                   --
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
-- Natools.Reference_Tests is a test suite for Natools.References           --
-- reference-counted object holder.                                         --
-- Note that the task-safety test is quite long and often reports success   --
-- on task-unsafe code when run on a single core. For these reasons, it is  --
-- not used by All_Tests.                                                   --
------------------------------------------------------------------------------

with Natools.Tests;

private with Ada.Finalization;
private with GNAT.Debug_Pools;
private with Natools.References;
private with System.Storage_Pools;

package Natools.Reference_Tests is

   package NT renames Natools.Tests;

   procedure All_Tests (Report : in out NT.Reporter'Class);
      --  All tests except Test_Task_Safety (see the Note above)

   procedure Test_Data_Access (Report : in out NT.Reporter'Class);
   procedure Test_Double_Finalize (Report : in out NT.Reporter'Class);
   procedure Test_Instance_Counts (Report : in out NT.Reporter'Class);
   procedure Test_Reference_Counts (Report : in out NT.Reporter'Class);
   procedure Test_Reference_Tests (Report : in out NT.Reporter'Class);

   procedure Test_Task_Safety (Report : in out NT.Reporter'Class);

private

   Instance_Count : Integer := 0;

   type Counter is new Ada.Finalization.Limited_Controlled with record
      Instance_Number : Natural := 0;
   end record;

   function Factory return Counter;
   overriding procedure Initialize (Object : in out Counter);
   overriding procedure Finalize (Object : in out Counter);

   Pool : GNAT.Debug_Pools.Debug_Pool;

   package Refs is new Natools.References
     (Counter,
      System.Storage_Pools.Root_Storage_Pool'Class (Pool),
      System.Storage_Pools.Root_Storage_Pool'Class (Pool));

end Natools.Reference_Tests;
