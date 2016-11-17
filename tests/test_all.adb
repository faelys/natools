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

-----------------------------------------------------------------------
-- Test_All is a binary gathering all tests from Natools components. --
-----------------------------------------------------------------------

with Ada.Command_Line;
with Ada.Text_IO;
with Natools.Chunked_Strings.Tests;
with Natools.Constant_Indefinite_Ordered_Map_Tests;
with Natools.Cron.Tests;
with Natools.Getopt_Long_Tests;
with Natools.HMAC_Tests;
with Natools.Reference_Tests;
with Natools.Reference_Tests.Pools;
with Natools.S_Expressions.Atom_Buffers.Tests;
with Natools.S_Expressions.Cache_Tests;
with Natools.S_Expressions.Conditionals.Tests;
with Natools.S_Expressions.Dynamic_Interpreter_Tests;
with Natools.S_Expressions.Encodings.Tests;
with Natools.S_Expressions.Enumeration_IO.Tests;
with Natools.S_Expressions.File_RW_Tests;
with Natools.S_Expressions.Interpreter_Tests;
with Natools.S_Expressions.Lockable.Tests;
with Natools.S_Expressions.Parsers.Tests;
with Natools.S_Expressions.Printers.Tests;
with Natools.S_Expressions.Printers.Pretty.Tests;
with Natools.S_Expressions.Printers.Pretty.Config.Tests;
with Natools.S_Expressions.Templates.Tests;
with Natools.Smaz.Tests;
with Natools.Smaz_Tests;
with Natools.Static_Hash_Maps.S_Expressions.Tests;
with Natools.String_Slice_Set_Tests;
with Natools.String_Slice_Tests;
with Natools.Time_IO.Tests;
with Natools.Time_Keys.Tests;
with Natools.Time_Statistics.Tests;
with Natools.Tests.Text_IO;

procedure Test_All is
   package Uneven_Chunked_Strings is new Natools.Chunked_Strings
     (Default_Allocation_Unit => 7,
      Default_Chunk_Size      => 15);
   package Uneven_Chunked_Strings_Tests is new Uneven_Chunked_Strings.Tests;

   package Even_Chunked_Strings is new Natools.Chunked_Strings
     (Default_Allocation_Unit => 6,
      Default_Chunk_Size      => 18);
   package Even_Chunked_Strings_Tests is new Even_Chunked_Strings.Tests;

   package Single_Chunked_Strings is new Natools.Chunked_Strings
     (Default_Allocation_Unit => 10,
      Default_Chunk_Size      => 10);
   package Single_Chunked_Strings_Tests is new Single_Chunked_Strings.Tests;

   Report : Natools.Tests.Text_IO.Text_Reporter;
begin
   Ada.Text_IO.Set_Line_Length (80);
   Report.Section ("All Tests");

   Report.Section ("Chunked_String with uneven allocation unit");
   Uneven_Chunked_Strings_Tests.All_Tests (Report);
   Report.End_Section;

   Report.Section ("Chunked_String with even allocation unit");
   Even_Chunked_Strings_Tests.All_Tests (Report);
   Report.End_Section;

   Report.Section ("Chunked_String with single allocation unit");
   Single_Chunked_Strings_Tests.All_Tests (Report);
   Report.End_Section;

   Report.Section ("Constant_Indefinite_Ordered_Maps");
   Natools.Constant_Indefinite_Ordered_Map_Tests.All_Tests (Report);
   Report.End_Section;

   Report.Section ("Cron");
   Natools.Cron.Tests.All_Tests (Report);
   Report.End_Section;

   Report.Section ("Getopt_Long");
   Natools.Getopt_Long_Tests.All_Tests (Report);
   Report.End_Section;

   Report.Section ("HMAC and GNAT_HMAC");
   Natools.HMAC_Tests.All_Tests (Report);
   Report.End_Section;

   Report.Section ("References");
   Natools.Reference_Tests.All_Tests (Report);
   Natools.Reference_Tests.Test_Task_Safety (Report);
   Report.End_Section;

   Report.Section ("References.Pools");
   Natools.Reference_Tests.Pools.All_Tests (Report);
   Report.End_Section;

   Report.Section ("S_Expressions.Atom_Buffers");
   Natools.S_Expressions.Atom_Buffers.Tests.All_Tests (Report);
   Report.End_Section;

   Report.Section ("S_Expressions.Caches");
   Natools.S_Expressions.Cache_Tests.All_Tests (Report);
   Report.End_Section;

   Report.Section ("S_Expressions.Conditionals");
   Natools.S_Expressions.Conditionals.Tests.All_Tests (Report);
   Report.End_Section;

   Report.Section ("S_Expressions.Dynamic_Interpreters");
   Natools.S_Expressions.Dynamic_Interpreter_Tests.All_Tests (Report);
   Report.End_Section;

   Report.Section ("S_Expressions.Encodings");
   Natools.S_Expressions.Encodings.Tests.All_Tests (Report);
   Report.End_Section;

   Report.Section ("S_Expressions.Enumeration_IO");
   Natools.S_Expressions.Enumeration_IO.Tests.All_Tests (Report);
   Report.End_Section;

   Report.Section ("S_Expressions.File_Readers and File_Writers");
   Natools.S_Expressions.File_RW_Tests.All_Tests (Report);
   Report.End_Section;

   Report.Section ("S_Expressions.Interpreters");
   Natools.S_Expressions.Interpreter_Tests.All_Tests (Report);
   Report.End_Section;

   Report.Section ("S_Expressions.Lockable");
   Natools.S_Expressions.Lockable.Tests.All_Tests (Report);
   Report.End_Section;

   Report.Section ("S_Expressions.Parsers");
   Natools.S_Expressions.Parsers.Tests.All_Tests (Report);
   Report.End_Section;

   Report.Section ("S_Expressions.Printers");
   Natools.S_Expressions.Printers.Tests.All_Tests (Report);
   Report.End_Section;

   Report.Section ("S_Expressions.Printers.Pretty");
   Natools.S_Expressions.Printers.Pretty.Tests.All_Tests (Report);
   Report.End_Section;

   Report.Section ("S_Expressions.Printers.Pretty.Config");
   Natools.S_Expressions.Printers.Pretty.Config.Tests.All_Tests (Report);
   Report.End_Section;

   Report.Section ("S_Expressions.Templates");
   Natools.S_Expressions.Templates.Tests.All_Tests (Report);
   Report.End_Section;

   Report.Section ("Smaz");
   Natools.Smaz_Tests.All_Tests (Report);
   Report.End_Section;

   Report.Section ("Smaz (superseded)");
   Natools.Smaz.Tests.All_Tests (Report);
   Report.End_Section;

   Report.Section ("Static_Hash_Maps.S_Expressions");
   Natools.Static_Hash_Maps.S_Expressions.Tests.All_Tests (Report);
   Report.End_Section;

   Report.Section ("String_Slices");
   Natools.String_Slice_Tests.All_Tests (Report);
   Report.End_Section;

   Report.Section ("String_Slices.Slice_Sets");
   Natools.String_Slice_Set_Tests.All_Tests (Report);
   Report.End_Section;

   Report.Section ("Time_IO");
   Natools.Time_IO.Tests.All_Tests (Report);
   Report.End_Section;

   Report.Section ("Time_Keys");
   Natools.Time_Keys.Tests.All_Tests (Report);
   Report.End_Section;

   Report.Section ("Time_Statistics");
   Natools.Time_Statistics.Tests.All_Tests (Report);
   Report.End_Section;

   Natools.Tests.Text_IO.Print_Results (Report.Total_Results);

   declare
      Results : constant Natools.Tests.Result_Summary := Report.Total_Results;
   begin
      if Results (Natools.Tests.Fail) > 0 or
         Results (Natools.Tests.Error) > 0
      then
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      else
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
      end if;
   end;
   Report.End_Section;
end Test_All;
