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
with Natools.Getopt_Long_Tests;
with Natools.Reference_Tests;
with Natools.S_Expressions.Atom_Buffers.Tests;
with Natools.S_Expressions.Cache_Tests;
with Natools.S_Expressions.Encodings.Tests;
with Natools.S_Expressions.Interpreter_Tests;
with Natools.S_Expressions.Lockable.Tests;
with Natools.S_Expressions.Parsers.Tests;
with Natools.S_Expressions.Printers.Tests;
with Natools.S_Expressions.Printers.Pretty.Tests;
with Natools.S_Expressions.Printers.Pretty.Config.Tests;
with Natools.String_Slice_Set_Tests;
with Natools.String_Slice_Tests;
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

   Report.Section ("Getopt_Long");
   Natools.Getopt_Long_Tests.All_Tests (Report);
   Report.End_Section;

   Report.Section ("References");
   Natools.Reference_Tests.All_Tests (Report);
   Report.End_Section;

   Report.Section ("S_Expressions.Atom_Buffers");
   Natools.S_Expressions.Atom_Buffers.Tests.All_Tests (Report);
   Report.End_Section;

   Report.Section ("S_Expressions.Caches");
   Natools.S_Expressions.Cache_Tests.All_Tests (Report);
   Report.End_Section;

   Report.Section ("S_Expressions.Encodings");
   Natools.S_Expressions.Encodings.Tests.All_Tests (Report);
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

   Report.Section ("String_Slices");
   Natools.String_Slice_Tests.All_Tests (Report);
   Report.End_Section;

   Report.Section ("String_Slices.Slice_Sets");
   Natools.String_Slice_Set_Tests.All_Tests (Report);
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
