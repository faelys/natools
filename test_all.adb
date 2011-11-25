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
with Natools.Getopt_Long_Tests;
with Natools.Tests.Text_IO;

procedure Test_All is
   Report : Natools.Tests.Text_IO.Text_Reporter;
begin
   Ada.Text_IO.Set_Line_Length (80);
   Report.Section ("All Tests");

   Report.Section ("Getopt_Long");
   Natools.Getopt_Long_Tests.All_Tests (Report);
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
