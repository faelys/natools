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

with Natools.S_Expressions.Parsers;
with Natools.S_Expressions.Templates.Generic_Discrete_Render;
with Natools.S_Expressions.Templates.Tests.Integers;
with Natools.S_Expressions.Templates.Tests.Dates;
with Natools.S_Expressions.Test_Tools;

package body Natools.S_Expressions.Templates.Tests is

   type Day_Name is (Monday, Tuesday, Wednesday, Thursday,
       Friday, Saturday, Sunday);

   procedure Day_Render is new Generic_Discrete_Render (Day_Name);


   -------------------------
   -- Complete Test Suite --
   -------------------------

   procedure All_Tests (Report : in out NT.Reporter'Class) is
   begin
      Test_Date (Report);
      Test_Discrete (Report);
      Test_Integers (Report);
   end All_Tests;


   --------------------------------------
   -- Inidividual Children Test Suites --
   --------------------------------------

   procedure Test_Date (Report : in out NT.Reporter'Class) is
   begin
      Report.Section ("Date templates");
      Natools.S_Expressions.Templates.Tests.Dates.All_Tests (Report);
      Report.End_Section;
   end Test_Date;


   procedure Test_Discrete (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Discrete templates");

      procedure Render_Test
        (Template : in String;
         Value : in Day_Name;
         Expected : in String);

      procedure Render_Test
        (Template : in String;
         Value : in Day_Name;
         Expected : in String)
      is
         Input : aliased Test_Tools.Memory_Stream;
         Output : Test_Tools.Memory_Stream;
         Parser : Parsers.Stream_Parser (Input'Access);
      begin
         Input.Set_Data (To_Atom (Template));
         Test_Tools.Next_And_Check (Test, Parser, Events.Open_List, 1);
         Parser.Next;
         Output.Set_Expected (To_Atom (Expected));
         Day_Render (Output, Parser, Value);
         Output.Check_Stream (Test);
      end Render_Test;
   begin
      Render_Test ("(a b)", Friday, "FRIDAY");
      Render_Test ("(a b)", Monday, "a");
      Render_Test ("(a b)", Tuesday, "b");
      Render_Test ("(a b (c))", Friday, "c");
   exception
      when Error : others => Test.Report_Exception (Error);
   end Test_Discrete;


   procedure Test_Integers (Report : in out NT.Reporter'Class) is
   begin
      Report.Section ("Integer templates");
      Natools.S_Expressions.Templates.Tests.Integers.All_Tests (Report);
      Report.End_Section;
   end Test_Integers;

end Natools.S_Expressions.Templates.Tests;

