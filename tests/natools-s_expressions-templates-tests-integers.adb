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
with Natools.S_Expressions.Test_Tools;
with Natools.S_Expressions.Templates.Integers;
with Natools.Static_Maps.S_Expressions.Templates.Integers.T;

package body Natools.S_Expressions.Templates.Tests.Integers is

   procedure Test_Render
     (Test : in out NT.Test;
      Template : in String;
      Value : in Integer;
      Expected : in String);
      --  Run Template with Value and compare the result with Expected


   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

   procedure Test_Render
     (Test : in out NT.Test;
      Template : in String;
      Value : in Integer;
      Expected : in String)
   is
      Input : aliased Test_Tools.Memory_Stream;
      Output : Test_Tools.Memory_Stream;
      Parser : Parsers.Stream_Parser (Input'Access);
   begin
      Input.Set_Data (To_Atom (Template));
      Parser.Next;
      Output.Set_Expected (To_Atom (Expected));
      Templates.Integers.Render (Output, Parser, Value);
      Output.Check_Stream (Test);
   end Test_Render;



   -------------------------
   -- Complete Test Suite --
   -------------------------

   procedure All_Tests (Report : in out NT.Reporter'Class) is
   begin
      Alignment (Report);
      Default_Format (Report);
      Explicit_Sign (Report);
      Hexadecimal (Report);
      Overflow (Report);
      Parse_Errors (Report);
      Static_Hash_Map (Report);
   end All_Tests;


   -----------------------
   -- Inidividual Tests --
   -----------------------

   procedure Alignment (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Debug instantiation");
   begin
      Test_Render (Test, "(width 5)", 0, "    0");
      Test_Render (Test, "(width 5)(padding _)(align center)", 10, "_10__");
      Test_Render (Test, "(width 5 10)(left-align)", 7, "7    ");
      Test_Render (Test, "(min-width 5)(right-align)", 2, "    2");
      Test_Render (Test, "(width 5)(padding > <)(centered)", 4, ">>4<<");
      Test_Render
        (Test,
         "(width 5)(left-padding ""["")(right-padding ""]"")(centered)",
         126,
         "[126]");
      Test_Render (Test, "(width 3)(centered)", 16, "16 ");
      Test_Render (Test, "(width 3)(centered)", 456, "456");
      Test_Render (Test, "(width 3)(align left)", 567, "567");
      Test_Render (Test, "(width 3)(align right)", 678, "678");
   exception
      when Error : others => Test.Report_Exception (Error);
   end Alignment;


   procedure Default_Format (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Debug instantiation");
   begin
      Test_Render (Test, "", 42, "42");
   exception
      when Error : others => Test.Report_Exception (Error);
   end Default_Format;


   procedure Explicit_Sign (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Explicit sign specification");
   begin
      Test_Render (Test, "(sign +)", 42, "+42");
      Test_Render (Test, "(sign + _)", 42, "+42");
      Test_Render (Test, "(sign + _)", -42, "_42");
   exception
      when Error : others => Test.Report_Exception (Error);
   end Explicit_Sign;


   procedure Hexadecimal (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Hexadecimal representation");
      Hex_Spec : constant String
        := "(base 0 1 2 3 4 5 6 7 8 9 A B C D E F)";
   begin
      Test_Render (Test, Hex_Spec, 8, "8");
      Test_Render (Test, Hex_Spec, 16#BEE#, "BEE");
   exception
      when Error : others => Test.Report_Exception (Error);
   end Hexadecimal;


   procedure Overflow (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Width overflow");
   begin
      Test_Render (Test, "(width 3)", 10_000, "");
      Test_Render (Test, "(max-width 4)", 10_000, "");
      Test_Render (Test, "(max-width 3 ""[...]"")", 10_000, "[...]");
      Test_Render (Test, "(width 2 3 ...)", 10_000, "...");
   exception
      when Error : others => Test.Report_Exception (Error);
   end Overflow;


   procedure Parse_Errors (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Parse errors in template");
   begin
      Test_Render (Test, "(invalid-command)", 1, "1");
      Test_Render (Test, "(align)", 2, "2");
      Test_Render (Test, "(align invalid)", 3, "3");
      Test_Render (Test, "(padding)", 4, "4");
      Test_Render (Test, "(left-padding)", 5, "5");
      Test_Render (Test, "(right-padding)", 6, "6");
      Test_Render (Test, "(signs)", 7, "7");
      Test_Render (Test, "(width)", 8, "8");
      Test_Render (Test, "(max-width)", 9, "9");
      Test_Render (Test, "(min-width)", 10, "10");
   exception
      when Error : others => Test.Report_Exception (Error);
   end Parse_Errors;


   procedure Static_Hash_Map (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Parse errors in template");
   begin
      if not Natools.Static_Maps.S_Expressions.Templates.Integers.T then
         Test.Fail;
      end if;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Static_Hash_Map;

end Natools.S_Expressions.Templates.Tests.Integers;