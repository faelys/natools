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
      Defaults : in Templates.Integers.Format;
      Template : in String;
      Value : in Integer;
      Expected : in String);
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


   procedure Test_Render
     (Test : in out NT.Test;
      Defaults : in Templates.Integers.Format;
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
      Templates.Integers.Render (Output, Defaults, Parser, Value);
      Output.Check_Stream (Test);
   end Test_Render;



   -------------------------
   -- Complete Test Suite --
   -------------------------

   procedure All_Tests (Report : in out NT.Reporter'Class) is
   begin
      Alignment (Report);
      Default_Format (Report);
      Explicit_Images (Report);
      Explicit_Sign (Report);
      Hexadecimal (Report);
      Overflow (Report);
      Parse_Errors (Report);
      Static_Hash_Map (Report);
      Explicit_Default_Format (Report);
      Prefix_And_Suffix (Report);
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


   procedure Explicit_Default_Format (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Client-provided default format");
   begin
      declare
         Default : Templates.Integers.Format;
      begin
         Default.Set_Minimum_Width (2);
         Default.Set_Left_Padding (To_Atom ("0"));

         Test_Render (Test, Default, "", 5, "05");
         Test_Render (Test, Default, "", 12, "12");
         Test_Render (Test, Default, "(padding 1: )", 7, " 7");
      end;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Explicit_Default_Format;


   procedure Explicit_Images (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Explicit images in template");
   begin
      Test_Render (Test, "(image (-2 two) (666 evil))", 10, "10");
      Test_Render (Test, "(image (-2 two) (666 evil))", -2, "two");
      Test_Render (Test, "(image (-2 two) (666 evil))", 666, "evil");
      Test_Render (Test, "(image (-2 two) (666 evil) (-2))", -2, "-2");
      Test_Render (Test, "(image (1 one))3:Two4:four", 1, "one");
      Test_Render (Test, "(image (1 one))3:Two4:four", 2, "Two");
      Test_Render (Test, "(image (1 one))3:Two4:four", 3, "four");
      Test_Render (Test, "(image (1 one))3:Two4:four", 4, "4");
      Test_Render (Test, "(image (invalid -))5:first", Integer'First, "first");
   exception
      when Error : others => Test.Report_Exception (Error);
   end Explicit_Images;


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


   procedure Prefix_And_Suffix (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Parse errors in template");
      Ordinal : constant String
        := "(suffix 1:? (th (0 31)) (st 1 21 31) (nd 2 22) (rd 3 23) (0: 0))";
   begin
      declare
         Format : Templates.Integers.Format;
      begin
         Format.Set_Prefix ((0, 9), To_Atom ("a"));
         Format.Set_Prefix ((-99, -10), To_Atom ("b"));
         Format.Set_Prefix ((50, 99), To_Atom ("c"));
         Format.Set_Prefix (0, To_Atom ("d"));
         Format.Set_Prefix (-10, To_Atom ("e"));
         Format.Set_Prefix (5, To_Atom ("f"));
         Format.Set_Prefix ((7, 52), To_Atom ("g"));
         Format.Set_Prefix ((-52, -49), To_Atom ("h"));
         Format.Set_Prefix ((-100, -90), To_Atom ("i"));
         Format.Remove_Prefix (8);

         Test_Render (Test, Format, "", -196, "-196");
         Test_Render (Test, Format, "", -101, "-101");
         Test_Render (Test, Format, "", -100, "i-100");
         Test_Render (Test, Format, "", -90, "i-90");
         Test_Render (Test, Format, "", -89, "b-89");
         Test_Render (Test, Format, "", -53, "b-53");
         Test_Render (Test, Format, "", -52, "h-52");
         Test_Render (Test, Format, "", -49, "h-49");
         Test_Render (Test, Format, "", -48, "b-48");
         Test_Render (Test, Format, "", -11, "b-11");
         Test_Render (Test, Format, "", -10, "e-10");
         Test_Render (Test, Format, "", -9, "-9");
         Test_Render (Test, Format, "", -1, "-1");
         Test_Render (Test, Format, "", 0, "d0");
         Test_Render (Test, Format, "", 1, "a1");
         Test_Render (Test, Format, "", 4, "a4");
         Test_Render (Test, Format, "", 5, "f5");
         Test_Render (Test, Format, "", 6, "a6");
         Test_Render (Test, Format, "", 7, "g7");
         Test_Render (Test, Format, "", 8, "8");
         Test_Render (Test, Format, "", 9, "g9");
         Test_Render (Test, Format, "", 52, "g52");
         Test_Render (Test, Format, "", 53, "c53");
         Test_Render (Test, Format, "", 99, "c99");
         Test_Render (Test, Format, "", 100, "100");
         Test_Render (Test, Format, "", 192, "192");
      end;

      declare
         Format : Templates.Integers.Format;
      begin
         Format.Set_Suffix ((0, 10), To_Atom ("th"));
         Format.Set_Suffix (1, To_Atom ("st"));
         Format.Remove_Suffix (0);

         Test_Render (Test, Format, "", -1, "-1");
         Test_Render (Test, Format, "", 0, "0");
         Test_Render (Test, Format, "", 1, "1st");
         Test_Render (Test, Format, "", 4, "4th");
         Test_Render (Test, Format, "", 10, "10th");
      end;

      Test_Render (Test, Ordinal, -1, "-1?");
      Test_Render (Test, Ordinal, 0, "0");
      Test_Render (Test, Ordinal, 1, "1st");
      Test_Render (Test, Ordinal, 2, "2nd");
      Test_Render (Test, Ordinal, 3, "3rd");
      Test_Render (Test, Ordinal, 4, "4th");

      Test_Render (Test, "(prefix (a) (b invalid (9 5)))", 0, "0");
      Test_Render (Test, "(prefix (c (invalid 5) (-1 invalid)))", 0, "0");
      Test_Render (Test, "(prefix (d ((invalid) 5) (-1)) ())", 0, "0");

      declare
         Format : Templates.Integers.Format;
      begin
         Format.Set_Minimum_Width (10);
         Format.Set_Suffix (1, To_Atom ("<sup>er</sup>"), 2);
         Format.Set_Prefix (10, To_Atom ("dix : "));

         Test_Render (Test, Format, "", 5, "         5");
         Test_Render (Test, Format, "", 1, "       1<sup>er</sup>");
         Test_Render (Test, Format, "(centered)", 10, " dix : 10 ");
         Test_Render (Test, Format, "(suffix ((th 0) 7))", 7, "         7th");
      end;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Prefix_And_Suffix;


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
