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

with Ada.Characters.Latin_1;

with Natools.S_Expressions.Encodings;
with Natools.S_Expressions.Parsers;
with Natools.S_Expressions.Test_Tools;

package body Natools.S_Expressions.Printers.Pretty.Tests is

   package Latin_1 renames Ada.Characters.Latin_1;

   procedure Check_Stream
     (Test : in out NT.Test;
      Stream : in Test_Tools.Memory_Stream);
      --  On error in Stream, report error and dump relevant information.

   procedure Parse_Print_Test
     (Test : in out NT.Test;
      Param : in Parameters;
      Expected : in Atom);
      --  Parse Expected and feed it into a new pretty printer, checking
      --  the result is identical to Expected.



   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

   procedure Check_Stream
     (Test : in out NT.Test;
      Stream : in Test_Tools.Memory_Stream) is
   begin
      if Stream.Has_Mismatch or else Stream.Unread_Expected /= Null_Atom then
         if Stream.Has_Mismatch then
            Test.Fail ("Mismatch at position"
              & Count'Image (Stream.Mismatch_Index));

            declare
               Stream_Data : Atom renames Stream.Get_Data;
            begin
               Test_Tools.Dump_Atom
                 (Test,
                  Stream_Data (Stream_Data'First .. Stream.Mismatch_Index - 1),
                  "Matching data");
               Test_Tools.Dump_Atom
                 (Test,
                  Stream_Data (Stream.Mismatch_Index .. Stream_Data'Last),
                  "Mismatching data");
            end;
         end if;

         if Stream.Unread_Expected /= Null_Atom then
            Test.Fail;
            Test_Tools.Dump_Atom
              (Test,
               Stream.Unread_Expected,
               "Left to expect");
         end if;
      end if;
   end Check_Stream;


   procedure Parse_Print_Test
     (Test : in out NT.Test;
      Param : in Parameters;
      Expected : in Atom) is
   begin
      declare
         Input, Output : aliased Test_Tools.Memory_Stream;
         Parser : aliased Parsers.Parser;
         Subparser : Parsers.Subparser (Parser'Access, Input'Access);
         Pretty_Printer : Printer (Output'Access);
         Event : Events.Event;
      begin
         Input.Set_Data (Expected);
         Output.Set_Expected (Expected);
         Pretty_Printer.Set_Parameters (Param);
         Subparser.Next (Event);
         Transfer (Subparser, Pretty_Printer);
         Check_Stream (Test, Output);
      end;
   exception
      when Error : others =>
         Test.Report_Exception (Error);
   end Parse_Print_Test;



   -------------------------
   -- Complete Test Suite --
   -------------------------

   procedure All_Tests (Report : in out NT.Reporter'Class) is
   begin
      Basic_Printing (Report);
      Atom_Encodings (Report);
      Separators (Report);
   end All_Tests;



   -----------------------
   -- Inidividual Tests --
   -----------------------

   procedure Atom_Encodings (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Atom encodings");
      Param : Parameters := Canonical;
   begin
      Parse_Print_Test (Test, Param,
        To_Atom ("17:verbatim" & Latin_1.NUL & "encoding"));

      Param.Fallback := Hexadecimal;
      Parse_Print_Test (Test, Param,
        To_Atom ("#48657861646563696D616C03456E636F64696E670A#"));

      Param.Hex_Casing := Encodings.Lower;
      Parse_Print_Test (Test, Param,
        To_Atom ("#4c6f7765722043617365204865786164"
                & "6563696d616c03456e636f64696e670a#"));

      Param.Fallback := Base64;
      Parse_Print_Test (Test, Param,
        To_Atom ("|QmFzZS02NAllbmNvZGluZwo=|"));

      Param.Quoted := Single_Line;
      Parse_Print_Test (Test, Param,
        To_Atom ("""quoted\r\nstring\tencoding"""));

      Param.Token := Standard_Token;
      Parse_Print_Test (Test, Param,
        To_Atom ("(standard token ""123""encoding)"));

      Param.Token := Extended_Token;
      Parse_Print_Test (Test, Param,
        To_Atom ("(extended token 123 encoding)"));
   exception
      when Error : others => Test.Report_Exception (Error);
   end Atom_Encodings;


   procedure Basic_Printing (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Basic printing");
   begin
      declare
         Output : aliased Test_Tools.Memory_Stream;
         P : Printer (Output'Access);
      begin
         Output.Set_Expected (To_Atom ("(7:command(6:subarg)3:arg)3:end"));
         P.Set_Parameters (Canonical);

         P.Open_List;
         P.Append_Atom (To_Atom ("command"));
         P.Open_List;
         P.Append_Atom (To_Atom ("subarg"));
         P.Close_List;
         P.Append_Atom (To_Atom ("arg"));
         P.Close_List;
         P.Append_Atom (To_Atom ("end"));

         Check_Stream (Test, Output);
      end;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Basic_Printing;


   procedure Separators (Report : in out NT.Reporter'Class) is
      procedure Test_Exp (Pr : in out Printer);

      procedure Test_Exp (Pr : in out Printer) is
      begin
         Pr.Append_Atom (To_Atom ("begin"));
         Pr.Open_List;
         Pr.Open_List;
         Pr.Close_List;
         Pr.Open_List;
         Pr.Append_Atom (To_Atom ("head"));
         Pr.Append_Atom (To_Atom ("tail"));
         Pr.Close_List;
         Pr.Close_List;
         Pr.Append_Atom (To_Atom ("end"));
      end Test_Exp;

      Test : NT.Test := Report.Item ("Separators");
      Param : Parameters := Canonical;
   begin
      declare
         Output : aliased Test_Tools.Memory_Stream;
         Pr : Printer (Output'Access);
      begin
         Output.Set_Expected (To_Atom ("5:begin(()(4:head4:tail))3:end"));
         Pr.Set_Parameters (Param);
         Test_Exp (Pr);
         Check_Stream (Test, Output);
      end;

      Param.Space_At := (others => (others => True));

      declare
         Output : aliased Test_Tools.Memory_Stream;
         Pr : Printer (Output'Access);
      begin
         Output.Set_Expected
           (To_Atom ("5:begin ( ( ) ( 4:head 4:tail ) ) 3:end"));
         Pr.Set_Parameters (Param);
         Test_Exp (Pr);
         Check_Stream (Test, Output);
      end;

      Param.Newline_At := (others => (others => True));
      Param.Newline := LF;

      declare
         Output : aliased Test_Tools.Memory_Stream;
         Pr : Printer (Output'Access);
      begin
         Output.Set_Expected (To_Atom ("5:begin" & Latin_1.LF
           & '(' & Latin_1.LF
           & '(' & Latin_1.LF
           & ')' & Latin_1.LF
           & '(' & Latin_1.LF
           & "4:head" & Latin_1.LF
           & "4:tail" & Latin_1.LF
           & ')' & Latin_1.LF
           & ')' & Latin_1.LF
           & "3:end"));
         Pr.Set_Parameters (Param);
         Test_Exp (Pr);
         Check_Stream (Test, Output);
      end;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Separators;

end Natools.S_Expressions.Printers.Pretty.Tests;
