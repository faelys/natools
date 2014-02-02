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
      Atom_Width (Report);
      Quoted_String_Escapes (Report);
      Indentation (Report);
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


   procedure Atom_Width (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Atom width");
      Param : Parameters
        := (Width => 10,
            Newline_At => (others => (others => False)),
            Space_At => (others => (others => False)),
            Tab_Stop => 8,
            Indentation => 3,
            Indent => Spaces,
            Quoted => No_Quoted,
            Token => No_Token,
            Hex_Casing => Encodings.Upper,
            Quoted_Escape => Hex_Escape,
            Char_Encoding => ASCII,
            Fallback => Hexadecimal,
            Newline => LF);
   begin
      declare
         Output : aliased Test_Tools.Memory_Stream;
         Pr : Printer (Output'Access);
      begin
         Output.Set_Expected (To_Atom ("(" & Latin_1.LF
            & "   #303132" & Latin_1.LF
            & "    333435" & Latin_1.LF
            & "    3637#)"));
         Pr.Set_Parameters (Param);

         Pr.Open_List;
         Pr.Append_Atom (To_Atom ("01234567"));
         Pr.Close_List;

         Check_Stream (Test, Output);
      end;

      Param.Fallback := Base64;

      declare
         Output : aliased Test_Tools.Memory_Stream;
         Pr : Printer (Output'Access);
      begin
         Output.Set_Expected (To_Atom ("(" & Latin_1.LF
            & "   |  YWJj" & Latin_1.LF
            & "      REVG" & Latin_1.LF
            & "      Z2hp" & Latin_1.LF
            & "      SktM" & Latin_1.LF
            & "   |)"));
         Pr.Set_Parameters (Param);

         Pr.Open_List;
         Pr.Append_Atom (To_Atom ("abcDEFghiJKL"));
         Pr.Close_List;

         Check_Stream (Test, Output);
      end;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Atom_Width;


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


   procedure Indentation (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Indentation");
      Param : Parameters
        := (Width => 16,
            Newline_At => (others => (others => False)),
            Space_At => (others => (others => False)),
            Tab_Stop => 8,
            Indentation => 3,
            Indent => Tabs_And_Spaces,
            Quoted => Single_Line,
            Token => Standard_Token,
            Hex_Casing => Encodings.Upper,
            Quoted_Escape => Hex_Escape,
            Char_Encoding => ASCII,
            Fallback => Verbatim,
            Newline => LF);
   begin
      Parse_Print_Test (Test, Param, To_Atom
        ("(first-level(" & Latin_1.LF
         & "      second-level" & Latin_1.LF
         & "      (third" & Latin_1.LF
         & Latin_1.HT & " level" & Latin_1.LF
         & Latin_1.HT & " ""Q#""))" & Latin_1.LF
         & "   end)"));

      Param.Indent := Spaces;
      Param.Token := Extended_Token;

      Parse_Print_Test (Test, Param, To_Atom
        ("(first-level(" & Latin_1.LF
         & "      second-level" & Latin_1.LF
         & "      (third" & Latin_1.LF
         & "         level" & Latin_1.LF
         & "         ""Q)""))" & Latin_1.LF
         & "   end)"));

      Param.Indent := Tabs;
      Param.Indentation := 1;
      Param.Tab_Stop := 5;

      Parse_Print_Test (Test, Param, To_Atom
        ("(first-level(" & Latin_1.LF
         & Latin_1.HT & Latin_1.HT & "second-level" & Latin_1.LF
         & Latin_1.HT & Latin_1.HT & "(third" & Latin_1.LF
         & Latin_1.HT & Latin_1.HT & Latin_1.HT & "level" & Latin_1.LF
         & Latin_1.HT & Latin_1.HT & Latin_1.HT & "2:Q(" & Latin_1.LF
         & Latin_1.HT & Latin_1.HT & "))end)"));

   exception
      when Error : others => Test.Report_Exception (Error);
   end Indentation;


   procedure Quoted_String_Escapes (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Escapes in quoted string atoms");
      Source : constant Atom (0 .. 123) := To_Atom
        ("Special: "  --  indices 0 .. 17
         & Latin_1.BS & Latin_1.HT & Latin_1.LF
         & Latin_1.VT & Latin_1.FF & Latin_1.CR
         & '\' & '"' & Latin_1.NUL
         & "UTF-8 sequences: "  --  indices 18 .. 62
         & "é, −, 🁡, "  --  U+00E9, U+2212, U+1F061
         & Character'Val (16#F9#) & Character'Val (16#88#)
         & Character'Val (16#B4#) & Character'Val (16#95#)
         & Character'Val (16#A7#)   --  U+1234567
         & ", "
         & Character'Val (16#FD#) & Character'Val (16#B6#)
         & Character'Val (16#95#) & Character'Val (16#83#)
         & Character'Val (16#88#) & Character'Val (16#90#)  --  U+76543210
         & "Invalid UTF-8 sequences: "  --  indices 63 .. 117
         & Character'Val (16#AA#) & ", "
         & Character'Val (16#C3#) & ", "
         & Character'Val (16#E2#) & Character'Val (16#88#) & ", "
         & Character'Val (16#F0#) & Character'Val (16#9F#)
         & Character'Val (16#81#) & ", "
         & Character'Val (16#F9#) & Character'Val (16#88#)
         & Character'Val (16#B4#) & Character'Val (16#95#) & ", "
         & Character'Val (16#FD#) & Character'Val (16#B6#)
         & Character'Val (16#95#) & Character'Val (16#83#)
         & Character'Val (16#88#) & ", "
         & Character'Val (16#FE#) & "."
         & Latin_1.CR & Latin_1.LF  --  indices 118 .. 119
         & "<>"  --  indices 120 .. 121
         & Latin_1.CR & Latin_1.LF);
      Param : Parameters
        := (Width => 0,
            Newline_At => (others => (others => False)),
            Space_At => (others => (others => False)),
            Tab_Stop => 8,
            Indentation => 3,
            Indent => Spaces,
            Quoted => When_Shorter,
            Token => No_Token,
            Hex_Casing => Encodings.Upper,
            Quoted_Escape => Hex_Escape,
            Char_Encoding => ASCII,
            Fallback => Hexadecimal,
            Newline => CR_LF);
   begin
      declare
         Output : aliased Test_Tools.Memory_Stream;
         Pr : Printer (Output'Access);
      begin
         --  Check that the first quoted string encoding is exactly as long as
         --  fallback (hexadecimal) encoding, by trying with one less char.
         Output.Set_Expected
           (Encodings.Hex_Atom_Begin
            & Encodings.Encode_Hex
               (Source (Source'First + 1 .. Source'Last), Param.Hex_Casing)
            & Encodings.Hex_Atom_End);
         Pr.Set_Parameters (Param);
         Pr.Append_Atom (Source (Source'First + 1 .. Source'Last));
         Check_Stream (Test, Output);
      end;

      declare
         Output : aliased Test_Tools.Memory_Stream;
         Pr : Printer (Output'Access);
      begin
         Output.Set_Expected (To_Atom
           ("""Special: \b\t\n\v\f\r\\\""\x00"
            & "UTF-8 sequences: \xC3\xA9, \xE2\x88\x92, \xF0\x9F\x81\xA1, "
            & "\xF9\x88\xB4\x95\xA7, \xFD\xB6\x95\x83\x88\x90"
            & "Invalid UTF-8 sequences: "
            & "\xAA, \xC3, \xE2\x88, \xF0\x9F\x81, \xF9\x88\xB4\x95, "
            & "\xFD\xB6\x95\x83\x88, \xFE." & Latin_1.CR & Latin_1.LF
            & "<>\r\n"""));
         Pr.Set_Parameters (Param);
         Pr.Append_Atom (Source);
         Check_Stream (Test, Output);
      end;

      Param.Char_Encoding := Latin;
      Param.Hex_Casing := Encodings.Lower;

      declare
         Output : aliased Test_Tools.Memory_Stream;
         Pr : Printer (Output'Access);
      begin
         Output.Set_Expected (To_Atom
           ("""Special: \b\t\n\v\f\r\\\""\x00"
            & "UTF-8 sequences: "
            & Character'Val (16#C3#) & Character'Val (16#A9#)
            & ", " & Character'Val (16#E2#) & "\x88\x92, "
            & Character'Val (16#F0#) & "\x9f\x81"
            & Character'Val (16#A1#) & ", "
            & Character'Val (16#F9#) & "\x88"
            & Character'Val (16#B4#) & "\x95"
            & Character'Val (16#A7#) & ", "
            & Character'Val (16#FD#) & Character'Val (16#B6#)
            & "\x95\x83\x88\x90"
            & "Invalid UTF-8 sequences: "
            & Character'Val (16#AA#) & ", "
            & Character'Val (16#C3#) & ", "
            & Character'Val (16#E2#) & "\x88, "
            & Character'Val (16#F0#) & "\x9f\x81, "
            & Character'Val (16#F9#) & "\x88"
            & Character'Val (16#B4#) & "\x95, "
            & Character'Val (16#FD#) & Character'Val (16#B6#)
            & "\x95\x83\x88, " & Character'Val (16#FE#) & '.'
            & Latin_1.CR & Latin_1.LF
            & "<>\r\n"""));
         Pr.Set_Parameters (Param);
         Pr.Append_Atom (Source);
         Check_Stream (Test, Output);
      end;

      Param.Char_Encoding := UTF_8;
      Param.Quoted_Escape := Octal_Escape;

      declare
         Output : aliased Test_Tools.Memory_Stream;
         Pr : Printer (Output'Access);
      begin
         Output.Set_Expected (To_Atom ("""Special: \b\t\n\v\f\r\\\""\000")
            & Source (18 .. 62)
            & To_Atom ("Invalid UTF-8 sequences: "
               & "\252, \303, \342\210, \360\237\201, "
               & "\371\210\264\225, \375\266\225\203\210, \376."
               & Latin_1.CR & Latin_1.LF
               & "<>\r\n"""));
         Pr.Set_Parameters (Param);
         Pr.Append_Atom (Source);
         Check_Stream (Test, Output);
      end;

      Param.Width := 31;

      declare
         Output : aliased Test_Tools.Memory_Stream;
         Pr : Printer (Output'Access);
      begin
         Output.Set_Expected (To_Atom ("""Special: \b\t\n\v\f\r\\\""\000"
                                 & '\' & Latin_1.CR & Latin_1.LF)
            & Source (18 .. 62)
            & To_Atom ('\' & Latin_1.CR & Latin_1.LF
               & "Invalid UTF-8 sequences: \252,\" & Latin_1.CR & Latin_1.LF
               & " \303, \342\210, \360\237\201,\" & Latin_1.CR & Latin_1.LF
               & " \371\210\264\225, \375\266\" & Latin_1.CR & Latin_1.LF
               & "\225\203\210, \376."
               & Latin_1.CR & Latin_1.LF
               & "<>\r\n"""));
         Pr.Set_Parameters (Param);
         Pr.Append_Atom (Source);
         Check_Stream (Test, Output);
      end;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Quoted_String_Escapes;


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
