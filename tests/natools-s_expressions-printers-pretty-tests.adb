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

   procedure Parse_Print_Test
     (Test : in out NT.Test;
      Param : in Parameters;
      Expected : in Atom);
      --  Parse Expected and feed it into a new pretty printer, checking
      --  the result is identical to Expected.



   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

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
      begin
         Input.Set_Data (Expected);
         Output.Set_Expected (Expected);
         Pretty_Printer.Set_Parameters (Param);
         Subparser.Next;
         Transfer (Subparser, Pretty_Printer);
         Output.Check_Stream (Test);
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
      Newline_Formats (Report);
      Token_Separation (Report);
      Parameter_Mutators (Report);
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

         Output.Check_Stream (Test);
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

         Output.Check_Stream (Test);
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

         Output.Check_Stream (Test);
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


   procedure Newline_Formats (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Newline formats");

      procedure Print (Pr : in out Printer);

      procedure Print (Pr : in out Printer) is
      begin
         Pr.Open_List;
         Pr.Append_Atom (To_Atom ("begin"));
         Pr.Append_Atom (To_Atom
           ("quoted" & Latin_1.CR & Latin_1.LF & Latin_1.CR & "str"));
         Pr.Close_List;
      end Print;

      Param : Parameters
        := (Width => 7,
            Newline_At => (others => (others => False)),
            Space_At => (others => (others => False)),
            Tab_Stop => 8,  --  unused
            Indentation => 1,
            Indent => Spaces,
            Quoted => When_Shorter,
            Token => Standard_Token,
            Hex_Casing => Encodings.Upper,
            Quoted_Escape => Hex_Escape,
            Char_Encoding => ASCII,
            Fallback => Hexadecimal,
            Newline => CR);
   begin
      Param.Newline_At (Atom_Data, Atom_Data) := True;

      declare
         Output : aliased Test_Tools.Memory_Stream;
         Pr : Printer (Output'Access);
      begin
         Pr.Set_Parameters (Param);
         Output.Set_Expected (To_Atom
           ("(begin" & Latin_1.CR
            & " ""quot\" & Latin_1.CR
            & "ed" & Latin_1.CR
            & "\n" & Latin_1.CR
            & "str"")"));
         Print (Pr);
         Output.Check_Stream (Test);
      end;

      Param.Newline := LF;

      declare
         Output : aliased Test_Tools.Memory_Stream;
         Pr : Printer (Output'Access);
      begin
         Pr.Set_Parameters (Param);
         Output.Set_Expected (To_Atom
           ("(begin" & Latin_1.LF
            & " ""quot\" & Latin_1.LF
            & "ed\r" & Latin_1.LF
            & "\rstr"")"));
         Print (Pr);
         Output.Check_Stream (Test);
      end;

      Param.Newline := CR_LF;

      declare
         Output : aliased Test_Tools.Memory_Stream;
         Pr : Printer (Output'Access);
      begin
         Pr.Set_Parameters (Param);
         Output.Set_Expected (To_Atom
           ("(begin" & Latin_1.CR & Latin_1.LF
            & " ""quot\" & Latin_1.CR & Latin_1.LF
            & "ed" & Latin_1.CR & Latin_1.LF
            & "\rstr"")"));
         Print (Pr);
         Output.Check_Stream (Test);
      end;

      Param.Newline := LF_CR;

      declare
         Output : aliased Test_Tools.Memory_Stream;
         Pr : Printer (Output'Access);
      begin
         Pr.Set_Parameters (Param);
         Output.Set_Expected (To_Atom
           ("(begin" & Latin_1.LF & Latin_1.CR
            & " ""quot\" & Latin_1.LF & Latin_1.CR
            & "ed\r" & Latin_1.LF & Latin_1.CR
            & "str"")"));
         Print (Pr);
         Output.Check_Stream (Test);
      end;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Newline_Formats;


   procedure Parameter_Mutators (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Parameter mutators");
      Initial : constant Parameters
        := (Width => 0,
            Newline_At => (others => (others => False)),
            Space_At => (others => (others => False)),
            Tab_Stop => 8,
            Indentation => 0,
            Indent => Spaces,
            Quoted => No_Quoted,
            Token => No_Token,
            Hex_Casing => Encodings.Upper,
            Quoted_Escape => Octal_Escape,
            Char_Encoding => ASCII,
            Fallback => Verbatim,
            Newline => LF);
      Final : constant Parameters
        := (Width => 79,
            Newline_At => (others => (others => True)),
            Space_At => (others => (others => True)),
            Tab_Stop => 4,
            Indentation => 1,
            Indent => Tabs,
            Quoted => When_Shorter,
            Token => Standard_Token,
            Hex_Casing => Encodings.Lower,
            Quoted_Escape => Hex_Escape,
            Char_Encoding => UTF_8,
            Fallback => Hexadecimal,
            Newline => CR_LF);
   begin
      declare
         Output : aliased Test_Tools.Memory_Stream;
         Pr : Printer (Output'Access);
      begin
         Pr.Set_Parameters (Initial);

         Pr.Set_Width (Final.Width);
         Pr.Set_Newline_At (Final.Newline_At);
         Pr.Set_Space_At (Final.Space_At);
         Pr.Set_Tab_Stop (Final.Tab_Stop);
         Pr.Set_Indentation (Final.Indentation);
         Pr.Set_Indent (Final.Indent);
         Pr.Set_Quoted (Final.Quoted);
         Pr.Set_Token (Final.Token);
         Pr.Set_Hex_Casing (Final.Hex_Casing);
         Pr.Set_Quoted_Escape (Final.Quoted_Escape);
         Pr.Set_Char_Encoding (Final.Char_Encoding);
         Pr.Set_Fallback (Final.Fallback);
         Pr.Set_Newline (Final.Newline);

         if Pr.Get_Parameters /= Final then
            Test.Fail;
         end if;
      end;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Parameter_Mutators;


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
         Output.Check_Stream (Test);
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
         Output.Check_Stream (Test);
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
         Output.Check_Stream (Test);
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
         Output.Check_Stream (Test);
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
         Output.Check_Stream (Test);
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
         Output.Check_Stream (Test);
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
         Output.Check_Stream (Test);
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
         Output.Check_Stream (Test);
      end;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Separators;


   procedure Token_Separation (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Token separation");
      Token : constant Atom := To_Atom ("token");
   begin
      declare
         Output : aliased Test_Tools.Memory_Stream;
         Pr : Printer (Output'Access);
      begin
         Output.Set_Expected (To_Atom
           ("(begin(token ""quoted\n""token token #4865780A#token "
            & "|QmFzZS02NAo=|token)end)"));
         Pr.Set_Parameters
          ((Width => 0,
            Newline_At => (others => (others => False)),
            Space_At => (others => (others => False)),
            Tab_Stop => 8,
            Indentation => 0,
            Indent => Spaces,
            Quoted => When_Shorter,
            Token => Standard_Token,
            Hex_Casing => Encodings.Upper,
            Quoted_Escape => Hex_Escape,
            Char_Encoding => ASCII,
            Fallback => Hexadecimal,
            Newline => LF));

         Pr.Open_List;
         Pr.Append_Atom (To_Atom ("begin"));
         Pr.Open_List;
         Pr.Append_Atom (Token);
         Pr.Append_Atom (To_Atom ("quoted" & Latin_1.LF));
         Pr.Append_Atom (Token);
         Pr.Append_Atom (Token);
         Pr.Set_Quoted (No_Quoted);
         Pr.Append_Atom (To_Atom ("Hex" & Latin_1.LF));
         Pr.Append_Atom (Token);
         Pr.Set_Fallback (Base64);
         Pr.Append_Atom (To_Atom ("Base-64" & Latin_1.LF));
         Pr.Append_Atom (Token);
         Pr.Close_List;
         Pr.Append_Atom (To_Atom ("end"));
         Pr.Close_List;

         Output.Check_Stream (Test);
      end;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Token_Separation;

end Natools.S_Expressions.Printers.Pretty.Tests;
