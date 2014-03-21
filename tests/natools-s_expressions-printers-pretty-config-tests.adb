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

with Natools.S_Expressions.Encodings;
with Natools.S_Expressions.Parsers;
with Natools.S_Expressions.Test_Tools;

package body Natools.S_Expressions.Printers.Pretty.Config.Tests is

   procedure Check_Param
     (Test : in out NT.Test;
      Result : in Parameters;
      Expected : in Parameters;
      Context : in String := "");


   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

   procedure Check_Param
     (Test : in out NT.Test;
      Result : in Parameters;
      Expected : in Parameters;
      Context : in String := "")
   is
      use type Encodings.Hex_Casing;

      function Image (E : Entity) return String;
      function Image (Left, Right : Entity; Active : Boolean) return String;
      function Image (Sep : Entity_Separator) return String;
      function Image (I : Indent_Type) return String;

      function Image (E : Entity) return String is
      begin
         case E is
            when Opening => return "O";
            when Atom_Data => return "A";
            when Closing => return "C";
         end case;
      end Image;

      function Image (Left, Right : Entity; Active : Boolean) return String is
      begin
         if Active then
            return  ' ' & Image (Left) & Image (Right);
         else
            return "";
         end if;
      end Image;

      function Image (Sep : Entity_Separator) return String is
         Result : String
           := Image (Opening, Opening, Sep (Opening, Opening))
            & Image (Opening, Atom_Data, Sep (Opening, Atom_Data))
            & Image (Opening, Closing, Sep (Opening, Closing))
            & Image (Atom_Data, Opening, Sep (Atom_Data, Opening))
            & Image (Atom_Data, Atom_Data, Sep (Atom_Data, Atom_Data))
            & Image (Atom_Data, Closing, Sep (Atom_Data, Closing))
            & Image (Closing, Opening, Sep (Closing, Opening))
            & Image (Closing, Atom_Data, Sep (Closing, Atom_Data))
            & Image (Closing, Closing, Sep (Closing, Closing))
            & ')';
      begin
         if Result'Length = 1 then
            return "()";
         else
            Result (Result'First) := '(';
            return Result;
         end if;
      end Image;

      function Image (I : Indent_Type) return String is
      begin
         case I is
            when Spaces => return "spaces";
            when Tabs => return "tabs";
            when Tabs_And_Spaces => return "columns (with tabs)";
         end case;
      end Image;
   begin
      if Result = Expected then
         return;
      end if;

      Test.Fail (Context);

      if Result.Width /= Expected.Width then
         Test.Info ("Found width"
           & Screen_Offset'Image (Result.Width)
           & ", expected"
           & Screen_Offset'Image (Expected.Width));
      end if;

      if Result.Newline_At /= Expected.Newline_At then
         Test.Info ("Found newline at "
           & Image (Result.Newline_At)
           & ", expected "
           & Image (Expected.Newline_At));
      end if;

      if Result.Space_At /= Expected.Space_At then
         Test.Info ("Found space at "
           & Image (Result.Space_At)
           & ", expected "
           & Image (Expected.Space_At));
      end if;

      if Result.Tab_Stop /= Expected.Tab_Stop then
         Test.Info ("Found tab stop"
           & Screen_Offset'Image (Result.Tab_Stop)
           & ", expected"
           & Screen_Offset'Image (Expected.Tab_Stop));
      end if;

      if Result.Indentation /= Expected.Indentation
        or Result.Indent /= Expected.Indent
      then
         Test.Info ("Found indentation"
           & Screen_Offset'Image (Result.Indentation)
           & ' ' & Image (Result.Indent)
           & ", expected"
           & Screen_Offset'Image (Expected.Indentation)
           & ' ' & Image (Expected.Indent));
      end if;

      if Result.Quoted /= Expected.Quoted then
         Test.Info ("Found quoted option "
           & Quoted_Option'Image (Result.Quoted)
           & ", expected "
           & Quoted_Option'Image (Expected.Quoted));
      end if;

      if Result.Token /= Expected.Token then
         Test.Info ("Found token option "
           & Token_Option'Image (Result.Token)
           & ", expected "
           & Token_Option'Image (Expected.Token));
      end if;

      if Result.Hex_Casing /= Expected.Hex_Casing then
         Test.Info ("Found hex casing "
           & Encodings.Hex_Casing'Image (Result.Hex_Casing)
           & ", expected "
           & Encodings.Hex_Casing'Image (Expected.Hex_Casing));
      end if;

      if Result.Quoted_Escape /= Expected.Quoted_Escape then
         Test.Info ("Found quoted escape "
           & Quoted_Escape_Type'Image (Result.Quoted_Escape)
           & ", expected "
           & Quoted_Escape_Type'Image (Expected.Quoted_Escape));
      end if;

      if Result.Char_Encoding /= Expected.Char_Encoding then
         Test.Info ("Found character encoding "
           & Character_Encoding'Image (Result.Char_Encoding)
           & ", expected "
           & Character_Encoding'Image (Expected.Char_Encoding));
      end if;

      if Result.Newline /= Expected.Newline then
         Test.Info ("Found newline encoding "
           & Newline_Encoding'Image (Result.Newline)
           & ", expected "
           & Newline_Encoding'Image (Expected.Newline));
      end if;

      if Result.Fallback /= Result.Fallback then
         Test.Info ("Found fallback atom encoding "
           & Atom_Encoding'Image (Result.Fallback)
           & ", expected "
           & Atom_Encoding'Image (Expected.Fallback));
      end if;
   end Check_Param;



   -------------------------
   -- Complete Test Suite --
   -------------------------

   procedure All_Tests (Report : in out NT.Reporter'Class) is
   begin
      Read_Test (Report);
      Write_Test (Report);
   end All_Tests;



   ----------------------
   -- Individual Tests --
   ----------------------

   procedure Read_Test (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Read from S-expression");
   begin
      declare
         Param : Parameters := Canonical;
         Expected : Parameters;
         Input : aliased Test_Tools.Memory_Stream;
         Parser : aliased Parsers.Parser;
         Subparser : Parsers.Subparser (Parser'Access, Input'Access);
      begin
         Input.Write (To_Atom
           ("(width 80)"
            & "(newline cr-lf atom-atom all)"
            & "utf-8"
            & "(space none open-open open-atom open-close atom-open atom-atom"
            & " atom-close close-open close-atom close-close)"
            & "token"
            & "(tab-stop 4)"
            & "single-line-quoted-string"
            & "(indentation 1 tab)"
            & "lower-hex"
            & "(escape hexadecimal)"));
         Expected
           := (Width => 80,
               Newline_At => (others => (others => True)),
               Space_At => (others => (others => True)),
               Tab_Stop => 4,
               Indentation => 1,
               Indent => Tabs,
               Quoted => Single_Line,
               Token => Standard_Token,
               Hex_Casing => Encodings.Lower,
               Quoted_Escape => Hex_Escape,
               Char_Encoding => UTF_8,
               Fallback => Hexadecimal,
               Newline => CR_LF);
         Test_Tools.Next_And_Check (Test, Subparser, Events.Open_List, 1);
         Update (Param, Subparser);
         Check_Param (Test, Param, Expected, "In first expression:");

         Input.Write (To_Atom
           ("(indentation 3 spaces)width(token extended)"
            & "(newline (not close-close))"));
         Expected.Indentation := 3;
         Expected.Indent := Spaces;
         Expected.Width := 0;
         Expected.Token := Extended_Token;
         Expected.Newline_At (Closing, Closing) := False;
         Test_Tools.Next_And_Check (Test, Subparser, Events.Open_List, 1);
         Update (Param, Subparser);
         Check_Param (Test, Param, Expected, "In second expression:");

         Input.Write (To_Atom
           ("(indentation 4 tabbed-spaces)upper-hex(width (10))(token)"));
         Expected.Indentation := 4;
         Expected.Indent := Tabs_And_Spaces;
         Expected.Hex_Casing := Encodings.Upper;
         Test_Tools.Next_And_Check (Test, Subparser, Events.Open_List, 1);
         Update (Param, Subparser);
         Check_Param (Test, Param, Expected, "In third expression:");

         Input.Write (To_Atom
           ("no-indentation(token never)"));
         Expected.Indentation := 0;
         Expected.Token := No_Token;
         Test_Tools.Next_And_Check (Test, Subparser, Events.Add_Atom, 0);
         Update (Param, Subparser);
         Check_Param (Test, Param, Expected, "In fourth expression:");

         Input.Write (To_Atom
           ("lower-case(token standard)"));
         Expected.Token := Standard_Token;
         Expected.Hex_Casing := Encodings.Lower;
         Test_Tools.Next_And_Check (Test, Subparser, Events.Add_Atom, 0);
         Update (Param, Subparser);
         Check_Param (Test, Param, Expected, "In fifth expression:");
      end;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Read_Test;


   procedure Write_Test (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Write parameters into S-expression");

      procedure Testcase
        (Title : in String;
         Param : in Parameters;
         Expr : in String);

      procedure Testcase
        (Title : in String;
         Param : in Parameters;
         Expr : in String)
      is
         Buffer : aliased Test_Tools.Memory_Stream;
         Output : Printers.Canonical (Buffer'Access);
         Parser : aliased Parsers.Parser;
         Subparser : Parsers.Subparser (Parser'Access, Buffer'Access);
         Reread : Parameters := Pretty.Canonical;
      begin
         Buffer.Set_Expected (To_Atom (Expr));
         Print (Output, Param);
         Subparser.Next;
         Update (Reread, Subparser);
         Check_Param (Test, Reread, Param, Title);
         Buffer.Check_Stream (Test);
      end Testcase;
   begin
      Testcase ("First case:",
        (Width => 80,
         Newline_At => (others => (others => True)),
         Space_At => (others => (others => True)),
         Tab_Stop => 4,
         Indentation => 1,
         Indent => Tabs,
         Quoted => Single_Line,
         Token => Standard_Token,
         Hex_Casing => Encodings.Lower,
         Quoted_Escape => Hex_Escape,
         Char_Encoding => UTF_8,
         Fallback => Hexadecimal,
         Newline => CR_LF),
         "(7:newline5:cr-lf3:all)(5:space3:all)(8:tab-stop1:4)(5:width2:80)"
         & "(11:indentation1:13:tab)25:single-line-quoted-string"
         & "(6:escape11:hexadecimal)(5:token8:standard)5:utf-8"
         & "10:lower-case11:hexadecimal");

      Testcase ("Second case:",
        (Width => 0,
         Newline_At => (others => (Opening => True, others => False)),
         Space_At => (others => (Atom_Data => True, others => False)),
         Tab_Stop => 4,
         Indentation => 1,
         Indent => Spaces,
         Quoted => No_Quoted,
         Token => No_Token,
         Hex_Casing => Encodings.Upper,
         Quoted_Escape => Octal_Escape,
         Char_Encoding => ASCII,
         Fallback => Verbatim,
         Newline => LF),
         "(7:newline2:lf4:none9:open-open9:atom-open10:close-open)"
         & "(5:space4:none9:open-atom9:atom-atom10:close-atom)"
         & "(8:tab-stop1:4)"
         & "8:no-width(11:indentation1:15:space)16:no-quoted-string"
         & "(6:escape5:octal)(5:token5:never)5:ascii10:upper-case"
         & "8:verbatim");

      Testcase ("Third case:",
        (Width => 0,
         Newline_At => (others => (others => False)),
         Space_At => (others => (others => False)),
         Tab_Stop => 8,
         Indentation => 1,
         Indent => Tabs_And_Spaces,
         Quoted => When_Shorter,
         Token => Extended_Token,
         Hex_Casing => Encodings.Lower,
         Quoted_Escape => Octal_Escape,
         Char_Encoding => Latin,
         Fallback => Base64,
         Newline => CR),
         "(7:newline2:cr4:none)(5:space4:none)(8:tab-stop1:8)8:no-width"
         & "(11:indentation1:112:tabbed-space)26:quoted-string-when-shorter"
         & "(6:escape5:octal)(5:token8:extended)7:latin-1"
         & "10:lower-case7:base-64");

      Testcase ("Fourth case:",
        (Width => 0,
         Newline_At => (others => (others => True)),
         Space_At => (others => (others => True)),
         Tab_Stop => 8,
         Indentation => 3,
         Indent => Spaces,
         Quoted => No_Quoted,
         Token => No_Token,
         Hex_Casing => Encodings.Lower,
         Quoted_Escape => Octal_Escape,
         Char_Encoding => ASCII,
         Fallback => Base64,
         Newline => LF_CR),
         "(7:newline5:lf-cr3:all)(5:space3:all)(8:tab-stop1:8)8:no-width"
         & "(11:indentation1:36:spaces)16:no-quoted-string"
         & "(6:escape5:octal)(5:token5:never)5:ascii"
         & "10:lower-case7:base-64");

      Testcase ("Fifth case:",
        (Width => 0,
         Newline_At => (others => (others => True)),
         Space_At => (others => (others => True)),
         Tab_Stop => 8,
         Indentation => 2,
         Indent => Tabs,
         Quoted => No_Quoted,
         Token => No_Token,
         Hex_Casing => Encodings.Lower,
         Quoted_Escape => Octal_Escape,
         Char_Encoding => ASCII,
         Fallback => Base64,
         Newline => LF),
         "(7:newline2:lf3:all)(5:space3:all)(8:tab-stop1:8)8:no-width"
         & "(11:indentation1:24:tabs)16:no-quoted-string"
         & "(6:escape5:octal)(5:token5:never)5:ascii"
         & "10:lower-case7:base-64");

      Testcase ("Sixth case:",
        (Width => 0,
         Newline_At => (others => (others => True)),
         Space_At => (others => (others => True)),
         Tab_Stop => 8,
         Indentation => 4,
         Indent => Tabs_And_Spaces,
         Quoted => No_Quoted,
         Token => No_Token,
         Hex_Casing => Encodings.Lower,
         Quoted_Escape => Octal_Escape,
         Char_Encoding => ASCII,
         Fallback => Base64,
         Newline => LF),
         "(7:newline2:lf3:all)(5:space3:all)(8:tab-stop1:8)8:no-width"
         & "(11:indentation1:413:tabbed-spaces)16:no-quoted-string"
         & "(6:escape5:octal)(5:token5:never)5:ascii"
         & "10:lower-case7:base-64");

      Testcase ("Seventh case:",
        (Width => 0,
         Newline_At => (others => (others => True)),
         Space_At => (others => (others => True)),
         Tab_Stop => 8,
         Indentation => 0,
         Indent => Spaces,
         Quoted => No_Quoted,
         Token => No_Token,
         Hex_Casing => Encodings.Lower,
         Quoted_Escape => Octal_Escape,
         Char_Encoding => ASCII,
         Fallback => Base64,
         Newline => LF),
         "(7:newline2:lf3:all)(5:space3:all)(8:tab-stop1:8)8:no-width"
         & "14:no-indentation16:no-quoted-string"
         & "(6:escape5:octal)(5:token5:never)5:ascii"
         & "10:lower-case7:base-64");
   exception
      when Error : others => Test.Report_Exception (Error);
   end Write_Test;

end Natools.S_Expressions.Printers.Pretty.Config.Tests;
