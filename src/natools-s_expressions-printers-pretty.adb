------------------------------------------------------------------------------
-- Copyright (c) 2013-2014, Natacha Porté                                   --
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

package body Natools.S_Expressions.Printers.Pretty is

   function Fit_In_Line
     (Output : in Printer;
      Width  : in Screen_Offset)
      return Boolean;
      --  Return whether Width can fit in the current line of Output

   function Indent_Width (Output : in Printer) return Screen_Offset;
      --  Return current indentation width on screen

   function Is_Extended_Token (Data : in Atom) return Boolean;
      --  Check whether Atom can be parsed by Natools.S_Expressions.Parsers
      --  when encoded as a token.

   function Is_Standard_Token (Data : in Atom) return Boolean;
      --  Check whether Atom can be encoded as a token according to Standard

   function Is_Newline
     (Data : in Atom;
      Position : in Offset;
      Encoding : in Newline_Encoding)
      return Boolean;
      --  Check whether Data contains a newline at Position

   procedure Quoted_Lengths
     (Data        : in Atom;
      Encoding    : in Character_Encoding;
      Width       : in Screen_Offset;
      Newline     : in Newline_Encoding;
      Single_Line : in Boolean;
      Size        : out Count;
      Cursor      : in out Screen_Offset);
      --  Dry run of quoted-string encoding of Data, to measure the output
      --  length and the final position of the screen cursor.

   function Multi_Line_Quoted_Size
     (Output : in Printer;
      Data   : in Atom)
      return Count;
      --  Return how much octets a multi-line encoding of Data in Output
      --  would take.

   function Single_Line_Quoted_Size
     (Data     : in Atom;
      Encoding : in Character_Encoding)
      return Count;
   pragma Unreferenced (Single_Line_Quoted_Size);
      --  Return how much octets a single line encoding of Data would take

   function Single_Line_Quoted_Width
     (Data     : in Atom;
      Encoding : in Character_Encoding)
      return Screen_Offset;
      --  Return how much screen space a single line encoding of Data takes

   function UTF_Character_Size (Data : in Atom; Position : in Offset)
      return Count;
      --  Return how much octets spans the UTF-8 character at Position
      --  in Data, or 0 when it's an invalid UTF-8 sequence.

   procedure Write_Base64 (Output : in out Printer; Data : in Atom);
      --  Output base-64 encoding of Data

   procedure Write_Hex (Output : in out Printer; Data : in Atom);
      --  Output hexadecimal encoding of Data

   procedure Write_Quoted
     (Output : in out Printer;
      Data : in Atom;
      Single_Line : in Boolean);
      --  Output quoted-string encoding of Data

   procedure Write_Verbatim (Output : in out Printer; Data : in Atom);
      --  Output verbatim encoding of Data



   ------------------------
   -- Helper Subprograms --
   ------------------------

   function Fit_In_Line
     (Output : in Printer;
      Width  : in Screen_Offset)
      return Boolean is
   begin
      return Output.Param.Width = 0
        or Output.Param.Width >= Output.Cursor - 1 + Width;
   end Fit_In_Line;


   function Indent_Width (Output : in Printer) return Screen_Offset is
   begin
      if Output.Indent_Level > 0 and Output.Param.Indentation > 0 then
         case Output.Param.Indent is
            when Spaces | Tabs_And_Spaces =>
               return Output.Param.Indentation * Output.Indent_Level;
            when Tabs =>
               return Output.Param.Indentation * Output.Indent_Level
                 * Output.Param.Tab_Stop;
         end case;
      else
         return 0;
      end if;
   end Indent_Width;


   function Is_Extended_Token (Data : in Atom) return Boolean is
   begin
      for I in Data'Range loop
         case Data (I) is
            when 0 | Encodings.Space | Encodings.HT | Encodings.VT
                   | Encodings.LF | Encodings.CR | Encodings.FF
                   | Encodings.List_Begin | Encodings.List_End =>
               return False;
            when others => null;
         end case;
      end loop;

      return Data'Length > 0;
   end Is_Extended_Token;


   function Is_Standard_Token (Data : in Atom) return Boolean is
   begin
      if Data'Length = 0
        or else Data (Data'First) in Encodings.Digit_0 .. Encodings.Digit_9
      then
         return False;
      end if;

      for I in Data'Range loop
         case Data (I) is
            when Encodings.Upper_A .. Encodings.Upper_Z
              | Encodings.Lower_A .. Encodings.Lower_Z
              | Encodings.Digit_0 .. Encodings.Digit_9
              | Character'Pos ('-')
              | Character'Pos ('.')
              | Character'Pos ('/')
              | Character'Pos ('_')
              | Character'Pos (':')
              | Character'Pos ('*')
              | Character'Pos ('+')
              | Character'Pos ('=') =>
               null;
            when others => return False;
         end case;
      end loop;

      return True;
   end Is_Standard_Token;


   function UTF_Character_Size (Data : in Atom; Position : in Offset)
      return Count is
   begin
      case Data (Position) is
         when 2#0000_0000# .. 2#0111_1111# =>
            return 1;
         when 2#1000_0000# .. 2#1011_1111# =>
            return 0;
         when 2#1100_0000# .. 2#1101_1111# =>
            if Position + 1 in Data'Range
              and then Data (Position + 1) in 2#1000_0000# .. 2#1011_1111#
            then
               return 2;
            else
               return 0;
            end if;
         when 2#1110_0000# .. 2#1110_1111# =>
            if Position + 2 in Data'Range
              and then Data (Position + 1) in 2#1000_0000# .. 2#1011_1111#
              and then Data (Position + 2) in 2#1000_0000# .. 2#1011_1111#
            then
               return 3;
            else
               return 0;
            end if;
         when 2#1111_0000# .. 2#1111_0111# =>
            if Position + 3 in Data'Range
              and then Data (Position + 1) in 2#1000_0000# .. 2#1011_1111#
              and then Data (Position + 2) in 2#1000_0000# .. 2#1011_1111#
              and then Data (Position + 3) in 2#1000_0000# .. 2#1011_1111#
            then
               return 4;
            else
               return 0;
            end if;
         when 2#1111_1000# .. 2#1111_1011# =>
            if Position + 4 in Data'Range
              and then Data (Position + 1) in 2#1000_0000# .. 2#1011_1111#
              and then Data (Position + 2) in 2#1000_0000# .. 2#1011_1111#
              and then Data (Position + 3) in 2#1000_0000# .. 2#1011_1111#
              and then Data (Position + 4) in 2#1000_0000# .. 2#1011_1111#
            then
               return 5;
            else
               return 0;
            end if;
         when 2#1111_1100# .. 2#1111_1101# =>
            if Position + 5 in Data'Range
              and then Data (Position + 1) in 2#1000_0000# .. 2#1011_1111#
              and then Data (Position + 2) in 2#1000_0000# .. 2#1011_1111#
              and then Data (Position + 3) in 2#1000_0000# .. 2#1011_1111#
              and then Data (Position + 4) in 2#1000_0000# .. 2#1011_1111#
              and then Data (Position + 5) in 2#1000_0000# .. 2#1011_1111#
            then
               return 6;
            else
               return 0;
            end if;
         when 2#1111_1110# .. 2#1111_1111# =>
            return 0;
      end case;
   end UTF_Character_Size;


   function Is_Newline
     (Data : in Atom;
      Position : in Offset;
      Encoding : in Newline_Encoding)
      return Boolean is
   begin
      case Encoding is
         when CR =>
            return Data (Position) = Encodings.CR;
         when LF =>
            return Data (Position) = Encodings.LF;
         when CR_LF =>
            return Position + 1 in Data'Range
              and then Data (Position) = Encodings.CR
              and then Data (Position + 1) = Encodings.LF;
         when LF_CR =>
            return Position + 1 in Data'Range
              and then Data (Position) = Encodings.LF
              and then Data (Position + 1) = Encodings.CR;
      end case;
   end Is_Newline;


   procedure Newline (Output : in out Printer) is
      Data : Atom (0 .. 1);
      Length : Count;
      Writer : Printer'Class renames Printer'Class (Output);
   begin
      case Output.Param.Newline is
         when CR =>
            Data (0) := Encodings.CR;
            Length := 1;
         when LF =>
            Data (0) := Encodings.LF;
            Length := 1;
         when CR_LF =>
            Data (0) := Encodings.CR;
            Data (1) := Encodings.LF;
            Length := 2;
         when LF_CR =>
            Data (0) := Encodings.LF;
            Data (1) := Encodings.CR;
            Length := 2;
      end case;
      Writer.Write_Raw (Data (0 .. Length - 1));

      if Output.Indent_Level > 0 and Output.Param.Indentation > 0 then
         case Output.Param.Indent is
            when Spaces =>
               Output.Cursor := Output.Param.Indentation * Output.Indent_Level
                                + 1;
               Writer.Write_Raw
                 ((1 .. Count (Output.Cursor) - 1 => Encodings.Space));
            when Tabs =>
               Output.Cursor := Output.Param.Indentation * Output.Indent_Level;
               Writer.Write_Raw ((1 .. Count (Output.Cursor) => Encodings.HT));
               Output.Cursor := Output.Cursor * Output.Param.Tab_Stop + 1;
            when Tabs_And_Spaces =>
               Output.Cursor := Output.Param.Indentation * Output.Indent_Level
                                + 1;
               declare
                  Tab_Count : constant Count
                    := (Count (Output.Cursor) - 1)
                        / Count (Output.Param.Tab_Stop);
                  Space_Count : constant Count
                    := (Count (Output.Cursor) - 1)
                        mod Count (Output.Param.Tab_Stop);
               begin
                  Writer.Write_Raw ((1 .. Tab_Count => Encodings.HT));
                  Writer.Write_Raw ((1 .. Space_Count => Encodings.Space));
               end;
         end case;
      else
         Output.Cursor := 1;
      end if;
   end Newline;


   procedure Quoted_Lengths
     (Data        : in Atom;
      Encoding    : in Character_Encoding;
      Width       : in Screen_Offset;
      Newline     : in Newline_Encoding;
      Single_Line : in Boolean;
      Size        : out Count;
      Cursor      : in out Screen_Offset)
   is
      C : Count;
      I : Offset := Data'First;
      Last_Non_NL : Offset := Data'Last;
      Input_Delta : Count;
      Output_Delta : Count;
      Width_Adjust : Offset;
      New_Cursor : Screen_Column;
   begin
      while Last_Non_NL in Data'Range
        and then (Data (Last_Non_NL) = Encodings.CR
                  or Data (Last_Non_NL) = Encodings.LF)
      loop
         Last_Non_NL := Last_Non_NL - 1;
      end loop;

      Size := 2;
      Cursor := Cursor + 1;
      while I in Data'Range loop
         Input_Delta := 1;
         Width_Adjust := 0;
         case Data (I) is
            when 8 | 9 | 11 | 12
              | Encodings.Quoted_Atom_End | Encodings.Escape =>
               Output_Delta := 2;
            when 10 | 13 =>
               if Single_Line
                 or else I > Last_Non_NL
                 or else not Is_Newline (Data, I, Newline)
               then
                  Output_Delta := 2;
               else
                  Width_Adjust := -Offset (Cursor);
                  case Newline is
                     when LF | CR =>
                        Output_Delta := 1;
                     when CR_LF | LF_CR =>
                        Output_Delta := 2;
                        Input_Delta := 2;
                        Width_Adjust := Width_Adjust - 1;
                  end case;
               end if;
            when 0 .. 7 | 14 .. 31 =>
               Output_Delta := 4;
            when 16#80# .. 16#FF# =>
               case Encoding is
                  when ASCII =>
                     Output_Delta := 4;
                  when Latin =>
                     if Data (I) in 16#80# .. 16#9F# then
                        Output_Delta := 4;
                     else
                        Output_Delta := 1;
                     end if;
                  when UTF_8 =>
                     C := UTF_Character_Size (Data, I);
                     if C = 0 then
                        Output_Delta := 4;
                     else
                        Output_Delta := C;
                        Input_Delta := C;
                        Width_Adjust := 1 - C;
                     end if;
               end case;
            when others =>
               Output_Delta := 1;
         end case;

         New_Cursor := Screen_Column
           (Offset (Cursor) + Output_Delta + Width_Adjust);

         if not Single_Line
           and then Width > 0
           and then Cursor > 1
           and then (New_Cursor > Width + 1
             or else (New_Cursor = Width + 1
               and then I + 1 in Data'Range
               and then not Is_Newline (Data, I + 1, Newline)))
         then
            case Newline is
               when CR | LF =>
                  Size := Size + 2;
               when CR_LF | LF_CR =>
                  Size := Size + 3;
            end case;
            Cursor := 1;
         else
            I := I + Input_Delta;
            Size := Size + Output_Delta;
            Cursor := New_Cursor;
         end if;
      end loop;

      Cursor := Cursor + 1;
   end Quoted_Lengths;


   function Multi_Line_Quoted_Size
     (Output : in Printer;
      Data   : in Atom)
      return Count
   is
      Discarded_Cursor : Screen_Offset := Output.Cursor;
      Result : Count;
   begin
      Quoted_Lengths
        (Data,
         Output.Param.Char_Encoding,
         Output.Param.Width,
         Output.Param.Newline,
         False,
         Result,
         Discarded_Cursor);
      return Result;
   end Multi_Line_Quoted_Size;


   function Single_Line_Quoted_Size
     (Data     : in Atom;
      Encoding : in Character_Encoding)
      return Count
   is
      Discarded : Screen_Offset := 0;
      Result : Count;
   begin
      Quoted_Lengths
        (Data, Encoding,
         0, CR, True,
         Result, Discarded);
      return Result;
   end Single_Line_Quoted_Size;


   function Single_Line_Quoted_Width
     (Data     : in Atom;
      Encoding : in Character_Encoding)
      return Screen_Offset
   is
      Result : Screen_Offset := 0;
      Discarded : Count;
   begin
      Quoted_Lengths
        (Data, Encoding,
         0, CR, True,
         Discarded, Result);
      return Result;
   end Single_Line_Quoted_Width;


   procedure Write_Base64 (Output : in out Printer; Data : in Atom) is
      Available : Screen_Offset;
      I : Offset := Data'First;
      Chunk_Size : Count;
      Writer : Printer'Class renames Printer'Class (Output);
   begin
      if Output.Param.Width = 0 then
         Writer.Write_Raw ((0 => Encodings.Base64_Atom_Begin));
         Writer.Write_Raw (Encodings.Encode_Base64 (Data));
         Writer.Write_Raw ((0 => Encodings.Base64_Atom_End));
      else
         Writer.Write_Raw ((0 => Encodings.Base64_Atom_Begin));
         Output.Cursor := Output.Cursor + 1;

         loop
            Available := Output.Param.Width + 1 - Output.Cursor;
            Chunk_Size := Count'Max (1, Count (Available) / 4) * 3;

            if Available mod 4 /= 0 and then I in Data'Range then
               Writer.Write_Raw
                 (((1 .. Count (Available mod 4) => Encodings.Space)));
               Output.Cursor := Output.Cursor + (Available mod 4);
            end if;

            if I + Chunk_Size - 1 in Data'Range then
               Writer.Write_Raw
                 (Encodings.Encode_Base64 (Data (I .. I + Chunk_Size - 1)));
               Newline (Output);
               I := I + Chunk_Size;
            else
               Writer.Write_Raw
                 (Encodings.Encode_Base64 (Data (I .. Data'Last)));
               Writer.Write_Raw ((0 => Encodings.Base64_Atom_End));
               Output.Cursor := Output.Cursor
                 + Screen_Offset (Data'Last - I + 2) / 3 * 4 + 1;
               exit;
            end if;
         end loop;
      end if;
   end Write_Base64;


   procedure Write_Hex (Output : in out Printer; Data : in Atom) is
      Available : Screen_Offset;
      I : Offset := Data'First;
      Chunk_Size : Count;
      Writer : Printer'Class renames Printer'Class (Output);
   begin
      if Output.Param.Width = 0 then
         Writer.Write_Raw ((0 => Encodings.Hex_Atom_Begin));
         Writer.Write_Raw
           (Encodings.Encode_Hex (Data, Output.Param.Hex_Casing));
         Writer.Write_Raw ((0 => Encodings.Hex_Atom_End));
      else
         Writer.Write_Raw ((0 => Encodings.Hex_Atom_Begin));
         Output.Cursor := Output.Cursor + 1;

         loop
            Available := Output.Param.Width + 1 - Output.Cursor;
            Chunk_Size := Count'Max (1, Count (Available) / 2);

            if Available mod 2 = 1 and then I in Data'Range then
               Writer.Write_Raw ((0 => Encodings.Space));
               Output.Cursor := Output.Cursor + 1;
            end if;

            if I + Chunk_Size - 1 in Data'Range then
               Writer.Write_Raw
                 (Encodings.Encode_Hex
                    (Data (I .. I + Chunk_Size - 1),
                     Output.Param.Hex_Casing));
               Newline (Output);
               I := I + Chunk_Size;
            else
               Writer.Write_Raw
                 (Encodings.Encode_Hex
                    (Data (I .. Data'Last),
                     Output.Param.Hex_Casing));
               Writer.Write_Raw ((0 => Encodings.Hex_Atom_End));
               Output.Cursor := Output.Cursor
                 + Screen_Offset (Data'Last - I + 1) * 2 + 1;
               exit;
            end if;
         end loop;
      end if;
   end Write_Hex;


   procedure Write_Quoted
     (Output : in out Printer;
      Data : in Atom;
      Single_Line : in Boolean)
   is
      procedure Escape
        (Value : in Octet;
         Result : in out Atom;
         Pos : in Offset);

      Size : Count;
      Last_Non_NL : Offset := Data'Last;
      Expected_Cursor : Screen_Offset := Output.Cursor;

      procedure Escape
        (Value : in Octet;
         Result : in out Atom;
         Pos : in Offset) is
      begin
         Result (Pos) := Encodings.Escape;

         case Output.Param.Quoted_Escape is
            when Octal_Escape =>
               Result (Pos + 1) := Encodings.Digit_0 + (Value / 2**6);
               Result (Pos + 2) := Encodings.Digit_0 + (Value / 2**3) mod 2**3;
               Result (Pos + 3) := Encodings.Digit_0 + (Value mod 2**3);
            when Hex_Escape =>
               Result (Pos + 1) := Character'Pos ('x');
               Encodings.Encode_Hex
                 (Value,
                  Output.Param.Hex_Casing,
                  Result (Pos + 2),
                  Result (Pos + 3));
         end case;
      end Escape;
   begin
      Quoted_Lengths
        (Data,
         Output.Param.Char_Encoding,
         Output.Param.Width,
         Output.Param.Newline,
         Single_Line,
         Size,
         Expected_Cursor);

      while Last_Non_NL in Data'Range
        and then (Data (Last_Non_NL) = Encodings.CR
                  or Data (Last_Non_NL) = Encodings.LF)
      loop
         Last_Non_NL := Last_Non_NL - 1;
      end loop;

      declare
         Result : Atom (0 .. Size - 1);
         I : Offset := Data'First;
         O : Offset := Result'First + 1;
         C : Count;
         Input_Delta : Count;
         Output_Delta : Count;
         Width_Adjust : Offset;
         New_Cursor : Screen_Column;
      begin
         Result (0) := Encodings.Quoted_Atom_Begin;
         Output.Cursor := Output.Cursor + 1;

         while I in Data'Range loop
            Output_Delta := 1;
            Width_Adjust := 0;
            Input_Delta := 1;

            case Data (I) is
               when 8 =>
                  Result (O) := Encodings.Escape;
                  Result (O + 1) := Character'Pos ('b');
                  Output_Delta := 2;
               when 9 =>
                  Result (O) := Encodings.Escape;
                  Result (O + 1) := Character'Pos ('t');
                  Output_Delta := 2;
               when 10 =>
                  if Single_Line
                    or else I > Last_Non_NL
                    or else not Is_Newline (Data, I, Output.Param.Newline)
                  then
                     Result (O) := Encodings.Escape;
                     Result (O + 1) := Character'Pos ('n');
                     Output_Delta := 2;
                  else
                     Result (O) := Data (I);
                     Width_Adjust := -Offset (Output.Cursor);
                     if Output.Param.Newline = CR_LF
                       or Output.Param.Newline = LF_CR
                     then
                        Input_Delta := 2;
                        Result (O + 1) := Data (I + 1);
                        Output_Delta := 2;
                        Width_Adjust := Width_Adjust - 1;
                     end if;
                  end if;
               when 11 =>
                  Result (O) := Encodings.Escape;
                  Result (O + 1) := Character'Pos ('v');
                  Output_Delta := 2;
               when 12 =>
                  Result (O) := Encodings.Escape;
                  Result (O + 1) := Character'Pos ('f');
                  Output_Delta := 2;
               when 13 =>
                  if Single_Line
                    or else I > Last_Non_NL
                    or else not Is_Newline (Data, I, Output.Param.Newline)
                  then
                     Result (O) := Encodings.Escape;
                     Result (O + 1) := Character'Pos ('r');
                     Output_Delta := 2;
                  else
                     Result (O) := Data (I);
                     Width_Adjust := -Offset (Output.Cursor);
                     if Output.Param.Newline = CR_LF
                       or Output.Param.Newline = LF_CR
                     then
                        Input_Delta := 2;
                        Result (O + 1) := Data (I + 1);
                        Output_Delta := 2;
                        Width_Adjust := Width_Adjust - 1;
                     end if;
                  end if;
               when Encodings.Quoted_Atom_End | Encodings.Escape =>
                  Result (O) := Encodings.Escape;
                  Result (O + 1) := Data (I);
                  Output_Delta := 2;
               when 0 .. 7 | 14 .. 31 =>
                  Escape (Data (I), Result, O);
                  Output_Delta := 4;
               when 16#80# .. 16#FF# =>
                  case Output.Param.Char_Encoding is
                     when ASCII =>
                        Escape (Data (I), Result, O);
                        Output_Delta := 4;
                     when Latin =>
                        if Data (I) in 16#80# .. 16#9F# then
                           Escape (Data (I), Result, O);
                           Output_Delta := 4;
                        else
                           Result (O) := Data (I);
                        end if;
                     when UTF_8 =>
                        C := UTF_Character_Size (Data, I);
                        if C = 0 then
                           Escape (Data (I), Result, O);
                           Output_Delta := 4;
                        else
                           Result (O .. O + C - 1) := Data (I .. I + C - 1);
                           Input_Delta := C;
                           Output_Delta := C;
                           Width_Adjust := 1 - C;
                        end if;
                  end case;
               when others =>
                  Result (O) := Data (I);
            end case;

            New_Cursor := Screen_Column
              (Offset (Output.Cursor) + Output_Delta + Width_Adjust);

            if not Single_Line
              and then Output.Param.Width > 0
              and then Output.Cursor > 1
              and then (New_Cursor > Output.Param.Width + 1
                or else (New_Cursor = Output.Param.Width + 1
                  and then I + 1 in Data'Range
                  and then not Is_Newline (Data, I + 1, Output.Param.Newline)))
            then
               Result (O) := Encodings.Escape;
               case Output.Param.Newline is
                  when CR =>
                     Result (O + 1) := Encodings.CR;
                     O := O + 2;
                  when LF =>
                     Result (O + 1) := Encodings.LF;
                     O := O + 2;
                  when CR_LF =>
                     Result (O + 1) := Encodings.CR;
                     Result (O + 2) := Encodings.LF;
                     O := O + 3;
                  when LF_CR =>
                     Result (O + 1) := Encodings.LF;
                     Result (O + 2) := Encodings.CR;
                     O := O + 3;
               end case;
               Output.Cursor := 1;
            else
               I := I + Input_Delta;
               O := O + Output_Delta;
               Output.Cursor := New_Cursor;
            end if;
         end loop;

         pragma Assert (O = Result'Last);
         Result (O) := Encodings.Quoted_Atom_End;
         Output.Cursor := Output.Cursor + 1;

         Write_Raw (Printer'Class (Output), Result);
      end;

      pragma Assert (Output.Cursor = Expected_Cursor);
   end Write_Quoted;


   procedure Write_Verbatim (Output : in out Printer; Data : in Atom) is
      Length_Image : constant String := Count'Image (Data'Length);
      Prefix : Atom (0 .. Length_Image'Length - 1);
   begin
      for I in Length_Image'First + 1 .. Length_Image'Last loop
         Prefix (Count (I) - Count (Length_Image'First + 1)) :=
           Character'Pos (Length_Image (I));
      end loop;
      Prefix (Prefix'Last) := Encodings.Verbatim_Begin;

      Write_Raw (Printer'Class (Output), Prefix);
      Write_Raw (Printer'Class (Output), Data);
      Output.Cursor := Output.Cursor + Screen_Offset (Prefix'Length)
        + Screen_Offset (Data'Length);
   end Write_Verbatim;



   -----------------------
   -- Printer Interface --
   -----------------------

   overriding procedure Open_List (Output : in out Printer) is
   begin
      if Output.Param.Width > 0
        and then Output.Cursor > Output.Param.Width
        and then Output.Cursor > Indent_Width (Output) + 1
      then
         Newline (Output);
         Output.First := True;  --  inhibit extra space or newline
      end if;

      if not Output.First then
         if Output.Param.Newline_At (Output.Previous, Opening) then
            Newline (Output);
         elsif Output.Param.Space_At (Output.Previous, Opening) then
            Write_Raw (Printer'Class (Output), (0 => Encodings.Space));
            Output.Cursor := Output.Cursor + 1;
         end if;
      else
         Output.First := False;
      end if;

      Write_Raw (Printer'Class (Output), (0 => Encodings.List_Begin));
      Output.Cursor := Output.Cursor + 1;
      Output.Indent_Level := Output.Indent_Level + 1;
      Output.Previous := Opening;
      Output.Need_Blank := False;
   end Open_List;


   overriding procedure Append_Atom (Output : in out Printer;
                                     Data : in Atom)
   is
      Blank_Width : Screen_Offset;
      At_Origin : Boolean := False;
   begin
      if not Output.First then
         if Output.Param.Newline_At (Output.Previous, Atom_Data) then
            Newline (Output);
            Output.Need_Blank := False;
            At_Origin := True;
         elsif Output.Param.Space_At (Output.Previous, Atom_Data) then
            Output.Need_Blank := True;
         end if;
      else
         Output.First := False;
         Output.Need_Blank := False;
         At_Origin := True;
      end if;
      Output.Previous := Atom_Data;

      if Output.Need_Blank then
         Blank_Width := 1;
      else
         Blank_Width := 0;
      end if;

      --  Token encoding if requested and possible

      if (Output.Param.Token = Extended_Token
          and then Is_Extended_Token (Data))
        or else (Output.Param.Token = Standard_Token
                 and then Is_Standard_Token (Data))
      then
         declare
            Width : constant Screen_Offset
              := Single_Line_Quoted_Width (Data, Output.Param.Char_Encoding)
                 - 2;
         begin
            if not At_Origin
              and then not Fit_In_Line (Output, Blank_Width + Width)
            then
               Newline (Output);
            elsif Output.Need_Blank then
               Write_Raw (Printer'Class (Output), (0 => Encodings.Space));
               Output.Cursor := Output.Cursor + 1;
            end if;
            Write_Raw (Printer'Class (Output), Data);
            Output.Cursor := Output.Cursor + Width;
            Output.Need_Blank := True;
            return;
         end;
      end if;

      --  Single-line quoted string if requested and possible

      if Output.Param.Quoted = Single_Line then
         declare
            Width : constant Screen_Offset
              := Single_Line_Quoted_Width (Data, Output.Param.Char_Encoding);
         begin
            if Fit_In_Line (Output, Blank_Width + Width) then
               if Output.Need_Blank then
                  Write_Raw (Printer'Class (Output), (0 => Encodings.Space));
                  Output.Cursor := Output.Cursor + 1;
               end if;
               Write_Quoted (Output, Data, True);
               Output.Need_Blank := False;
               return;
            end if;

            if Indent_Width (Output) + Width <= Output.Param.Width then
               Newline (Output);
               Write_Quoted (Output, Data, True);
               Output.Need_Blank := False;
               return;
            end if;
         end;
      end if;

      --  Fall back on a universal token encoding

      declare
         Size : Count;
      begin
         case Output.Param.Fallback is
            when Base64 =>
               Size := (Data'Length + 2) / 3 * 4 + 2;
            when Hexadecimal =>
               Size := Data'Length * 2 + 2;
            when Verbatim =>
               declare
                  I : Count := 10;
               begin
                  Size := 2;
                  while Data'Length >= I loop
                     Size := Size + 1;
                     I := I * 10;
                  end loop;
                  Size := Size + Data'Length;
               end;
         end case;

         if Output.Param.Quoted = When_Shorter
           and then Multi_Line_Quoted_Size (Output, Data) <= Size
         then
            if Output.Need_Blank then
               Write_Raw (Printer'Class (Output), (0 => Encodings.Space));
               Output.Cursor := Output.Cursor + 1;
            end if;
            Write_Quoted (Output, Data, False);
            Output.Need_Blank := False;
            return;
         end if;

         if not At_Origin
           and then
           not Fit_In_Line (Output, Blank_Width + Screen_Offset (Size))
         then
            Newline (Output);
         elsif Output.Need_Blank then
            Write_Raw (Printer'Class (Output), (0 => Encodings.Space));
            Output.Cursor := Output.Cursor + 1;
         end if;

         case Output.Param.Fallback is
            when Base64 =>
               Write_Base64 (Output, Data);
            when Hexadecimal =>
               Write_Hex (Output, Data);
            when Verbatim =>
               Write_Verbatim (Output, Data);
         end case;
         Output.Need_Blank := False;
      end;
   end Append_Atom;


   overriding procedure Close_List (Output : in out Printer) is
   begin
      Output.Indent_Level := Output.Indent_Level - 1;

      if Output.Param.Width > 0
        and then Output.Cursor > Output.Param.Width
        and then Output.Cursor > Indent_Width (Output) + 1
      then
         Newline (Output);
         Output.First := True;  --  inhibit extra space or newline
      end if;

      if not Output.First then
         if Output.Param.Newline_At (Output.Previous, Closing) then
            Newline (Output);
         elsif Output.Param.Space_At (Output.Previous, Closing) then
            Write_Raw (Printer'Class (Output), (0 => Encodings.Space));
            Output.Cursor := Output.Cursor + 1;
         end if;
      else
         Output.First := False;
      end if;

      Write_Raw (Printer'Class (Output), (0 => Encodings.List_End));
      Output.Cursor := Output.Cursor + 1;
      Output.Previous := Closing;
      Output.Need_Blank := False;
   end Close_List;



   ---------------------------
   -- Configuration Methods --
   ---------------------------

   procedure Set_Parameters (Output : in out Printer; Param : in Parameters) is
   begin
      Output.Param := Param;
   end Set_Parameters;


   function Get_Parameters (Output : Printer) return Parameters is
   begin
      return Output.Param;
   end Get_Parameters;


   procedure Set_Width
     (Output : in out Printer;
      Width : in Screen_Offset) is
   begin
      Output.Param.Width := Width;
   end Set_Width;


   procedure Set_Newline_At
     (Output : in out Printer;
      Newline_At : in Entity_Separator) is
   begin
      Output.Param.Newline_At := Newline_At;
   end Set_Newline_At;


   procedure Set_Space_At
     (Output : in out Printer;
      Space_At : in Entity_Separator) is
   begin
      Output.Param.Space_At := Space_At;
   end Set_Space_At;


   procedure Set_Tab_Stop
     (Output : in out Printer;
      Tab_Stop : in Screen_Column) is
   begin
      Output.Param.Tab_Stop := Tab_Stop;
   end Set_Tab_Stop;


   procedure Set_Indentation
     (Output : in out Printer;
      Indentation : in Screen_Offset) is
   begin
      Output.Param.Indentation := Indentation;
   end Set_Indentation;


   procedure Set_Indent
     (Output : in out Printer;
      Indent : in Indent_Type) is
   begin
      Output.Param.Indent := Indent;
   end Set_Indent;


   procedure Set_Quoted
     (Output : in out Printer;
      Quoted : in Quoted_Option) is
   begin
      Output.Param.Quoted := Quoted;
   end Set_Quoted;


   procedure Set_Token
     (Output : in out Printer;
      Token : in Token_Option) is
   begin
      Output.Param.Token := Token;
   end Set_Token;


   procedure Set_Hex_Casing
     (Output : in out Printer;
      Hex_Casing : in Encodings.Hex_Casing) is
   begin
      Output.Param.Hex_Casing := Hex_Casing;
   end Set_Hex_Casing;


   procedure Set_Quoted_Escape
     (Output : in out Printer;
      Quoted_Escape : in Quoted_Escape_Type) is
   begin
      Output.Param.Quoted_Escape := Quoted_Escape;
   end Set_Quoted_Escape;


   procedure Set_Char_Encoding
     (Output : in out Printer;
      Char_Encoding : in Character_Encoding) is
   begin
      Output.Param.Char_Encoding := Char_Encoding;
   end Set_Char_Encoding;


   procedure Set_Fallback
     (Output : in out Printer;
      Fallback : in Atom_Encoding) is
   begin
      Output.Param.Fallback := Fallback;
   end Set_Fallback;


   procedure Set_Newline
     (Output : in out Printer;
      Newline : in Newline_Encoding) is
   begin
      Output.Param.Newline := Newline;
   end Set_Newline;


   overriding procedure Write_Raw
     (Output : in out Stream_Printer;
      Data : in Ada.Streams.Stream_Element_Array) is
   begin
      Output.Stream.Write (Data);
   end Write_Raw;

end Natools.S_Expressions.Printers.Pretty;
