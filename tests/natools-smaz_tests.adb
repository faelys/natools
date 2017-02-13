------------------------------------------------------------------------------
-- Copyright (c) 2015-2017, Natacha Porté                                   --
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
with Ada.Streams;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Natools.S_Expressions;
with Natools.Smaz_256;
with Natools.Smaz_4096;
with Natools.Smaz_64;
with Natools.Smaz_Generic;
with Natools.Smaz_Implementations.Base_4096;
with Natools.Smaz_Original;
with Natools.Smaz_Test_Base_64_Hash;

package body Natools.Smaz_Tests is

   generic
      with package Smaz is new Natools.Smaz_Generic (<>);
      with function Image (S : Ada.Streams.Stream_Element_Array) return String;
   procedure Generic_Roundtrip_Test
     (Test : in out NT.Test;
      Dict : in Smaz.Dictionary;
      Decompressed : in String;
      Compressed : in Ada.Streams.Stream_Element_Array);

   function Decimal_Image (S : Ada.Streams.Stream_Element_Array) return String;

   function Dictionary_4096 (Variable_Length_Verbatim : in Boolean)
     return Natools.Smaz_4096.Dictionary;

   function Dictionary_4096_Hash (S : in String) return Natural;

   function Direct_Image (S : Ada.Streams.Stream_Element_Array) return String
     renames Natools.S_Expressions.To_String;

   function To_SEA (S : String) return Ada.Streams.Stream_Element_Array
     renames Natools.S_Expressions.To_Atom;


   procedure Sample_Strings_4096
     (Report : in out NT.Reporter'Class;
      Dictionary : in Smaz_4096.Dictionary);

   procedure Sample_Strings_VLV_4096
     (Report : in out NT.Reporter'Class;
      Dictionary : in Smaz_4096.Dictionary);

   procedure Test_Validity_4096
     (Report : in out NT.Reporter'Class;
      Dictionary : in Smaz_4096.Dictionary);


   -----------------------
   -- Test Dictionaries --
   -----------------------

   LF : constant Character := Ada.Characters.Latin_1.LF;

   Dict_64 : constant Natools.Smaz_64.Dictionary
     := (Last_Code => 59,
         Values_Last => 119,
         Variable_Length_Verbatim => False,
         Max_Word_Length => 6,
         Offsets => (2, 3, 5, 6, 7, 8, 9, 10, 11, 12, 13, 15, 16, 18, 20, 22,
            24, 25, 26, 28, 30, 31, 32, 36, 38, 40, 42, 44, 45, 47, 49, 51, 53,
            56, 60, 63, 65, 68, 70, 72, 74, 76, 80, 82, 84, 88, 90, 92, 94, 98,
            101, 102, 103, 105, 111, 112, 114, 115, 118),
         Values => " ee stainruos l dt enescm p"
            & Character'Val (16#C3#) & Character'Val (16#A9#)
            & "pd de lere ld"
            & "e" & LF & "on cqumede mentes aiquen teerou    r  sque , is m q"
            & "ue" & Character'Val (16#C3#) & Character'Val (16#A0#)
            & " v'tiweblogfanj." & LF & LF & "ch",
         Hash => Natools.Smaz_Test_Base_64_Hash.Hash'Access);

   Dict_64_V : constant Natools.Smaz_64.Dictionary
     := (Last_Code => 59,
         Values_Last => 119,
         Variable_Length_Verbatim => True,
         Max_Word_Length => 6,
         Offsets => (2, 3, 5, 6, 7, 8, 9, 10, 11, 12, 13, 15, 16, 18, 20, 22,
            24, 25, 26, 28, 30, 31, 32, 36, 38, 40, 42, 44, 45, 47, 49, 51, 53,
            56, 60, 63, 65, 68, 70, 72, 74, 76, 80, 82, 84, 88, 90, 92, 94, 98,
            101, 102, 103, 105, 111, 112, 114, 115, 118),
         Values => " ee stainruos l dt enescm p"
            & Character'Val (16#C3#) & Character'Val (16#A9#)
            & "pd de lere ld"
            & "e" & LF & "on cqumede mentes aiquen teerou    r  sque , is m q"
            & "ue" & Character'Val (16#C3#) & Character'Val (16#A0#)
            & " v'tiweblogfanj." & LF & LF & "ch",
         Hash => Natools.Smaz_Test_Base_64_Hash.Hash'Access);



   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

   function Decimal_Image (S : Ada.Streams.Stream_Element_Array) return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;
   begin
      for I in S'Range loop
         Append (Result, Ada.Streams.Stream_Element'Image (S (I)));
      end loop;

      return To_String (Result);
   end Decimal_Image;


   function Dictionary_4096 (Variable_Length_Verbatim : in Boolean)
     return Natools.Smaz_4096.Dictionary
   is
      subtype Letter_Rank is Natural range 1 .. 26;
      function Lower (N : Letter_Rank) return Character
        is (Character'Val (Character'Pos ('a') - 1 + N));
      function Upper (N : Letter_Rank) return Character
        is (Character'Val (Character'Pos ('A') - 1 + N));

      subtype Digit_Rank is Natural range 0 .. 9;
      function Image (N : Digit_Rank) return Character
        is (Character'Val (Character'Pos ('0') + N));

      subtype Alphanum is Character with Static_Predicate
        => Alphanum in '0' .. '9' | 'A' .. 'Z' | 'a' .. 'z';

      Current_Index : Smaz_Implementations.Base_4096.Base_4096_Digit := 0;
      Current_Offset : Positive := 1;

      use type Smaz_Implementations.Base_4096.Base_4096_Digit;

      procedure Push_Value
        (Dict : in out Natools.Smaz_4096.Dictionary;
         Value : in String);

      procedure Push_Value
        (Dict : in out Natools.Smaz_4096.Dictionary;
         Value : in String) is
      begin
         if Current_Index > 0 then
            Dict.Offsets (Current_Index) := Current_Offset;
         end if;

         Dict.Values (Current_Offset .. Current_Offset + Value'Length - 1)
           := Value;

         Current_Index := Current_Index + 1;
         Current_Offset := Current_Offset + Value'Length;
      end Push_Value;
   begin
      return Dict : Natools.Smaz_4096.Dictionary
        := (Last_Code => 4059,
            Values_Last => 9120,
            Variable_Length_Verbatim => Variable_Length_Verbatim,
            Max_Word_Length => 3,
            Offsets => <>,
            Values => <>,
            Hash => Dictionary_4096_Hash'Access)
      do
         --  0 .. 61: space + letter
         for L in Alphanum loop
            Push_Value (Dict, (1 => ' ', 2 => L));
         end loop;

         --  62 .. 123: letter + space
         for L in Alphanum loop
            Push_Value (Dict, (1 => L, 2 => ' '));
         end loop;

         --  124 .. 185: letter + comma
         for L in Alphanum loop
            Push_Value (Dict, (1 => L, 2 => ','));
         end loop;

         --  186 .. 247: letter + period
         for L in Alphanum loop
            Push_Value (Dict, (1 => L, 2 => '.'));
         end loop;

         --  248 .. 255: double punctuation
         for L in Character range ' ' .. ''' loop
            Push_Value (Dict, (1 => L, 2 => L));
         end loop;

         --  256 .. 355: two-digit numbers
         for U in Digit_Rank loop
            for V in Digit_Rank loop
               Push_Value (Dict, (1 => Image (U), 2 => Image (V)));
            end loop;
         end loop;

         --  356 .. 1355: three-digit numbers
         for U in Digit_Rank loop
            for V in Digit_Rank loop
               for W in Digit_Rank loop
                  Push_Value
                    (Dict, (1 => Image (U), 2 => Image (V), 3 => Image (W)));
               end loop;
            end loop;
         end loop;

         --  1356 .. 2031: two lower-case letters
         for M in Letter_Rank loop
            for N in Letter_Rank loop
               Push_Value (Dict, (1 => Lower (M), 2 => Lower (N)));
            end loop;
         end loop;

         --  2032 .. 2707: lower-case then upper-case letter
         for M in Letter_Rank loop
            for N in Letter_Rank loop
               Push_Value (Dict, (1 => Lower (M), 2 => Upper (N)));
            end loop;
         end loop;

         --  2708 .. 3383: upper-case then lower-case letter
         for M in Letter_Rank loop
            for N in Letter_Rank loop
               Push_Value (Dict, (1 => Upper (M), 2 => Lower (N)));
            end loop;
         end loop;

         --  3384 .. 4059: two upper-case letters
         for M in Letter_Rank loop
            for N in Letter_Rank loop
               Push_Value (Dict, (1 => Upper (M), 2 => Upper (N)));
            end loop;
         end loop;

         pragma Assert (Current_Index = Dict.Last_Code + 1);
         pragma Assert (Current_Offset = Dict.Values_Last + 1);
      end return;
   end Dictionary_4096;


   function Dictionary_4096_Hash (S : in String) return Natural is
      function Rank (C : Character) return Natural
        is (case C is
            when '0' .. '9' => Character'Pos (C) - Character'Pos ('0'),
            when 'a' .. 'z' => Character'Pos (C) - Character'Pos ('a'),
            when 'A' .. 'Z' => Character'Pos (C) - Character'Pos ('A'),
            when others => raise Program_Error);
   begin
      case S'Length is
         when 2 =>
            declare
               U : constant Character := S (S'First);
               V : constant Character := S (S'Last);
            begin
               if U = ' ' and then V in '0' .. '9' then
                  return Rank (V);
               elsif U = ' ' and then V in 'A' .. 'Z' then
                  return 10 + Rank (V);
               elsif U = ' ' and then V in 'a' .. 'z' then
                  return 36 + Rank (V);
               elsif U in '0' .. '9' and then V in ' ' | ',' | '.' then
                  return Rank (U)
                    + (case V is when ' ' =>  62,
                                 when ',' => 124,
                                 when '.' => 186,
                                 when others => raise Program_Error);
               elsif U in 'A' .. 'Z' and then V in ' ' | ',' | '.' then
                  return Rank (U) + 10
                    + (case V is when ' ' =>  62,
                                 when ',' => 124,
                                 when '.' => 186,
                                 when others => raise Program_Error);
               elsif U in 'a' .. 'z' and then V in ' ' | ',' | '.' then
                  return Rank (U) + 36
                    + (case V is when ' ' =>  62,
                                 when ',' => 124,
                                 when '.' => 186,
                                 when others => raise Program_Error);
               elsif U in ' ' .. ''' and then U = V then
                  return 248 + Character'Pos (U) - Character'Pos (' ');
               elsif U in '0' .. '9' and then V in '0' .. '9' then
                  return 256 + Rank (U) * 10 + Rank (V);
               elsif U in 'a' .. 'z' and then V in 'a' .. 'z' then
                  return 1356 + Rank (U) * 26 + Rank (V);
               elsif U in 'a' .. 'z' and then V in 'A' .. 'Z' then
                  return 2032 + Rank (U) * 26 + Rank (V);
               elsif U in 'A' .. 'Z' and then V in 'a' .. 'z' then
                  return 2708 + Rank (U) * 26 + Rank (V);
               elsif U in 'A' .. 'Z' and then V in 'A' .. 'Z' then
                  return 3384 + Rank (U) * 26 + Rank (V);
               else
                  return 4095;
               end if;
            end;

         when 3 =>
            declare
               U : constant Character := S (S'First);
               V : constant Character := S (S'First + 1);
               W : constant Character := S (S'First + 2);
            begin
               if U in '0' .. '9'
                 and then V in '0' .. '9'
                 and then W in '0' .. '9'
               then
                  return 356 + Rank (U) * 100 + Rank (V) * 10 + Rank (W);
               else
                  return 4095;
               end if;
            end;
         when others =>
            return 4095;
      end case;
   end Dictionary_4096_Hash;


   procedure Generic_Roundtrip_Test
     (Test : in out NT.Test;
      Dict : in Smaz.Dictionary;
      Decompressed : in String;
      Compressed : in Ada.Streams.Stream_Element_Array)
   is
      use type Ada.Streams.Stream_Element_Array;
      use type Ada.Streams.Stream_Element_Offset;
   begin
      declare
         First_OK : Boolean := False;
      begin
         declare
            Buffer : constant Ada.Streams.Stream_Element_Array
              := Smaz.Compress (Dict, Decompressed);
         begin
            First_OK := True;

            if Buffer /= Compressed then
               Test.Fail ("Bad compression of """ & Decompressed & '"');
               Test.Info ("Found:   " & Image (Buffer));
               Test.Info ("Expected:" & Image (Compressed));

               declare
                  Round : constant String
                    := Smaz.Decompress (Dict, Buffer);
               begin
                  if Round /= Decompressed then
                     Test.Info ("Roundtrip failed, got: """ & Round & '"');
                  else
                     Test.Info ("Roundtrip OK");
                  end if;
               end;
            end if;
         end;
      exception
         when Error : others =>
            if not First_OK then
               Test.Info ("During compression of """ & Decompressed & '"');
            end if;

            Test.Report_Exception (Error, NT.Fail);
      end;

      declare
         First_OK : Boolean := False;
      begin
         declare
            Buffer : constant String
              := Smaz.Decompress (Dict, Compressed);
         begin
            First_OK := True;

            if Buffer /= Decompressed then
               Test.Fail ("Bad decompression of " & Image (Compressed));
               Test.Info ("Found:   """ & Buffer & '"');
               Test.Info ("Expected:""" & Decompressed & '"');

               declare
                  Round : constant Ada.Streams.Stream_Element_Array
                    := Smaz.Compress (Dict, Buffer);
               begin
                  if Round /= Compressed then
                     Test.Info ("Roundtrip failed, got: " & Image (Round));
                  else
                     Test.Info ("Roundtrip OK");
                  end if;
               end;
            end if;
         end;
      exception
         when Error : others =>
            if not First_OK then
               Test.Info ("During decompression of " & Image (Compressed));
            end if;

            Test.Report_Exception (Error, NT.Fail);
      end;
   end Generic_Roundtrip_Test;


   procedure Roundtrip_Test is new Generic_Roundtrip_Test
     (Natools.Smaz_256, Decimal_Image);

   procedure Roundtrip_Test is new Generic_Roundtrip_Test
     (Natools.Smaz_4096, Direct_Image);

   procedure Roundtrip_Test is new Generic_Roundtrip_Test
     (Natools.Smaz_64, Direct_Image);



   -------------------------
   -- Complete Test Suite --
   -------------------------

   procedure All_Tests (Report : in out NT.Reporter'Class) is
   begin
      Report.Section ("Base 256");
      All_Tests_256 (Report);
      Report.End_Section;

      Report.Section ("Base 64");
      All_Tests_64 (Report);
      Report.End_Section;

      Report.Section ("Base 4096");
      All_Tests_4096 (Report);
      Report.End_Section;
   end All_Tests;



   ------------------------------
   -- Test Suite for Each Base --
   ------------------------------

   procedure All_Tests_256 (Report : in out NT.Reporter'Class) is
   begin
      Test_Validity_256 (Report);
      Sample_Strings_256 (Report);
   end All_Tests_256;


   procedure All_Tests_4096 (Report : in out NT.Reporter'Class) is
   begin
      declare
         Dict : constant Smaz_4096.Dictionary := Dictionary_4096 (False);
      begin
         Report.Section ("Without variable-length verbatim");
         Test_Validity_4096 (Report, Dict);
         Sample_Strings_4096 (Report, Dict);
         Report.End_Section;
      end;

      declare
         Dict : constant Smaz_4096.Dictionary := Dictionary_4096 (True);
      begin
         Report.Section ("With variable-length verbatim");
         Test_Validity_4096 (Report, Dict);
         Sample_Strings_VLV_4096 (Report, Dict);
         Report.End_Section;
      end;
   end All_Tests_4096;


   procedure All_Tests_64 (Report : in out NT.Reporter'Class) is
   begin
      Test_Validity_64 (Report);
      Sample_Strings_64 (Report);
      Sample_Strings_VLV_64 (Report);
   end All_Tests_64;



   -------------------------------
   -- Individual Base-256 Tests --
   -------------------------------

   procedure Sample_Strings_256 (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Roundtrip on sample strings");
   begin
      Roundtrip_Test (Test, Smaz_Original.Dictionary,
         "This is a small string",
         (254, 84, 76, 56, 172, 62, 173, 152, 62, 195, 70));
      Roundtrip_Test (Test, Smaz_Original.Dictionary,
         "foobar",
         (220, 6, 90, 79));
      Roundtrip_Test (Test, Smaz_Original.Dictionary,
         "the end",
         (1, 171, 61));
      Roundtrip_Test (Test, Smaz_Original.Dictionary,
         "not-a-g00d-Exampl333",
         (132, 204, 4, 204, 59, 255, 12, 48, 48, 100, 45, 69, 120, 97, 109,
           112, 108, 51, 51, 51));
      Roundtrip_Test (Test, Smaz_Original.Dictionary,
         "Smaz is a simple compression library",
         (254, 83, 173, 219, 56, 172, 62, 226, 60, 87, 161, 45, 60, 33, 166,
           107, 205, 8, 90, 130, 12, 83));
      Roundtrip_Test (Test, Smaz_Original.Dictionary,
         "Nothing is more difficult, and therefore more precious, "
           & "than to be able to decide",
         (254, 78, 223, 102, 99, 116, 45, 42, 11, 129, 44, 44, 131, 38, 22, 3,
           148, 63, 210, 68, 11, 45, 42, 11, 60, 33, 28, 144, 164, 36, 203,
           143, 96, 92, 25, 90, 87, 82, 165, 215, 237, 2));
      Roundtrip_Test (Test, Smaz_Original.Dictionary,
         "this is an example of what works very well with smaz",
         (155, 56, 172, 41, 2, 250, 4, 45, 60, 87, 32, 159, 135, 65, 42, 254,
           107, 23, 231, 71, 145, 152, 243, 227, 10, 173, 219));
      Roundtrip_Test (Test, Smaz_Original.Dictionary,
         "1000 numbers 2000 will 10 20 30 compress very little",
         (255, 3, 49, 48, 48, 48, 236, 38, 45, 92, 221, 0, 255, 3, 50, 48, 48,
           48, 243, 152, 0, 255, 7, 49, 48, 32, 50, 48, 32, 51,
           48, 161, 45, 60, 33, 166, 0, 231, 71, 151, 3, 3, 87));
      Roundtrip_Test (Test, Smaz_Original.Dictionary,
         ": : : :",
         (255, 6, 58, 32, 58, 32, 58, 32, 58));
   exception
      when Error : others => Test.Report_Exception (Error);
   end Sample_Strings_256;


   procedure Test_Validity_256 (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Test dictionary validity");
   begin
      if not Natools.Smaz_256.Is_Valid (Smaz_Original.Dictionary) then
         Test.Fail;
      end if;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Test_Validity_256;



   --------------------------------
   -- Individual Base-4096 Tests --
   --------------------------------

   procedure Sample_Strings_4096
     (Report : in out NT.Reporter'Class;
      Dictionary : in Smaz_4096.Dictionary)
   is
      Test : NT.Test := Report.Item ("Roundtrip on sample strings");
   begin
      Roundtrip_Test (Test, Dictionary,
         "This is a small string",
         To_SEA ("JyuYsA0BiBscXVtBzcOcka"));
         --       This is a small string
      Roundtrip_Test (Test, Dictionary,
         "foobar",
         To_SEA ("cX5adV"));
         --       foobar
      Roundtrip_Test (Test, Dictionary,
         "the end",
         To_SEA ("BdmBBX//kB"));
         --       the en<d >
      Roundtrip_Test (Test, Dictionary,
         "not-a-g00d-Exampl333",
         To_SEA ("sa3/01SYtcGMwQWLTsYVdbxK"));
         --       no< t-a -g0 0d->Exampl333
         --  t-a  001011_10  1011_0100  10_000110
         --  -g0  101101_00  1110_0110  00_001100
         --  0d-  000011_00  0010_0110  10_110100
      Roundtrip_Test (Test, Dictionary,
         "Smaz is a simple compression library",
         To_SEA ("0xlVsA0BiBocTauZmAEbjbGXocFbvAdYGcec"));
         --       Smaz is a simple compression library
      Roundtrip_Test (Test, Dictionary,
         "Nothing is more difficult, and therefore more precious, "
           & "than to be able to decide",
         To_SEA ("0vBdpYoBuYwAJbmBiWTXeYfdzCkAha3AGYKccXKcwAJbmBjb2WqYmd//sA"
         --       Nothing is more difficult, and therefore more precious< ,>
           & "3ACYvBIdlAmBNVuZ3AwBeWIWeW"));
         --    than to be able to decide
      Roundtrip_Test (Test, Dictionary,
         "this is an example of what works very well with smaz",
         To_SEA ("BduYsA0BZVoAieTauZyAnBPefV6AJbiZ5AFX6BMe1Z6AvYpBsclV"));
         --       this is an example of what works very well with smaz
      Roundtrip_Test (Test, Dictionary,
         "1000 numbers 2000 will 10 20 30 compress very little",
         To_SEA ("IH+AyaFaFX0BsI+AQe1ZBA+AUEDA+AOWTaKcyc5AFX6ByZNduZ"));
         --      1000 numbers 2*0 will 10 20 30 compress very little",
      Roundtrip_Test (Test, Dictionary,
         ": : : :",
         To_SEA ("5/6AiOgoDI6A"));
         --  :_:  010111_00  0000_0100  01_011100
         --  _:_  000001_00  0101_1100  00_000100
         --  :    010111_00  0000
      Roundtrip_Test (Test, Dictionary,
         (1 .. 80 => ':'),
         To_SEA (Ada.Strings.Fixed."*"
           (2, "c/" & Ada.Strings.Fixed."*" (12, "6ojO"))
           & "4/6ojO6ojO6oD"));
         --  :::  010111_00  0101_1100  01_011100
   exception
      when Error : others => Test.Report_Exception (Error);
   end Sample_Strings_4096;


   procedure Sample_Strings_4096 (Report : in out NT.Reporter'Class) is
   begin
      Sample_Strings_4096 (Report, Dictionary_4096 (False));
   end Sample_Strings_4096;


   procedure Sample_Strings_VLV_4096
     (Report : in out NT.Reporter'Class;
      Dictionary : in Smaz_4096.Dictionary)
   is
      Test : NT.Test := Report.Item ("Roundtrip on sample strings");
   begin
      Roundtrip_Test (Test, Dictionary,
         "This is a small string",
         To_SEA ("JyuYsA0BiBscXVtBzcOcka"));
         --       This is a small string
      Roundtrip_Test (Test, Dictionary,
         "foobar",
         To_SEA ("cX5adV"));
         --       foobar
      Roundtrip_Test (Test, Dictionary,
         "the end",
         To_SEA ("BdmBBX+/kB"));
         --       the en<d >
      Roundtrip_Test (Test, Dictionary,
         "not-a-g00d-Exampl333",
         To_SEA ("sa2/01SYtcGMwQWLTsYVdbxK"));
         --       no< t-a -g0 0d->Exampl333
         --  t-a  001011_10  1011_0100  10_000110
         --  -g0  101101_00  1110_0110  00_001100
         --  0d-  000011_00  0010_0110  10_110100
      Roundtrip_Test (Test, Dictionary,
         "Smaz is a simple compression library",
         To_SEA ("0xlVsA0BiBocTauZmAEbjbGXocFbvAdYGcec"));
         --       Smaz is a simple compression library
      Roundtrip_Test (Test, Dictionary,
         "Nothing is more difficult, and therefore more precious, "
           & "than to be able to decide",
         To_SEA ("0vBdpYoBuYwAJbmBiWTXeYfdzCkAha3AGYKccXKcwAJbmBjb2WqYmd+/sA"
         --       Nothing is more difficult, and therefore more precious< ,>
           & "3ACYvBIdlAmBNVuZ3AwBeWIWeW"));
         --    than to be able to decide
      Roundtrip_Test (Test, Dictionary,
         "this is an example of what works very well with smaz",
         To_SEA ("BduYsA0BZVoAieTauZyAnBPefV6AJbiZ5AFX6BMe1Z6AvYpBsclV"));
         --       this is an example of what works very well with smaz
      Roundtrip_Test (Test, Dictionary,
         "1000 numbers 2000 will 10 20 30 compress very little",
         To_SEA ("IH+AyaFaFX0BsI+AQe1ZBA+AUEDA+AOWTaKcyc5AFX6ByZNduZ"));
         --      1000 numbers 2*0 will 10 20 30 compress very little",
      Roundtrip_Test (Test, Dictionary,
         ": : : :",
         To_SEA ("4/6AiOgoDI6A"));
         --  :_:  010111_00  0000_0100  01_011100
         --  _:_  000001_00  0101_1100  00_000100
         --  :    010111_00  0000
      Roundtrip_Test (Test, Dictionary,
         (1 .. 4096 + 36 => ':'),
         To_SEA ("////" & Ada.Strings.Fixed."*" (1377, "6ojO") & "+/6A"));
         --  :::  010111_00  0101_1100  01_011100
   exception
      when Error : others => Test.Report_Exception (Error);
   end Sample_Strings_VLV_4096;


   procedure Sample_Strings_VLV_4096 (Report : in out NT.Reporter'Class) is
   begin
      Sample_Strings_VLV_4096 (Report, Dictionary_4096 (True));
   end Sample_Strings_VLV_4096;


   procedure Test_Validity_4096
     (Report : in out NT.Reporter'Class;
      Dictionary : in Smaz_4096.Dictionary)
   is
      Test : NT.Test := Report.Item ("Test dictionary validity");
   begin
      if not Smaz_4096.Is_Valid (Dictionary) then
         Test.Fail;
      end if;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Test_Validity_4096;


   procedure Test_Validity_4096 (Report : in out NT.Reporter'Class) is
   begin
      Test_Validity_4096 (Report, Dictionary_4096 (False));
      Test_Validity_4096 (Report, Dictionary_4096 (True));
   end Test_Validity_4096;



   ------------------------------
   -- Individual Base-64 Tests --
   ------------------------------

   procedure Sample_Strings_64 (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Roundtrip on sample strings");
   begin
      Roundtrip_Test (Test, Dict_64,
         "Simple Test",
         To_SEA ("+TBGSVYA+UBQE"));
         --       <S>imp* <T>*t
      Roundtrip_Test (Test, Dict_64,
         "SiT",
         To_SEA ("/ATlGV"));
         --        <SiT>  smaller than <S>i<T> ("+TBG+UB")
      Roundtrip_Test (Test, Dict_64,
         "sIMple TEST_WITH_14_B",
         To_SEA ("D9J1EVYA8UVETR1XXlEVI9VM08lQ"));
         --       s<IM>p* <TE ST_ WIT H_1 4_B>
         --  TE   001010_10  1010_0010  00
         --  ST_  110010_10  0010_1010  11_111010
         --  WIT  111010_10  1001_0010  00_101010
         --  H_1  000100_10  1111_1010  10_001100
         --  4_B  001011_00  1111_1010  01_000010
      Roundtrip_Test (Test, Dict_64,
         "'7B_Verbatim'",
         To_SEA ("0+3IC9lVlJnYF1S0"));
         --       '<7B_Verb  >a*m'
         --  7    111011_00  0100
         --  B_V  010000_10  1111_1010  01_101010
         --  erb  101001_10  0100_1110  01_000110
         --  "erb" could have been encoded separately as "o+iB", which has
         --  the same length, but the tie is broken in favor of the longer
         --  verbatim fragment to help with corner cases.
      Roundtrip_Test (Test, Dict_64,
         "'49 bytes of data to show a verbatim count issue'",
         To_SEA ("090kTgIWenLK3NFEFAEKs/Ao92dAFAzo+iBF1SepHOvDJB0"));
         --       '<49 by >tsof_ata_to_< how>_a_ve<b>atm_ontisue'
         --                e_  d      s          r    i cu _s
         --  49   001011_00  1001_1100  10
         --   by  000001_00  0100_0110  10_011110
         --  how  000101_10  1111_0110  11_101110
         --- b    010001_10  0000
      Roundtrip_Test (Test, Dict_64,
         Character'Val (16#C3#) & Character'Val (16#A9#) & 'v'
           & Character'Val (16#C3#) & Character'Val (16#A8#) & "nement",
         To_SEA ("Uz9DjKHBi"));
         --       év<è >nement
         --  è    110000_11  0001_0101  00
   exception
      when Error : others => Test.Report_Exception (Error);
   end Sample_Strings_64;


   procedure Sample_Strings_VLV_64 (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item
        ("Roundtrip on sample strings with variable-length verbatim");
   begin
      Roundtrip_Test (Test, Dict_64_V,
         "Simple Test",
         To_SEA ("+TBGSVYA+UBQE"));
         --       <S>imp* <T>*t
      Roundtrip_Test (Test, Dict_64_V,
         "SiT",
         To_SEA ("8TlGV"));
         --        <SiT>  smaller than <S>i<T> ("+TBG+UB")
      Roundtrip_Test (Test, Dict_64_V,
         "sIMple TEST_WITH_14_B",
         To_SEA ("D9J1EVYA/KUVETR1XXlEVI9VM08lQ"));
         --       s<IM>p* < TE ST_ WIT H_1 4_B>
         --  TE   001010_10  1010_0010  00
         --  ST_  110010_10  0010_1010  11_111010
         --  WIT  111010_10  1001_0010  00_101010
         --  H_1  000100_10  1111_1010  10_001100
         --  4_B  001011_00  1111_1010  01_000010
      Roundtrip_Test (Test, Dict_64_V,
         "'7B_Verbatim'",
         To_SEA ("0/D3AC9lVlJnYF1S0"));
         --       '< 7B_Verb  >a*m'
         --  7    111011_00  0000
         --  B_V  010000_10  1111_1010  01_101010
         --  erb  101001_10  0100_1110  01_000110
   exception
      when Error : others => Test.Report_Exception (Error);
   end Sample_Strings_VLV_64;


   procedure Test_Validity_64 (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Test dictionary validity");
   begin
      if not Natools.Smaz_64.Is_Valid (Dict_64) then
         Test.Fail;
      end if;

      if not Natools.Smaz_64.Is_Valid (Dict_64_V) then
         Test.Fail;
      end if;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Test_Validity_64;

end Natools.Smaz_Tests;
