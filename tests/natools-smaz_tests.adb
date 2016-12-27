------------------------------------------------------------------------------
-- Copyright (c) 2015-2016, Natacha Porté                                   --
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
with Ada.Strings.Unbounded;
with Natools.S_Expressions;
with Natools.Smaz_256;
with Natools.Smaz_64;
with Natools.Smaz_Generic;
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

   function Direct_Image (S : Ada.Streams.Stream_Element_Array) return String
     renames Natools.S_Expressions.To_String;

   function To_SEA (S : String) return Ada.Streams.Stream_Element_Array
     renames Natools.S_Expressions.To_Atom;


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
         Values => " ee stainruos l dt enescm pépd de lere ld"
            & "e" & LF & "on cqumede mentes aiquen teerou    r  sque , is m q"
            & "ueà v'tiweblogfanj." & LF & LF & "ch",
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
   end All_Tests;



   ------------------------------
   -- Test Suite for Each Base --
   ------------------------------

   procedure All_Tests_256 (Report : in out NT.Reporter'Class) is
   begin
      Test_Validity_256 (Report);
      Sample_Strings_256 (Report);
   end All_Tests_256;


   procedure All_Tests_64 (Report : in out NT.Reporter'Class) is
   begin
      Test_Validity_64 (Report);
      Sample_Strings_64 (Report);
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
   exception
      when Error : others => Test.Report_Exception (Error);
   end Sample_Strings_64;


   procedure Test_Validity_64 (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Test dictionary validity");
   begin
      if not Natools.Smaz_64.Is_Valid (Dict_64) then
         Test.Fail;
      end if;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Test_Validity_64;

end Natools.Smaz_Tests;
