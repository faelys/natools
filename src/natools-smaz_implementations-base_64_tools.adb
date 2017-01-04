------------------------------------------------------------------------------
-- Copyright (c) 2016, Natacha Porté                                        --
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

package body Natools.Smaz_Implementations.Base_64_Tools is

   use type Ada.Streams.Stream_Element_Offset;

   -----------------------
   -- Bit Manipulations --
   -----------------------

   function Low_6 (Byte : in Character) return Base_64_Digit
     is (Base_64_Digit (Natural (Character'Pos (Byte)) mod 64));
   function Low_4 (Byte : in Character) return Single_Byte_Padding
     is (Single_Byte_Padding (Natural (Character'Pos (Byte)) mod 16));
   function Low_2 (Byte : in Character) return Double_Byte_Padding
     is (Double_Byte_Padding (Natural (Character'Pos (Byte)) mod 4));
      --  Least significant bits of a byte

   function High_6 (Byte : in Character) return Base_64_Digit
     is (Base_64_Digit (Natural (Character'Pos (Byte)) / 4));
   function High_4 (Byte : in Character) return Single_Byte_Padding
     is (Single_Byte_Padding (Natural (Character'Pos (Byte)) / 16));
   function High_2 (Byte : in Character) return Double_Byte_Padding
     is (Double_Byte_Padding (Natural (Character'Pos (Byte)) / 64));
      --  Most significant bits of a byte

   function Image_2_4 (Low : Double_Byte_Padding; High : Single_Byte_Padding)
     return Base_64_Symbol
     is (Image (Low + High * 4));
   function Image_4_2 (Low : Single_Byte_Padding; High : Double_Byte_Padding)
     return Base_64_Symbol
     is (Image (Low + High * 16));
      --  Composition into a base-64 symbol

   function Low_4 (Digit : in Base_64_Digit) return Natural
     is (Natural (Digit mod 16));
   function Low_2 (Digit : in Base_64_Digit) return Natural
     is (Natural (Digit mod 4));
      --  Least significat bits of a digit

   function High_4 (Digit : in Base_64_Digit) return Natural
     is (Natural (Digit / 4));
   function High_2 (Digit : in Base_64_Digit) return Natural
     is (Natural (Digit / 16));
      --  Least significat bits of a digit

   function Value_2_6 (Low, High : in Natural) return Character
     is (Character'Val (Low + High * 4));
   function Value_4_4 (Low, High : in Natural) return Character
     is (Character'Val (Low + High * 16));
   function Value_6_2 (Low, High : in Natural) return Character
     is (Character'Val (Low + High * 64));
      --  Combine parts of digits into a Character



   ----------------------
   -- Public Interface --
   ----------------------

   procedure Decode
     (Input : in Ada.Streams.Stream_Element_Array;
      Offset : in out Ada.Streams.Stream_Element_Offset;
      Output : out String)
   is
      Index : Positive := Output'First;
      Padding_1 : Single_Byte_Padding;
      Padding_2 : Double_Byte_Padding;
   begin
      while Index in Output'Range loop
         case Output'Last - Index + 1 is
            when 1 =>
               Decode_Single (Input, Offset, Output (Index), Padding_1);
               Index := Index + 1;

            when 2 =>
               Decode_Double
                 (Input, Offset, Output (Index .. Index + 1), Padding_2);
               Index := Index + 2;

            when others =>
               Decode_Block (Input, Offset, Output (Index .. Index + 2));
               Index := Index + 3;
         end case;
      end loop;
   end Decode;


   procedure Decode_Block
     (Input : in Ada.Streams.Stream_Element_Array;
      Offset : in out Ada.Streams.Stream_Element_Offset;
      Output : out Full_Block)
   is
      Data : array (1 .. 4) of Base_64_Digit;
   begin
      for I in Data'Range loop
         Next_Digit (Input, Offset, Data (I));
      end loop;

      Output (1) := Value_6_2 (Natural (Data (1)), Low_2   (Data (2)));
      Output (2) := Value_4_4 (High_4  (Data (2)), Low_4   (Data (3)));
      Output (3) := Value_2_6 (High_2  (Data (3)), Natural (Data (4)));
   end Decode_Block;


   procedure Decode_Double
     (Input : in Ada.Streams.Stream_Element_Array;
      Offset : in out Ada.Streams.Stream_Element_Offset;
      Output : out Double_Byte;
      Padding : out Double_Byte_Padding)
   is
      Data : array (1 .. 3) of Base_64_Digit;
   begin
      for I in Data'Range loop
         Next_Digit (Input, Offset, Data (I));
      end loop;

      Output (1) := Value_6_2 (Natural (Data (1)), Low_2   (Data (2)));
      Output (2) := Value_4_4 (High_4  (Data (2)), Low_4   (Data (3)));
      Padding := Double_Byte_Padding (High_2 (Data (3)));
   end Decode_Double;


   procedure Decode_Single
     (Input : in Ada.Streams.Stream_Element_Array;
      Offset : in out Ada.Streams.Stream_Element_Offset;
      Output : out Character;
      Padding : out Single_Byte_Padding)
   is
      Data : array (1 .. 2) of Base_64_Digit;
   begin
      for I in Data'Range loop
         Next_Digit (Input, Offset, Data (I));
      end loop;

      Output := Value_6_2 (Natural (Data (1)), Low_2   (Data (2)));
      Padding := Single_Byte_Padding (High_4 (Data (2)));
   end Decode_Single;


   procedure Encode
     (Input : in String;
      Output : in out Ada.Streams.Stream_Element_Array;
      Offset : in out Ada.Streams.Stream_Element_Offset)
   is
      Index : Positive := Input'First;
   begin
      while Index in Input'Range loop
         case Input'Last - Index + 1 is
            when 1 =>
               Encode_Single (Input (Index), 0, Output, Offset);
               Index := Index + 1;

            when 2 =>
               Encode_Double (Input (Index .. Index + 1), 0, Output, Offset);
               Index := Index + 2;

            when others =>
               Encode_Block (Input (Index .. Index + 2), Output, Offset);
               Index := Index + 3;
         end case;
      end loop;
   end Encode;


   procedure Encode_Block
     (Input : in Full_Block;
      Output : in out Ada.Streams.Stream_Element_Array;
      Offset : in out Ada.Streams.Stream_Element_Offset) is
   begin
      Output (Offset + 0) := Image (Low_6 (Input (1)));
      Output (Offset + 1) := Image_2_4 (High_2 (Input (1)), Low_4 (Input (2)));
      Output (Offset + 2) := Image_4_2 (High_4 (Input (2)), Low_2 (Input (3)));
      Output (Offset + 3) := Image (High_6 (Input (3)));
      Offset := Offset + 4;
   end Encode_Block;


   procedure Encode_Double
     (Input : in Double_Byte;
      Padding : in Double_Byte_Padding;
      Output : in out Ada.Streams.Stream_Element_Array;
      Offset : in out Ada.Streams.Stream_Element_Offset) is
   begin
      Output (Offset + 0) := Image (Low_6 (Input (1)));
      Output (Offset + 1) := Image_2_4 (High_2 (Input (1)), Low_4 (Input (2)));
      Output (Offset + 2) := Image_4_2 (High_4 (Input (2)), Padding);
      Offset := Offset + 3;
   end Encode_Double;


   procedure Encode_Single
     (Input : in Character;
      Padding : in Single_Byte_Padding;
      Output : in out Ada.Streams.Stream_Element_Array;
      Offset : in out Ada.Streams.Stream_Element_Offset) is
   begin
      Output (Offset + 0) := Image (Low_6 (Input));
      Output (Offset + 1) := Image_2_4 (High_2 (Input), Padding);
      Offset := Offset + 2;
   end Encode_Single;


   procedure Next_Digit
     (Input : in Ada.Streams.Stream_Element_Array;
      Offset : in out Ada.Streams.Stream_Element_Offset;
      Digit : out Base_64_Digit) is
   begin
      while Input (Offset) not in Base_64_Symbol loop
         Offset := Offset + 1;
      end loop;

      Digit := Value (Input (Offset));
      Offset := Offset + 1;
   end Next_Digit;


   function Symbol_Count (Input : Ada.Streams.Stream_Element_Array)
     return Ada.Streams.Stream_Element_Count
   is
      Result : Ada.Streams.Stream_Element_Count := 0;
   begin
      for S of Input loop
         if S in Base_64_Symbol then
            Result := Result + 1;
         end if;
      end loop;

      return Result;
   end Symbol_Count;

end Natools.Smaz_Implementations.Base_64_Tools;
