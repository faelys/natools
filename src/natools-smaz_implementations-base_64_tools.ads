------------------------------------------------------------------------------
-- Copyright (c) 2016-2017, Natacha Porté                                   --
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

------------------------------------------------------------------------------
-- Natools.Smaz_Implementations.Base_64_Tools provides types and primitives --
-- common to both base-64 and base-4096 variants of Smaz.                   --
------------------------------------------------------------------------------

with Ada.Streams;

package Natools.Smaz_Implementations.Base_64_Tools is
   pragma Pure;

   use type Ada.Streams.Stream_Element;


   type Base_64_Digit is range 0 .. 63;

   subtype Single_Byte_Padding is Base_64_Digit range 0 .. 15;
      --  Padding after a single-byte partial block
   subtype Double_Byte_Padding is Base_64_Digit range 0 .. 3;
      --  Padding after a double-byte partial block

   subtype Full_Block is String (1 .. 3);
   subtype Double_Byte is String (1 .. 2);

   subtype Full_Block_Image is Ada.Streams.Stream_Element_Array (1 .. 4);
   subtype Double_Byte_Image is Ada.Streams.Stream_Element_Array (1 .. 3);
   subtype Single_Byte_Image is Ada.Streams.Stream_Element_Array (1 .. 2);

   type Base_64_Array
     is array (Ada.Streams.Stream_Element_Offset range <>) of Base_64_Digit;

   subtype Base_64_Symbol is Ada.Streams.Stream_Element
     with Static_Predicate => Base_64_Symbol in
                                 Character'Pos ('A') .. Character'Pos ('Z')
                               | Character'Pos ('a') .. Character'Pos ('z')
                               | Character'Pos ('0') .. Character'Pos ('9')
                               | Character'Pos ('+')  | Character'Pos ('/');


   function Image (Digit : in Base_64_Digit) return Ada.Streams.Stream_Element
     is (case Digit is
         when 0 .. 25
           => Character'Pos ('A') + Ada.Streams.Stream_Element (Digit) - 0,
         when 26 .. 51
           => Character'Pos ('a') + Ada.Streams.Stream_Element (Digit) - 26,
         when 52 .. 61
           => Character'Pos ('0') + Ada.Streams.Stream_Element (Digit) - 52,
         when 62 => Character'Pos ('+'),
         when 63 => Character'Pos ('/'));
      --  Printable character representation of Digit

   function Value (Symbol : in Base_64_Symbol) return Base_64_Digit
     is (case Symbol is
         when Character'Pos ('A') .. Character'Pos ('Z')
           => Base_64_Digit (Symbol - Character'Pos ('A') + 0),
         when Character'Pos ('a') .. Character'Pos ('z')
           => Base_64_Digit (Symbol - Character'Pos ('a') + 26),
         when Character'Pos ('0') .. Character'Pos ('9')
           => Base_64_Digit (Symbol - Character'Pos ('0') + 52),
         when Character'Pos ('+') => 62,
         when Character'Pos ('/') => 63);
      --  Value represented by a symbol


   function Image_Length (Input_Length : in Natural)
     return Ada.Streams.Stream_Element_Count
     is (Ada.Streams.Stream_Element_Count
           (Input_Length + (Input_Length + 2) / 3));
      --  Paddingless encoded length

   function Value_Length (Input_Length : in Ada.Streams.Stream_Element_Count)
     return Natural
     is (Natural (Input_Length) - (Natural (Input_Length) + 3) / 4);
      --  Original length of an encoded array

   function Symbol_Count (Input : Ada.Streams.Stream_Element_Array)
     return Ada.Streams.Stream_Element_Count;
      --  Return the number of valid symbols in Input


   procedure Encode
     (Input : in String;
      Output : in out Ada.Streams.Stream_Element_Array;
      Offset : in out Ada.Streams.Stream_Element_Offset);
      --  Paddingless raw encoding of Input data

   procedure Decode
     (Input : in Ada.Streams.Stream_Element_Array;
      Offset : in out Ada.Streams.Stream_Element_Offset;
      Output : out String);
      --  Paddingless raw decoding of Input data


   procedure Encode_Block
     (Input : in Full_Block;
      Output : in out Ada.Streams.Stream_Element_Array;
      Offset : in out Ada.Streams.Stream_Element_Offset);
   procedure Encode_Double
     (Input : in Double_Byte;
      Padding : in Double_Byte_Padding;
      Output : in out Ada.Streams.Stream_Element_Array;
      Offset : in out Ada.Streams.Stream_Element_Offset);
   procedure Encode_Single
     (Input : in Character;
      Padding : in Single_Byte_Padding;
      Output : in out Ada.Streams.Stream_Element_Array;
      Offset : in out Ada.Streams.Stream_Element_Offset);
      --  Encode a complete or partial block into Output at Offset

   procedure Decode_Block
     (Input : in Ada.Streams.Stream_Element_Array;
      Offset : in out Ada.Streams.Stream_Element_Offset;
      Output : out Full_Block);
   procedure Decode_Double
     (Input : in Ada.Streams.Stream_Element_Array;
      Offset : in out Ada.Streams.Stream_Element_Offset;
      Output : out Double_Byte;
      Padding : out Double_Byte_Padding);
   procedure Decode_Single
     (Input : in Ada.Streams.Stream_Element_Array;
      Offset : in out Ada.Streams.Stream_Element_Offset;
      Output : out Character;
      Padding : out Single_Byte_Padding);


   procedure Next_Digit
     (Input : in Ada.Streams.Stream_Element_Array;
      Offset : in out Ada.Streams.Stream_Element_Offset;
      Digit : out Base_64_Digit);
   procedure Next_Digit_Or_End
     (Input : in Ada.Streams.Stream_Element_Array;
      Offset : in out Ada.Streams.Stream_Element_Offset;
      Digit : out Base_64_Digit;
      Finished : out Boolean);
      --  Look for the first valid symbol in Input from Offset, and decode it

end Natools.Smaz_Implementations.Base_64_Tools;
