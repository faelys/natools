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

------------------------------------------------------------------------------
-- Natools.Smaz is a re-implementation of the short string compression      --
-- algorithm "Smaz" by Salvatore Sanfilippo                                 --
-- (see https://github.com/antirez/smaz).                                   --
-- Its main selling point is its simplicity and CPU performance. However    --
-- the implementation here emphasizes correctness (which greatly benefits   --
-- from simplicity) over performance (so no benchmarks have been made).     --
--                                                                          --
-- The basic idea behind the algorithm is that bytes in the encoded (and    --
-- hopefully compressed) message are indexes in a static compiled-in        --
-- dictionary, and two special byte values to mark verbatim data.           --
--                                                                          --
-- For example, using original Smaz dictionary, the string "Athe33" is      --
-- encoded as (254, 65, 1, 255, 1, 51, 51), which can be broken down as:    --
--   * 254 to mark the following byte as verbatim                           --
--   * 65 which is verbatim byte for 'A'                                    --
--   * 1 to mark the second word in the dictionary: "the"                   --
--   * 255 to mark variable-length verbatim escape                          --
--   * 1 to encoding the length of the verbatim fragment: 2 bytes           --
--   * 51, 51 the verbatim bytes for "33".                                  --
--                                                                          --
-- Note that the encoder has been improved over the original Smaz encoder,  --
-- in that it merges adjacent verbatim fragments when it makes the output   --
-- smaller. For example, with the input 5-byte string "33 33", the original --
-- naive encoder would produce the 9-byte output                            --
-- (255, 1, 51, 51, 0, 255, 1, 51, 51), while encoder here would encode the --
-- whole string in a single verbatim fragment, leading to the 7-byte output --
-- (255, 4, 51, 51, 32, 51, 51).                                            --
------------------------------------------------------------------------------

with Ada.Streams;

package Natools.Smaz is
   pragma Pure (Natools.Smaz);

   use type Ada.Streams.Stream_Element;

   type Offset_Array is
     array (Ada.Streams.Stream_Element range <>) of Positive;

   type Dictionary
     (Dict_Last : Ada.Streams.Stream_Element;
      String_Size : Natural)
   is record
      Variable_Length_Verbatim : Boolean;
      Max_Word_Length : Positive;
      Offsets : Offset_Array (0 .. Dict_Last);
      Values : String (1 .. String_Size);
      Hash : not null access function (Value : String) return Natural;
   end record with
      Dynamic_Predicate => (for all I in Dictionary.Offsets'Range
         => Dictionary.Offsets (I) in Dictionary.Values'Range
            and then ((if I = Dictionary.Offsets'Last
                        then Dictionary.Values'Last + 1
                        else Dictionary.Offsets (I + 1))
                      - Dictionary.Offsets (I)
                  in 1 .. Dictionary.Max_Word_Length));

   function Dict_Entry
     (Dict : in Dictionary;
      Index : in Ada.Streams.Stream_Element)
     return String
     with Pre => Index <= Dict.Dict_Last;
      --  Return the string for at the given Index in Dict


   function Compressed_Upper_Bound
     (Dict : in Dictionary;
      Input : in String)
     return Ada.Streams.Stream_Element_Count;
      --  Return the maximum number of bytes needed to encode Input

   procedure Compress
     (Dict : in Dictionary;
      Input : in String;
      Output_Buffer : out Ada.Streams.Stream_Element_Array;
      Output_Last : out Ada.Streams.Stream_Element_Offset);
      --  Encode Input into Output_Buffer

   function Compress (Dict : in Dictionary; Input : in String)
     return Ada.Streams.Stream_Element_Array;
      --  Return an encoded buffer for Input


   function Decompressed_Length
     (Dict : in Dictionary;
      Input : in Ada.Streams.Stream_Element_Array)
     return Natural;
      --  Return the exact length when Input is decoded

   procedure Decompress
     (Dict : in Dictionary;
      Input : in Ada.Streams.Stream_Element_Array;
      Output_Buffer : out String;
      Output_Last : out Natural);
      --  Decode Input into Output_Buffer

   function Decompress
     (Dict : in Dictionary; Input : in Ada.Streams.Stream_Element_Array)
     return String;
      --  Return a decoded buffer for Input

end Natools.Smaz;
