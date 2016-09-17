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


   function Compressed_Upper_Bound
     (Dict : in Dictionary;
      Input : in String)
     return Ada.Streams.Stream_Element_Count;

   procedure Compress
     (Dict : in Dictionary;
      Input : in String;
      Output_Buffer : out Ada.Streams.Stream_Element_Array;
      Output_Last : out Ada.Streams.Stream_Element_Offset);

   function Compress (Dict : in Dictionary; Input : in String)
     return Ada.Streams.Stream_Element_Array;


   function Decompressed_Length
     (Dict : in Dictionary;
      Input : in Ada.Streams.Stream_Element_Array)
     return Natural;

   procedure Decompress
     (Dict : in Dictionary;
      Input : in Ada.Streams.Stream_Element_Array;
      Output_Buffer : out String;
      Output_Last : out Natural);

   function Decompress
     (Dict : in Dictionary; Input : in Ada.Streams.Stream_Element_Array)
     return String;

end Natools.Smaz;
