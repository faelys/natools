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

generic
   type Dictionary_Code is (<>);

   with procedure Read_Code
     (Input : in Ada.Streams.Stream_Element_Array;
      Offset : in out Ada.Streams.Stream_Element_Offset;
      Code : out Dictionary_Code;
      Verbatim_Length : out Natural;
      Last_Code : in Dictionary_Code;
      Variable_Length_Verbatim : in Boolean);

   with procedure Read_Verbatim
     (Input : in Ada.Streams.Stream_Element_Array;
      Offset : in out Ada.Streams.Stream_Element_Offset;
      Output : out String);

   with procedure Skip_Verbatim
     (Input : in Ada.Streams.Stream_Element_Array;
      Offset : in out Ada.Streams.Stream_Element_Offset;
      Verbatim_Length : in Positive);

   with function Verbatim_Size
     (Input_Length : in Positive;
      Last_Code : in Dictionary_Code;
      Variable_Length_Verbatim : in Boolean)
     return Ada.Streams.Stream_Element_Count;

   with procedure Write_Code
     (Output : in out Ada.Streams.Stream_Element_Array;
      Offset : in out Ada.Streams.Stream_Element_Offset;
      Code : in Dictionary_Code);

   with procedure Write_Verbatim
     (Output : in out Ada.Streams.Stream_Element_Array;
      Offset : in out Ada.Streams.Stream_Element_Offset;
      Input : in String;
      Last_Code : in Dictionary_Code;
      Variable_Length_Verbatim : in Boolean);

package Natools.Smaz_Generic is
   pragma Pure;

   type Offset_Array is array (Dictionary_Code range <>) of Positive;

   type Dictionary
     (Last_Code : Dictionary_Code;
      Values_Last : Natural)
   is record
      Variable_Length_Verbatim : Boolean;
      Max_Word_Length : Positive;
      Offsets : Offset_Array
        (Dictionary_Code'Succ (Dictionary_Code'First) .. Last_Code);
      Values : String (1 .. Values_Last);
      Hash : not null access function (Value : String) return Natural;
   end record;


   function Code_First
     (Offsets : in Offset_Array;
      Code : in Dictionary_Code;
      Fallback : in Positive)
     return Positive
     is (if Code in Offsets'Range then Offsets (Code) else Fallback);
      --  Return the first index of the value for Code in Dictionary.Values

   function Code_Last
     (Offsets : in Offset_Array;
      Code : in Dictionary_Code;
      Fallback : in Natural)
     return Natural
     is (if Dictionary_Code'Succ (Code) in Offsets'Range
         then Offsets (Dictionary_Code'Succ (Code)) - 1
         else Fallback);
      --  Return the value index of the value for Code in Dictionary.Values

   function Is_Valid_Code
     (Dict : in Dictionary;
      Code : in Dictionary_Code)
     return Boolean
     is (Code in Dictionary_Code'First .. Dict.Last_Code);
      --  Return whether Code exists in Dict

   function Dict_Entry
     (Dict : in Dictionary;
      Code : in Dictionary_Code)
     return String
     is (Dict.Values (Code_First (Dict.Offsets, Code, Dict.Values'First)
                    .. Code_Last (Dict.Offsets, Code, Dict.Values'Last)))
     with Pre => Is_Valid_Code (Dict, Code);
      --  Return the string for at the given Index in Dict

   function Dict_Entry_Length
     (Dict : in Dictionary;
      Code : in Dictionary_Code)
     return Positive
     is (1 + Code_Last (Dict.Offsets, Code, Dict.Values'Last)
           - Code_First (Dict.Offsets, Code, Dict.Values'First))
     with Pre => Is_Valid_Code (Dict, Code);
      --  Return the length of the string for at the given Index in Dict


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

end Natools.Smaz_Generic;
