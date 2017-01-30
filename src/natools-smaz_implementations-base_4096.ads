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
-- Natools.Smaz_Implementations.Base_4096 provides the subprograms needed   --
-- to instantiate Natools.Smaz_Generic into a variant of the Smaz           --
-- compression algorithm that output directly base-64 printable symbols,    --
-- but with a dictionary indexed by two symbols, allowing a maximum size of --
-- 4095 entries.                                                            --
------------------------------------------------------------------------------

with Ada.Streams;

package Natools.Smaz_Implementations.Base_4096 is
   pragma Pure;

   type Base_4096_Digit is range 0 .. 4095;

   procedure Read_Code
     (Input : in Ada.Streams.Stream_Element_Array;
      Offset : in out Ada.Streams.Stream_Element_Offset;
      Code : out Base_4096_Digit;
      Verbatim_Length : out Natural;
      Last_Code : in Base_4096_Digit;
      Variable_Length_Verbatim : in Boolean);

   procedure Read_Verbatim
     (Input : in Ada.Streams.Stream_Element_Array;
      Offset : in out Ada.Streams.Stream_Element_Offset;
      Output : out String);

   procedure Skip_Verbatim
     (Input : in Ada.Streams.Stream_Element_Array;
      Offset : in out Ada.Streams.Stream_Element_Offset;
      Verbatim_Length : in Positive);

   function Verbatim_Size
     (Input_Length : in Positive;
      Last_Code : in Base_4096_Digit;
      Variable_Length_Verbatim : in Boolean)
     return Ada.Streams.Stream_Element_Count;

   procedure Write_Code
     (Output : in out Ada.Streams.Stream_Element_Array;
      Offset : in out Ada.Streams.Stream_Element_Offset;
      Code : in Base_4096_Digit);

   procedure Write_Verbatim
     (Output : in out Ada.Streams.Stream_Element_Array;
      Offset : in out Ada.Streams.Stream_Element_Offset;
      Input : in String;
      Last_Code : in Base_4096_Digit;
      Variable_Length_Verbatim : in Boolean);

end Natools.Smaz_Implementations.Base_4096;
