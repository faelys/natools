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
-- Natools.Smaz_Implementations.Base_256 provides the subprograms needed to --
-- instantiate Natools.Smaz_Generic into the original Smaz compression      --
-- algorithm, with byte-based output stream.                                --
------------------------------------------------------------------------------

with Ada.Streams;

package Natools.Smaz_Implementations.Base_256 is
   pragma Pure;

   procedure Read_Code
     (Input : in Ada.Streams.Stream_Element_Array;
      Offset : in out Ada.Streams.Stream_Element_Offset;
      Code : out Ada.Streams.Stream_Element;
      Verbatim_Length : out Natural;
      Last_Code : in Ada.Streams.Stream_Element;
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
      Last_Code : in Ada.Streams.Stream_Element;
      Variable_Length_Verbatim : in Boolean)
     return Ada.Streams.Stream_Element_Count;

   procedure Write_Code
     (Output : in out Ada.Streams.Stream_Element_Array;
      Offset : in out Ada.Streams.Stream_Element_Offset;
      Code : in Ada.Streams.Stream_Element);

   procedure Write_Verbatim
     (Output : in out Ada.Streams.Stream_Element_Array;
      Offset : in out Ada.Streams.Stream_Element_Offset;
      Input : in String;
      Last_Code : in Ada.Streams.Stream_Element;
      Variable_Length_Verbatim : in Boolean);

end Natools.Smaz_Implementations.Base_256;
