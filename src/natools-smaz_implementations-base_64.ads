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
-- Natools.Smaz_Implementations.Base_64 provides the subprograms needed to  --
-- instantiate Natools.Smaz_Generic into a variant of the Smaz compression  --
-- algorithm that output directly base-64 printable symbols, but with a     --
-- dictionary containing at most 61 elements.                               --
--                                                                          --
-- Similarly to original Smaz, low-numbered base-64 digit are indices in    --
-- the static dictionary, while high-numbered ones are verbatim headers.    --
-- The verbatim headers all indicate a number of bytes in the decoded       --
-- stream, and it is encoded without padding characters in the output (e.g. --
-- a two-byte verbatim sequence would be encoded as only three base-64      --
-- symbols).                                                                --
--                                                                          --
-- When Variable_Length_Verbatim is True, the same scheme as original Smaz  --
-- is used: 62 means one verbatim byte (encoded in two base-64 digits), 61  --
-- means two verbatim bytes, and so on, while 63 is followed by the number  --
-- of bytes on top of the hardcoded ones. For example, with a 60-entry      --
-- dictionary, 59 means the last dictionary entry, and 60 means 3-byte      --
-- verbatim string, and 63, 0 means 4-byte verbatim string.                 --
--                                                                          --
-- When Variable_Length_Verbatim is False, another variable-length scheme   --
-- is used, where the number of extra blocks is stored in the padding bits. --
--  * 111111 nnnnnn ... means (n+1) 3-byte blocks of verbatim data,         --
--  * 111110 AAAAAA AAnnnn ... means n 3-byte blocks and 1 byte (A),        --
--  * 111101 AAAAAA AABBBB BBBBnn ... means n 3-byte blocks and 2 bytes     --
-- If the dictionary is smaller, the extra codes are used for further 3n+2  --
-- blocks. For example, 60 would then mean 3(n+4)+2 bytes of verbatim data. --
------------------------------------------------------------------------------

with Ada.Streams;
with Natools.Smaz_Implementations.Base_64_Tools;

package Natools.Smaz_Implementations.Base_64 is
   pragma Pure;

   procedure Read_Code
     (Input : in Ada.Streams.Stream_Element_Array;
      Offset : in out Ada.Streams.Stream_Element_Offset;
      Code : out Natools.Smaz_Implementations.Base_64_Tools.Base_64_Digit;
      Verbatim_Length : out Natural;
      Last_Code : in Natools.Smaz_Implementations.Base_64_Tools.Base_64_Digit;
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
      Last_Code : in Natools.Smaz_Implementations.Base_64_Tools.Base_64_Digit;
      Variable_Length_Verbatim : in Boolean)
     return Ada.Streams.Stream_Element_Count;

   procedure Write_Code
     (Output : in out Ada.Streams.Stream_Element_Array;
      Offset : in out Ada.Streams.Stream_Element_Offset;
      Code : in Natools.Smaz_Implementations.Base_64_Tools.Base_64_Digit);

   procedure Write_Verbatim
     (Output : in out Ada.Streams.Stream_Element_Array;
      Offset : in out Ada.Streams.Stream_Element_Offset;
      Input : in String;
      Last_Code : in Natools.Smaz_Implementations.Base_64_Tools.Base_64_Digit;
      Variable_Length_Verbatim : in Boolean);

end Natools.Smaz_Implementations.Base_64;
