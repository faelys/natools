------------------------------------------------------------------------------
-- Copyright (c) 2014, Natacha Porté                                        --
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
-- Natools.Time_IO is a minimal parent for children packages which provide  --
-- subprograms to serialize and deserialize times to and from various       --
-- String representations.                                                  --
------------------------------------------------------------------------------

package Natools.Time_IO is
   pragma Pure;

private

   subtype Digit_Character is Character range '0' .. '9';

   subtype Digit_Number is Integer range 0 .. 9;

   function Image (N : Digit_Number) return Digit_Character
     is (Character'Val (N + Character'Pos (Digit_Character'First)));

   function Value (C : Digit_Character) return Digit_Number
     is (Character'Pos (C) - Character'Pos (Digit_Character'First));

   function Trim_Image (Raw_Image : String) return String
     is (if Raw_Image'Length > 0 and then Raw_Image (Raw_Image'First) = ' '
         then Raw_Image (Raw_Image'First + 1 .. Raw_Image'Last)
         else Raw_Image);

end Natools.Time_IO;
