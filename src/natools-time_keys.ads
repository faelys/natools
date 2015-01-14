------------------------------------------------------------------------------
-- Copyright (c) 2015, Natacha Porté                                        --
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
-- Natools.Time_Keys provides a concise but printable representation of     --
-- time where lexicographical order matches chronological order.            --
-- It is based on a base-64 symbol set that preserve order, picked from URL --
-- unreserved character set.                                                --
-- It consists simply of time components in big-endian order, trimming      --
-- tailing zeros, and using two base-64 digits for the year, which gives a  --
-- 4096 year span.                                                          --
-- This means a second granularity can be achieved with 7 characters. The   --
-- most compact way of encoding such a timestamp would be counting seconds, --
-- like UNIX time. The time covered by this format is rought 2^37 seconds,  --
-- which would mean 5 bytes or 7 base-64 digits (though 6 would be enough   --
-- for a useful time range).                                                --
------------------------------------------------------------------------------

with Ada.Calendar;

package Natools.Time_Keys is

   function Is_Valid (Key : String) return Boolean;
      --  Check whether Key is a valid encoded time.
      --  WARNING: this function returns true for invalid dates,
      --  like February 30th.

   function To_Key
     (Time : Ada.Calendar.Time;
      Max_Sub_Second_Digits : in Natural := 120)
     return String
     with Post => Is_Valid (To_Key'Result);
      --  Convert a time into a key

   function To_Time (Key : String) return Ada.Calendar.Time
     with Pre => Is_Valid (Key);
      --  Convert a valid key into the original time

private

   subtype Base_64_Digit is Character with Static_Predicate
     => Base_64_Digit in '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' | '~';

   type Base_64_Value is mod 2 ** 6;

   Digit_Offset : constant := 48;  --  Character'Pos ('0')
   Upper_Offset : constant := 55;  --  Character'Pos ('A') - 10
   Lower_Offset : constant := 60;  --  Character'Pos ('a') - 37

   function Value (Digit : Base_64_Digit) return Base_64_Value
     is (Base_64_Value
           (case Digit is
            when '0' .. '9' => Character'Pos (Digit) - Digit_Offset,
            when 'A' .. 'Z' => Character'Pos (Digit) - Upper_Offset,
            when '_'        => 36,
            when 'a' .. 'z' => Character'Pos (Digit) - Lower_Offset,
            when '~'        => 63));

   function I_Value (Digit : Base_64_Digit) return Integer
     is (Integer (Value (Digit)));

   function Image (Digit : Base_64_Value) return Base_64_Digit
     is (case Digit is
         when  0 ..  9 => Character'Val (Natural (Digit) + Digit_Offset),
         when 10 .. 35 => Character'Val (Natural (Digit) + Upper_Offset),
         when    36    => '_',
         when 37 .. 62 => Character'Val (Natural (Digit) + Lower_Offset),
         when    63    => '~');

   function I_Image (Digit : Integer) return Base_64_Digit
     is (Image (Base_64_Value (Digit)));

   function Is_Valid (Key : String) return Boolean
     is (Key'Length >= 4
         and then Key (Key'First)
               in '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' | '~'
         and then Key (Key'First + 1)
               in '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' | '~'
         and then Key (Key'First + 2) in '1' .. '9' | 'A' .. 'C'
         and then Key (Key'First + 3) in '1' .. '9' | 'A' .. 'V'
         and then (Key'First + 4 not in Key'Range
            or else Key (Key'First + 4) in '0' .. '9' | 'A' .. 'N')
         and then (Key'First + 5 not in Key'Range or else Key (Key'First + 5)
               in '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'w')
         and then (Key'First + 6 not in Key'Range or else Key (Key'First + 6)
               in '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'x')
         and then (for all I in Key'First + 7 .. Key'Last => Key (I)
               in '0' .. '9' | 'A' .. 'Z' | '_' | 'a' .. 'z' | '~'));

end Natools.Time_Keys;
