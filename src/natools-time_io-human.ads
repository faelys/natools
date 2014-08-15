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
-- Natools.Time_IO.Human provides human-friendly images of time-related     --
-- types, and format-guessing parsing of human input into time-related      --
-- types.                                                                   --
------------------------------------------------------------------------------

with Ada.Calendar;

package Natools.Time_IO.Human is

   function Difference_Image
     (Left, Right : Ada.Calendar.Time;
      Use_Weeks : Boolean := False)
     return String;
      --  Return an image of the time interval from Right to Left, i.e.
      --  the amount of time represented by Left-Right if it would fit
      --  in Duration type.
      --  Use_Weeks controls whether intervals longer than 7 days are
      --  represented as a number of weeks or of days, i.e. "51d" or "7w 2d".

   function Image (Value : Duration) return String;
      --  Return an image of the given time interval

end Natools.Time_IO.Human;
