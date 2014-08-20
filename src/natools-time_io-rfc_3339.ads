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
-- Natools.Time_IO.RFC_3339 provides subprograms to serialize and           --
-- deserialize times to and from String representations according to        --
-- RFC 3339, which is also a valid subset of ISO 8601.                      --
------------------------------------------------------------------------------

with Ada.Calendar.Time_Zones;

package Natools.Time_IO.RFC_3339 is

   Date_Separator : constant Character := '-';
   Date_Time_Separator : constant Character := 'T';
   Time_Separator : constant Character := ':';
   Subsecond_Separator : constant Character := '.';

   function Is_Valid (Image : String) return Boolean;
      --  Check whether Image is a valid RFC-3339 date

   function Image
     (Date : Ada.Calendar.Time;
      Subsecond_Digits : Natural := 0;
      Force_Leap_Second : Boolean := False)
     return String
     with Post => Is_Valid (Image'Result);
      --  Return the RFC-3339 representation of Date in current time zone

   function Image
     (Date : Ada.Calendar.Time;
      Time_Zone : Ada.Calendar.Time_Zones.Time_Offset;
      Subsecond_Digits : Natural := 0;
      Force_Leap_Second : Boolean := False)
     return String
     with Post => Is_Valid (Image'Result);
      --  Return the RFC-3339 representation of Date in Time_Zone

   function Value (Image : String) return Ada.Calendar.Time
     with Pre => Is_Valid (Image) or else raise Constraint_Error;
      --  Return the time associated with the given RFC-3339 representation

   procedure Value
     (Image : in String;
      Date : out Ada.Calendar.Time;
      Time_Zone : out Ada.Calendar.Time_Zones.Time_Offset)
     with Pre => Is_Valid (Image) or else raise Constraint_Error;
      --  Return the time associated with the given RFC-3339 representation

   procedure Value
     (Image : in String;
      Date : out Ada.Calendar.Time;
      Time_Zone : out Ada.Calendar.Time_Zones.Time_Offset;
      Leap_Second : out Boolean)
     with Pre => Is_Valid (Image) or else raise Constraint_Error;
      --  Return the time associated with the given RFC-3339 representation

end Natools.Time_IO.RFC_3339;
