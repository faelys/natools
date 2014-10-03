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
-- Natools.S_Expressions.Templates.Dates provides a template interpreter    --
-- for dates and times.                                                     --
------------------------------------------------------------------------------

with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;
with Ada.Streams;
with Natools.S_Expressions.Lockable;

package Natools.S_Expressions.Templates.Dates is

   type Split_Time is record
      Source : Ada.Calendar.Time;
      Time_Zone : Ada.Calendar.Time_Zones.Time_Offset;
      Year : Ada.Calendar.Year_Number;
      Month : Ada.Calendar.Month_Number;
      Day : Ada.Calendar.Day_Number;
      Day_Of_Week : Ada.Calendar.Formatting.Day_Name;
      Hour : Ada.Calendar.Formatting.Hour_Number;
      Minute : Ada.Calendar.Formatting.Minute_Number;
      Second : Ada.Calendar.Formatting.Second_Number;
      Sub_Second : Ada.Calendar.Formatting.Second_Duration;
   end record;

   function Split
     (Value : Ada.Calendar.Time;
      Time_Zone : Ada.Calendar.Time_Zones.Time_Offset)
     return Split_Time;
      --  Pre process split time component from Value in Time_Zone

   procedure Render
     (Output : in out Ada.Streams.Root_Stream_Type'Class;
      Template : in out Lockable.Descriptor'Class;
      Value : in Split_Time);
      --  Render the given time

   procedure Render
     (Output : in out Ada.Streams.Root_Stream_Type'Class;
      Template : in out Lockable.Descriptor'Class;
      Value : in Ada.Calendar.Time);
      --  Render the given time considered in local time zone

   procedure Render
     (Output : in out Ada.Streams.Root_Stream_Type'Class;
      Template : in out Lockable.Descriptor'Class;
      Value : in Ada.Calendar.Time;
      Time_Zone : Ada.Calendar.Time_Zones.Time_Offset);
      --  Render the given time

end Natools.S_Expressions.Templates.Dates;
