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
-- Natools.Time_IO provides subprograms to serialize and deserialize times  --
-- to and from various String representations.                              --
------------------------------------------------------------------------------

with Ada.Calendar.Formatting;

package body Natools.Time_IO.RFC_3339 is

   ---------------------
   -- Validity Checks --
   ---------------------

   function Is_Valid_Prefix (Image : String) return Boolean
     is (Image (Image'First) in Digit_Character
        and then Image (Image'First + 1) in Digit_Character
        and then Image (Image'First + 2) in Digit_Character
        and then Image (Image'First + 3) in Digit_Character
        and then Image (Image'First + 4) = Date_Separator
        and then Image (Image'First + 5) in Digit_Character
        and then Image (Image'First + 6) in Digit_Character
        and then Image (Image'First + 7) = Date_Separator
        and then Image (Image'First + 8) in Digit_Character
        and then Image (Image'First + 9) in Digit_Character
        and then Image (Image'First + 10) = Date_Time_Separator
        and then Image (Image'First + 11) in Digit_Character
        and then Image (Image'First + 12) in Digit_Character
        and then Image (Image'First + 13) = Time_Separator
        and then Image (Image'First + 14) in Digit_Character
        and then Image (Image'First + 15) in Digit_Character
        and then Image (Image'First + 16) = Time_Separator
        and then Image (Image'First + 17) in Digit_Character
        and then Image (Image'First + 18) in Digit_Character);

   function Is_Valid_Time_Zone (Image : String) return Boolean
     is (Image (Image'Last - 5) in '+' | '-'
        and then Image (Image'Last - 4) in Digit_Character
        and then Image (Image'Last - 3) in Digit_Character
        and then Image (Image'Last - 2) = Time_Separator
        and then Image (Image'Last - 1) in Digit_Character
        and then Image (Image'Last) in Digit_Character);

   function Is_Valid_Subsecond (Sub_Image : String) return Boolean
     is (Sub_Image'Length = 0
        or else (Sub_Image'Length >= 2
           and then Sub_Image (Sub_Image'First) = Subsecond_Separator
           and then (for all I in Sub_Image'First + 1 .. Sub_Image'Last
              => Sub_Image (I) in Digit_Character)));

   function Is_Valid (Image : String) return Boolean is
   begin
      return Image'Length >= 20
        and then Is_Valid_Prefix (Image)
        and then ((Image (Image'Last) = 'Z'
              and then Is_Valid_Subsecond
                    (Image (Image'First + 19 .. Image'Last - 1)))
           or else (Is_Valid_Time_Zone (Image)
              and then Is_Valid_Subsecond
                    (Image (Image'First + 19 .. Image'Last - 6))));
   end Is_Valid;



   --------------------
   -- Time To String --
   --------------------

   function Image
     (Date : Ada.Calendar.Time;
      Subsecond_Digits : Natural := 0)
     return String is
   begin
      return Image
        (Date,
         Ada.Calendar.Time_Zones.UTC_Time_Offset (Date),
         Subsecond_Digits);
   end Image;


   function Image
     (Date : Ada.Calendar.Time;
      Time_Zone : Ada.Calendar.Time_Zones.Time_Offset;
      Subsecond_Digits : Natural := 0)
     return String
   is
      function Subsecond_Image
        (Subsecond : Ada.Calendar.Formatting.Second_Duration)
        return String;

      function Time_Zone_Image return String;

      function Subsecond_Image
        (Subsecond : Ada.Calendar.Formatting.Second_Duration)
        return String
      is
         Remaining : Duration := Subsecond;
         Number : Digit_Number;
         N : Natural;
      begin
         if Subsecond_Digits = 0  then
            return "";
         end if;

         return Result : String (1 .. Subsecond_Digits + 1) do
            Result (1) := Subsecond_Separator;
            for I in 2 .. Subsecond_Digits + 1 loop
               Remaining := Remaining * 10;
               N := Natural (Remaining);
               if Duration (N) > Remaining then
                  Number := N - 1;
               else
                  Number := N;
               end if;
               Remaining := Remaining - Duration (Number);
               Result (I) := Image (Number);
            end loop;
         end return;
      end Subsecond_Image;

      function Time_Zone_Image return String is
         use type Ada.Calendar.Time_Zones.Time_Offset;
      begin
         if Time_Zone = 0 then
            return "Z";
         else
            declare
               Hour : constant Ada.Calendar.Time_Zones.Time_Offset
                 := (abs Time_Zone) / 60;
               Minute : constant Ada.Calendar.Time_Zones.Time_Offset
                 := (abs Time_Zone) mod 60;
               Sign : Character;
            begin
               if Time_Zone < 0 then
                  Sign := '-';
               else
                  Sign := '+';
               end if;

               return String'(Sign,
                 Image (Digit_Number (Hour / 10)),
                 Image (Digit_Number (Hour mod 10)),
                 Time_Separator,
                 Image (Digit_Number (Minute / 10)),
                 Image (Digit_Number (Minute mod 10)));
            end;
         end if;
      end Time_Zone_Image;

      Year : Ada.Calendar.Year_Number;
      Month : Ada.Calendar.Month_Number;
      Day : Ada.Calendar.Day_Number;
      Hour : Ada.Calendar.Formatting.Hour_Number;
      Minute : Ada.Calendar.Formatting.Minute_Number;
      Second : Ada.Calendar.Formatting.Second_Number;
      Subsecond : Ada.Calendar.Formatting.Second_Duration;
      Leap_Second : Boolean;
      Used_Second : Natural;
   begin
      Ada.Calendar.Formatting.Split
        (Date,
         Year, Month, Day,
         Hour, Minute, Second,
         Subsecond,
         Leap_Second,
         Time_Zone);

      if Leap_Second then
         pragma Assert (Second = 59);
         Used_Second := 60;
      else
         Used_Second := Second;
      end if;

      return
        (Image (Year / 1000),
         Image ((Year / 100) mod 10),
         Image ((Year / 10) mod 10),
         Image (Year mod 10),
         Date_Separator,
         Image (Month / 10),
         Image (Month mod 10),
         Date_Separator,
         Image (Day / 10),
         Image (Day mod 10),
         Date_Time_Separator,
         Image (Hour / 10),
         Image (Hour mod 10),
         Time_Separator,
         Image (Minute / 10),
         Image (Minute mod 10),
         Time_Separator,
         Image (Used_Second / 10),
         Image (Used_Second mod 10))
        & Subsecond_Image (Subsecond)
        & Time_Zone_Image;
   end Image;



   --------------------
   -- String To Time --
   --------------------

   function Value (Image : String) return Ada.Calendar.Time is
      Result : Ada.Calendar.Time;
      Discarded : Ada.Calendar.Time_Zones.Time_Offset;
   begin
      Value (Image, Result, Discarded);
      return Result;
   end Value;


   procedure Value
     (Image : in String;
      Date : out Ada.Calendar.Time;
      Time_Zone : out Ada.Calendar.Time_Zones.Time_Offset)
   is
      Year : Ada.Calendar.Year_Number;
      Month : Ada.Calendar.Month_Number;
      Day : Ada.Calendar.Day_Number;
      Hour : Ada.Calendar.Formatting.Hour_Number;
      Minute : Ada.Calendar.Formatting.Minute_Number;
      Second : Ada.Calendar.Formatting.Second_Number;
      Subsecond : Ada.Calendar.Formatting.Second_Duration := 0.0;
      Leap_Second : Boolean;
   begin
      Year := Natural'Value (Image (Image'First .. Image'First + 3));
      Month := Natural'Value (Image (Image'First + 5 .. Image'First + 6));
      Day := Natural'Value (Image (Image'First + 8 .. Image'First + 9));
      Hour := Natural'Value (Image (Image'First + 11 .. Image'First + 12));
      Minute := Natural'Value
        (Image (Image'First + 14 .. Image'First + 15));

      declare
         Number : constant Natural
           := Natural'Value (Image (Image'First + 17 .. Image'First + 18));
      begin
         if Number = 60 then
            Leap_Second := True;
            Second := 59;
         else
            Second := Number;
         end if;
      end;

      if Image (Image'First + 19) = Subsecond_Separator then
         declare
            I : Positive := Image'First + 20;
            Current : Duration := 0.1;
         begin
            while Image (I) in Digit_Character loop
               Subsecond := Subsecond + Current
                 * (Character'Pos (Image (I)) - Character'Pos ('0'));
               Current := Current / 10;
               I := I + 1;
            end loop;
         end;
      end if;

      if Image (Image'Last) = 'Z' then
         Time_Zone := 0;
      else
         Time_Zone := Ada.Calendar.Time_Zones.Time_Offset
           (Natural'Value (Image (Image'Last - 4 .. Image'Last - 3)) * 60
            + Natural'Value (Image (Image'Last - 1 .. Image'Last)));
         case Image (Image'Last - 5) is
            when '-' =>
               Time_Zone := Ada.Calendar.Time_Zones."-" (Time_Zone);
            when '+' =>
               null;
            when others =>
               raise Constraint_Error
                 with "Invalid time zone separator in RFC 3339 date";
         end case;
      end if;

      Date := Ada.Calendar.Formatting.Time_Of
        (Year, Month, Day,
         Hour, Minute, Second,
         Subsecond, Leap_Second, Time_Zone);
   end Value;

end Natools.Time_IO.RFC_3339;
