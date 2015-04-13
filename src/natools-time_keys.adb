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

with Ada.Calendar.Arithmetic;

package body Natools.Time_Keys is

   function Extract_Sub_Second (Key : String) return Duration;
      --  Read the end of Buffer and compute the Sub_Second part


   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

   function Extract_Sub_Second (Key : String) return Duration is
      Sub_Second : Duration := 0.0;
   begin
      for I in reverse Key'First + 7 .. Key'Last loop
         Sub_Second := (Sub_Second + Duration (Value (Key (I)))) / 32;
         Sub_Second := (Sub_Second + Duration'Small) / 2;
      end loop;

      return Sub_Second;
   end Extract_Sub_Second;



   -----------------------
   -- Publoic Interface --
   -----------------------

   function To_Key
     (Time : Ada.Calendar.Time;
      Max_Sub_Second_Digits : in Natural := 120)
     return String
   is
      Year : Ada.Calendar.Year_Number;
      Month : Ada.Calendar.Month_Number;
      Day : Ada.Calendar.Day_Number;
      Hour : Ada.Calendar.Formatting.Hour_Number;
      Minute : Ada.Calendar.Formatting.Minute_Number;
      Second : Ada.Calendar.Formatting.Second_Number;
      Sub_Second : Ada.Calendar.Formatting.Second_Duration;
      Leap_Second : Boolean;
   begin
      Ada.Calendar.Formatting.Split
        (Time,
         Year, Month, Day, Hour, Minute, Second, Sub_Second,
         Leap_Second);
      return To_Key
        (Year, Month, Day,
         Hour, Minute, Second, Sub_Second,
         Leap_Second,
         Max_Sub_Second_Digits);
   end To_Key;


   function To_Key
     (Year : Ada.Calendar.Year_Number;
      Month : Ada.Calendar.Month_Number;
      Day : Ada.Calendar.Day_Number;
      Hour : Ada.Calendar.Formatting.Hour_Number := 0;
      Minute : Ada.Calendar.Formatting.Minute_Number := 0;
      Second : Ada.Calendar.Formatting.Second_Number := 0;
      Sub_Second : Ada.Calendar.Formatting.Second_Duration := 0.0;
      Leap_Second : Boolean := False;
      Max_Sub_Second_Digits : Natural := 120)
     return String
   is
      procedure Increment_Buffer;

      Buffer : String (1 .. 7 + Max_Sub_Second_Digits);
      Last : Positive;

      procedure Increment_Buffer is
      begin
         while Last > 7 and then Buffer (Last) = '~' loop
            Last := Last - 1;
         end loop;

         if Last > 7 then
            Buffer (Last) := Image (Value (Buffer (Last)) + 1);
            return;
         end if;

         if Second <= 58 then
            Buffer (7) := I_Image (Second + 1);
            Last := 7;

         elsif Minute <= 58 then
            Buffer (6) := I_Image (Minute + 1);
            Last := 6;

         elsif Hour <= 22 then
            Buffer (5) := I_Image (Hour + 1);
            Last := 5;

         else
            Buffer (1 .. 4) := To_Key (Ada.Calendar.Arithmetic."+"
              (Ada.Calendar.Formatting.Time_Of (Year, Month, Day), 1));
            Last := 4;
         end if;
      end Increment_Buffer;

      N : Natural;
      D, Base : Duration;
   begin
      Buffer (1) := I_Image (Year / 64);
      Buffer (2) := I_Image (Year mod 64);
      Buffer (3) := I_Image (Month);
      Buffer (4) := I_Image (Day);
      Buffer (5) := I_Image (Hour);
      Buffer (6) := I_Image (Minute);

      if Leap_Second then
         pragma Assert (Second = 59);
         Buffer (7) := I_Image (60);
      else
         Buffer (7) := I_Image (Second);
      end if;

      if Sub_Second = 0.0 then
         if Second = 0 then
            if Minute = 0 then
               if Hour = 0 then
                  return Buffer (1 .. 4);
               else
                  return Buffer (1 .. 5);
               end if;
            else
               return Buffer (1 .. 6);
            end if;
         else
            return Buffer (1 .. 7);
         end if;
      end if;

      Last := 7;
      D := Sub_Second * 64;
      Base := 1.0;
      loop
         Last := Last + 1;
         Base := Base / 64.0;
         N := Natural (D);

         if Last = Buffer'Last or Base = 0.0 then
            if N < 64 then
               Buffer (Last) := I_Image (N);
            else
               Last := Last - 1;
               Increment_Buffer;
            end if;
            exit;
         end if;

         if Duration (N) > D then
            N := N - 1;
            pragma Assert (Duration (N) <= D);
         end if;

         D := (D - Duration (N)) * 64;
         Buffer (Last) := I_Image (N);

         exit when D = 0.0;
      end loop;

      return Buffer (1 .. Last);
   end To_Key;


   function To_Time (Key : String) return Ada.Calendar.Time is
      Leap_Second : constant Boolean
        := Key'First + 6 in Key'Range and then Key (Key'First + 6) = 'x';
   begin
      return Ada.Calendar.Formatting.Time_Of
        (Year        => I_Value (Key (Key'First)) * 64
                      + I_Value (Key (Key'First + 1)),
         Month       => I_Value (Key (Key'First + 2)),
         Day         => I_Value (Key (Key'First + 3)),
         Hour        => (if Key'First + 4 in Key'Range
                         then I_Value (Key (Key'First + 4)) else 0),
         Minute      => (if Key'First + 5 in Key'Range
                         then I_Value (Key (Key'First + 5)) else 0),
         Second      => (if Key'First + 6 in Key'Range
                         then (if Leap_Second then 59
                               else I_Value (Key (Key'First + 6)))
                         else 0),
         Sub_Second  => Extract_Sub_Second (Key),
         Leap_Second => Leap_Second);
   end To_Time;

end Natools.Time_Keys;
