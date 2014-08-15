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

with Ada.Calendar.Arithmetic;

package body Natools.Time_IO.Human is

   ---------------------
   -- Duration Images --
   ---------------------

   function Difference_Image
     (Left, Right : Ada.Calendar.Time;
      Use_Weeks : Boolean := False)
     return String
   is
      use type Ada.Calendar.Arithmetic.Day_Count;

      Days, Rounded_Days : Ada.Calendar.Arithmetic.Day_Count;
      Seconds : Duration;
      Leap_Seconds : Ada.Calendar.Arithmetic.Leap_Seconds_Count;
   begin
      if Ada.Calendar."<" (Left, Right) then
         return '-' & Difference_Image
           (Left => Right,
            Right => Left,
            Use_Weeks => Use_Weeks);
      end if;

      Ada.Calendar.Arithmetic.Difference
        (Left, Right,
         Days, Seconds, Leap_Seconds);

      Seconds := Seconds - 86400.0 + Duration (Leap_Seconds);
      if Seconds >= 0.0 then
         Days := Days + 1;
      else
         Seconds := Seconds + 86400.0;
      end if;

      if Seconds >= 43200.0 then
         Rounded_Days := Days + 1;
      else
         Rounded_Days := Days;
      end if;

      if Use_Weeks and then Rounded_Days >= 7 then
         declare
            Weeks : constant Ada.Calendar.Arithmetic.Day_Count
              := Rounded_Days / 7;
         begin
            Rounded_Days := Rounded_Days - Weeks * 7;
            if Weeks >= 10 or Rounded_Days = 0 then
               return Trim_Image
                 (Ada.Calendar.Arithmetic.Day_Count'Image (Weeks)) & 'w';
            else
               return Trim_Image
                 (Ada.Calendar.Arithmetic.Day_Count'Image (Weeks)) & 'w'
                 & Ada.Calendar.Arithmetic.Day_Count'Image (Rounded_Days)
                 & 'd';
            end if;
         end;

      elsif Rounded_Days >= 10 then
         return Trim_Image
           (Ada.Calendar.Arithmetic.Day_Count'Image (Rounded_Days)) & 'd';

      elsif Days > 0 then
         declare
            Hours : constant Natural := Natural (Seconds / 3600);
         begin
            case Hours is
               when 0 =>
                  return Trim_Image
                    (Ada.Calendar.Arithmetic.Day_Count'Image (Days)) & 'd';
               when 1 .. 23 =>
                  return Trim_Image
                    (Ada.Calendar.Arithmetic.Day_Count'Image (Days)) & 'd'
                    & Natural'Image (Hours) & 'h';
               when 24 =>
                  return Trim_Image
                    (Ada.Calendar.Arithmetic.Day_Count'Image (Days + 1)) & 'd';
               when others =>
                  raise Program_Error;
            end case;
         end;

      else
         return Image (Seconds);
      end if;
   end Difference_Image;


   function Image (Value : Duration) return String is
      function Local_Image
        (Mul_1, Div : Positive;
         Unit_1 : String;
         Mul_2 : Positive;
         Unit_2 : String)
        return String;

      function Scientific_Image (Mul : Positive; Unit : String) return String;


      function Local_Image
        (Mul_1, Div : Positive;
         Unit_1 : String;
         Mul_2 : Positive;
         Unit_2 : String)
        return String
      is
         Scaled : constant Duration := Value * Mul_1 / Div;
         Main : constant Natural := Natural (Scaled - 0.5);
         Secondary : constant Natural
           := Natural ((Scaled - Duration (Main)) * Mul_2);
      begin
         pragma Assert (Secondary <= Mul_2);

         if Secondary = Mul_2 then
            return Trim_Image (Natural'Image (Main + 1)) & Unit_1;

         elsif Secondary = 0 then
            return Trim_Image (Natural'Image (Main)) & Unit_1;

         else
            return Trim_Image (Natural'Image (Main)) & Unit_1
              & Natural'Image (Secondary) & Unit_2;
         end if;
      end Local_Image;

      function Scientific_Image (Mul : Positive; Unit : String)
        return String
      is
         Scaled : constant Duration := Value * Mul;
         I_Part : constant Natural := Natural (Scaled - 0.5);
         F_Part : constant Natural
           := Natural ((Scaled - Duration (I_Part)) * 1000);
      begin
         if F_Part = 0 then
            return Trim_Image (Natural'Image (I_Part)) & Unit;
         elsif F_Part = 1000 then
            return Trim_Image (Natural'Image (I_Part + 1)) & Unit;
         else
            return Trim_Image (Natural'Image (I_Part))
              & ('.',
               Image (F_Part / 100),
               Image ((F_Part / 10) mod 10),
               Image (F_Part mod 10))
              & Unit;
         end if;
      end Scientific_Image;
   begin
      if Value < 0.0 then
         return '-' & Image (-Value);

      elsif Value = 0.0 then
         return "0s";

      elsif Value >= 86400.0 - 1800.0 then
         return Local_Image (1, 86400, "d", 24, "h");

      elsif Value >= 36000.0 then
         return Trim_Image (Positive'Image (Positive (Value / 3600))) & 'h';

      elsif Value >= 3600.0 - 30.0 then
         return Local_Image (1, 3600, "h", 60, "m");

      elsif Value >= 600.0 then
         return Trim_Image (Positive'Image (Positive (Value / 60))) & " min";

      elsif Value >= 60.0 - 0.5 then
         return Local_Image (1, 60, " min", 60, "s");

      elsif Value >= 10.0 then
         return Trim_Image (Positive'Image (Positive (Value))) & 's';

      elsif Value >= 1.0 then
         return Scientific_Image (1, " s");

      elsif Value >= 0.01 then
         return Trim_Image (Positive'Image (Positive (Value * 1000))) & " ms";

      elsif Value >= 0.001 then
         return Scientific_Image (1_000, " ms");

      elsif Value >= 0.000_01 then
         return Trim_Image
           (Positive'Image (Positive (Value * 1_000_000))) & " us";

      elsif Value >= 0.000_001 then
         return Scientific_Image (1_000_000, " us");

      else
         return Scientific_Image (1_000_000_000, " ns");
      end if;
   end Image;

end Natools.Time_IO.Human;

