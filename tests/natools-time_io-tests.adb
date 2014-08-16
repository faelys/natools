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
with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;
with Natools.Time_IO.Human;

package body Natools.Time_IO.Tests is

   use type Ada.Calendar.Time;
   use type Ada.Calendar.Time_Zones.Time_Offset;


   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

   function Quote (Original : String) return String
     is ('"' & Original & '"');


   procedure Check is new NT.Generic_Check (String, "=", Quote);



   -------------------------
   -- Complete Test Suite --
   -------------------------

   procedure All_Tests (Report : in out NT.Reporter'Class) is
   begin
      Human_Duration (Report);
      Human_Time_Difference (Report);
   end All_Tests;



   -----------------------
   -- Inidividual Tests --
   -----------------------

   procedure Human_Duration (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Human-readable time intervals");

      function Compose
        (Second : Ada.Calendar.Formatting.Second_Number;
         Minute : Ada.Calendar.Formatting.Minute_Number := 0;
         Hour : Ada.Calendar.Formatting.Hour_Number := 0)
        return Duration
        is (Second * 1.0 + Minute * 60.0 + Hour * 3600.0);
   begin
      Check (Test, "-1d", Human.Image (-86400.0), "-1d");
      Check (Test, "0s", Human.Image (0.0), "0");
      Check (Test, "1d", Human.Image (86400.0), "1d");
      Check (Test, "1d", Human.Image (Compose (1, 30, 23)), "23h 30m 1s");
      Check (Test, "23h", Human.Image (Compose (59, 29, 23)), "23h 29m 59s");
      Check (Test, "15h", Human.Image (Compose (0, 20, 15)), "15h 20m");
      Check (Test, "10h", Human.Image (Compose (0, 0, 10)), "10h");
      Check (Test, "10h", Human.Image (Compose (31, 59, 9)), "9h 59m 31s");
      Check (Test, "9h 59m", Human.Image (Compose (29, 59, 9)), "9h 59m 29s");
      Check (Test, "2h", Human.Image (Compose (45, 59, 1)), "1h 59m 45s");
      Check (Test, "1h 2m", Human.Image (Compose (45, 1, 1)), "1h 1m 45s");
      Check (Test, "1h", Human.Image (Compose (31, 59)), "59m 31s");
      Check (Test, "59 min", Human.Image (Compose (28, 59)), "59m 28s");
      Check (Test, "10 min", Human.Image (600.1), "10m 0.1s");
      Check (Test, "10 min", Human.Image (599.7), "9m 59.7s");
      Check (Test, "9 min 59s", Human.Image (599.4), "9m 59.4s");
      Check (Test, "1 min", Human.Image (60.4), "1m 0.4s");
      Check (Test, "1 min", Human.Image (59.6), "59.6s");
      Check (Test, "59s", Human.Image (59.4), "59.4s");
      Check (Test, "10s", Human.Image (10.3), "10.3s");
      Check (Test, "6 s", Human.Image (6.0), "6s");
      Check (Test, "5.400 s", Human.Image (5.4), "5.4s");
      Check (Test, "1 s", Human.Image (1.0), "1s");
      Check (Test, "980 ms", Human.Image (0.98), "980ms");
      Check (Test, "40 ms", Human.Image (0.04), "40ms");
      Check (Test, "20 ms", Human.Image (0.02), "20ms");

      pragma Warnings (Off, "condition is always *");

      if 89999.0 in Duration then
         Check (Test, "1d 1h", Human.Image (89999.0), "1d 59m 59s");
      end if;

      --  The tests below require a smaller Duration'Small than what is
      --  guaranteed by the standard. Further conditions should be added
      --  to prevent a check from failing because of lack of Duration precision

      Check (Test, "2 s", Human.Image (2.0002), "2.0002s");
      Check (Test, "2 s", Human.Image (1.9997), "1.9997s");
      Check (Test, "1.999 s", Human.Image (1.999), "1.999s");
      Check (Test, "1 s", Human.Image (1.0), "1s");
      Check (Test, "999 ms", Human.Image (0.999), "999 ms");
      Check (Test, "2 s", Human.Image (2.000_4), "2.0004");
      Check (Test, "10 ms", Human.Image (0.0104), "10.4 ms");
      Check (Test, "9.990 ms", Human.Image (0.009_99), "9.990 ms");
      Check (Test, "1.001 ms", Human.Image (0.001_001), "1.001 ms");
      Check (Test, "1 ms", Human.Image (0.001), "1.000 ms");
      Check (Test, "999 us", Human.Image (0.000_999), "999 us");
      Check (Test, "10 us", Human.Image (0.000_01), "10 us");
      Check (Test, "9.500 us", Human.Image (0.000_009_5), "9.5 us");
      Check (Test, "1.100 us", Human.Image (0.000_001_1), "1.1 us");
      Check (Test, "1 us", Human.Image (0.000_001), "1 us");
      Check (Test, "900 ns", Human.Image (0.000_000_9), "900 ns");

      pragma Warnings (On, "condition is always *");

   exception
      when Error : others => Test.Report_Exception (Error);
   end Human_Duration;


   procedure Human_Time_Difference (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Human-readable time differences");
      Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;

      function Add
        (Base : Ada.Calendar.Time;
         Days : Ada.Calendar.Arithmetic.Day_Count;
         Seconds : Duration)
        return Ada.Calendar.Time
        is (Ada.Calendar."+"
              (Ada.Calendar.Arithmetic."+" (Base, Days), Seconds));

      function Test_Image
        (Days : Ada.Calendar.Arithmetic.Day_Count;
         Seconds : Duration;
         Use_Weeks : Boolean)
        return String
        is (Human.Difference_Image (Add (Now, Days, Seconds), Now, Use_Weeks));
   begin
      Check (Test, "-1d", Human.Difference_Image (Now, Add (Now, 1, 900.0)));
      Check (Test, "71d", Test_Image (71, 36_000.0, False));
      Check (Test, "10w", Test_Image (70, 3_600.0, True));
      Check (Test, "5w 1d", Test_Image (35, 60_000.0, True));
      Check (Test, "1w 2d", Test_Image (8, 54_000.0, True));
      Check (Test, "8d 15h", Test_Image (8, 54_000.0, False));
      Check (Test, "8d", Test_Image (7, 23 * 3600.0 + 35 * 60.0, False));
      Check (Test, "5d", Test_Image (5, 900.0, True));
      Check (Test, "10h", Test_Image (0, 36_598.0, True));
   exception
      when Error : others => Test.Report_Exception (Error);
   end Human_Time_Difference;

end Natools.Time_IO.Tests;
