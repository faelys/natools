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
with Natools.Time_IO.RFC_3339;

package body Natools.Time_IO.Tests is

   use type Ada.Calendar.Time;
   use type Ada.Calendar.Time_Zones.Time_Offset;

   type Extended_Time is record
      Time : Ada.Calendar.Time;
      Offset : Ada.Calendar.Time_Zones.Time_Offset;
   end record;


   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

   function Explicit_Sign (Image : String) return String
     is (if Image'Length > 0 and then Image (Image'First) = ' '
         then '+' & Image (Image'First + 1 .. Image'Last)
         else Image);

   function Has_Leap_Second_Support return Boolean;

   function Image (Time : Extended_Time) return String
     is ('[' & Ada.Calendar.Formatting.Image (Time.Time) & "] "
      & Explicit_Sign
         (Ada.Calendar.Time_Zones.Time_Offset'Image (Time.Offset)));

   function Quote (Original : String) return String
     is ('"' & Original & '"');


   procedure Check is new NT.Generic_Check (Extended_Time);

   procedure Check is new NT.Generic_Check (String, "=", Quote);


   function Has_Leap_Second_Support return Boolean is
      Leap_Second_Time : Ada.Calendar.Time;
      Year : Ada.Calendar.Year_Number;
      Month : Ada.Calendar.Month_Number;
      Day : Ada.Calendar.Day_Number;
      Hour : Ada.Calendar.Formatting.Hour_Number;
      Minute : Ada.Calendar.Formatting.Minute_Number;
      Second : Ada.Calendar.Formatting.Second_Number;
      Sub_Second : Ada.Calendar.Formatting.Second_Duration;
      Is_Leap_Second : Boolean;
   begin
      begin
         Leap_Second_Time := Ada.Calendar.Formatting.Time_Of
           (1990, 12, 31, 23, 59, 59, 0.25, True, 0);
      exception
         when Ada.Calendar.Time_Error =>
            --  Leap second are explicitly not supported
            return False;
      end;

      Ada.Calendar.Formatting.Split
        (Leap_Second_Time,
         Year, Month, Day,
         Hour, Minute, Second, Sub_Second,
         Is_Leap_Second,
         Time_Zone => 0);

      --  Check that Time_Of/Split at least work on the normal part

      pragma Assert (Year = 1990);
      pragma Assert (Month = 12);
      pragma Assert (Day = 31);
      pragma Assert (Hour = 23);
      pragma Assert (Minute = 59);
      pragma Assert (Second = 59);
      pragma Assert (Sub_Second = 0.25);

      --  According to the standard, Is_Leap_Second should be True at this
      --  point, because Time_Error should have been raised if leap second is
      --  not supported.
      --  However some implementations mistakenly drop silently Leap_Second,
      --  so actual support is determined here by check Is_Leap_Second.

      return Is_Leap_Second;
   end Has_Leap_Second_Support;



   -------------------------
   -- Complete Test Suite --
   -------------------------

   procedure All_Tests (Report : in out NT.Reporter'Class) is
   begin
      Human_Duration (Report);
      Human_Time_Difference (Report);
      Read_From_RFC_3339 (Report);
      Write_As_RFC_3339 (Report);
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


   procedure Read_From_RFC_3339 (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("RFC-3339 -> Ada.Calendar.Time");
      Now : constant Extended_Time
        := (Ada.Calendar.Clock, Ada.Calendar.Time_Zones.UTC_Time_Offset);

      function Value
        (Img : String;
         Expected_Leap : Boolean := False)
        return Extended_Time;

      function Value
        (Img : String;
         Expected_Leap : Boolean := False)
        return Extended_Time
      is
         Result : Extended_Time;
         Leap : Boolean;
      begin
         RFC_3339.Value (Img, Result.Time, Result.Offset, Leap);
         if Leap /= Expected_Leap then
            Test.Fail ("Unexpected leap second flag at "
              & Boolean'Image (Leap)
              & " for """ & Img & '"');
         end if;
         return Result;
      end Value;
   begin
      Check (Test,
         (Ada.Calendar.Formatting.Time_Of
           (1985, 04, 12, 23, 20, 50, 0.52, False, 0), 0),
         Value ("1985-04-12T23:20:50.52Z"),
         "[1] UTC time with subseconds:");

      Check (Test,
         (Ada.Calendar.Formatting.Time_Of
           (1996, 12, 19, 16, 39, 57, 0.0, False, -8 * 60), -8 * 60),
         Value ("1996-12-19T16:39:57-08:00"),
         "[2] Time with negative offset:");

      if Has_Leap_Second_Support then
         Check (Test,
            (Ada.Calendar.Formatting.Time_Of
              (1990, 12, 31, 23, 59, 59, 0.0, True, 0), 0),
            Value ("1990-12-31T23:59:60Z"),
            "[3] UTC leap second:");

         Check (Test,
            (Ada.Calendar.Formatting.Time_Of
              (1990, 12, 31, 15, 59, 59, 0.0, True, -8 * 60), -8 * 60),
            Value ("1990-12-31T15:59:60-08:00"),
            "[4] Leap second with time offset:");
      else
         Check (Test,
            (Ada.Calendar.Formatting.Time_Of
              (1990, 12, 31, 23, 59, 59, 0.0, False, 0), 0),
            Value ("1990-12-31T23:59:60Z", True),
            "[3] UTC leap second:");

         Check (Test,
            (Ada.Calendar.Formatting.Time_Of
              (1990, 12, 31, 15, 59, 59, 0.0, False, -8 * 60), -8 * 60),
            Value ("1990-12-31T15:59:60-08:00", True),
            "[4] Leap second with time offset:");
      end if;

      Check (Test,
         (Ada.Calendar.Formatting.Time_Of
           (1937, 01, 01, 12, 0, 27, 0.87, False, 20), 20),
         Value ("1937-01-01T12:00:27.87+00:20"),
         "[5] Noon in the Netherlands:");

      Check (Test, Now,
         Value (RFC_3339.Image (Now.Time, Subsecond_Digits => 9)),
         "[6] Round trip with current time:");

      declare
         Time : Extended_Time;
      begin
         RFC_3339.Value ("1990-11-31T23:59:60Z", Time.Time, Time.Offset);
         Test.Fail ("No exception on 1990-11-31, found " & Image (Time));
      exception
         when Ada.Calendar.Time_Error =>
            null;
      end;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Read_From_RFC_3339;


   procedure Write_As_RFC_3339 (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Ada.Calendar.Time -> RFC-3339");
   begin
      Check (Test,
         "1985-04-12T23:20:50.52Z",
         RFC_3339.Image
           (Ada.Calendar.Formatting.Time_Of
              (1985, 04, 12, 23, 20, 50, 0.52, False, 0),
            0, 2),
         "[1] UTC time with subseconds:");

      Check (Test,
         "1996-12-19T16:39:57-08:00",
         RFC_3339.Image
           (Ada.Calendar.Formatting.Time_Of
              (1996, 12, 19, 16, 39, 57, 0.0, False, -8 * 60),
            -8 * 60, 0),
         "[2] Time with negative offset:");

      if Has_Leap_Second_Support then
         Check (Test,
            "1990-12-31T23:59:60Z",
            RFC_3339.Image
              (Ada.Calendar.Formatting.Time_Of
                 (1990, 12, 31, 23, 59, 59, 0.0, True, 0),
               0, 0),
            "[3] UTC leap second:");

         Check (Test,
            "1990-12-31T15:59:60-08:00",
            RFC_3339.Image
              (Ada.Calendar.Formatting.Time_Of
                 (1990, 12, 31, 15, 59, 59, 0.0, True, -8 * 60),
               -8 * 60, 0),
            "[4] Leap second with time offset:");
      end if;

      Check (Test,
         "1990-12-31T23:59:60Z",
         RFC_3339.Image
           (Ada.Calendar.Formatting.Time_Of
              (1990, 12, 31, 23, 59, 59, 0.0, False, 0),
            0, 0, True),
         "[3b] UTC leap second with workaround:");

      Check (Test,
         "1990-12-31T15:59:60-08:00",
         RFC_3339.Image
           (Ada.Calendar.Formatting.Time_Of
              (1990, 12, 31, 15, 59, 59, 0.0, False, -8 * 60),
            -8 * 60, 0, True),
         "[4b] Leap second with time offset and workaround:");

      Check (Test,
         "1937-01-01T12:00:27.87+00:20",
         RFC_3339.Image
           (Ada.Calendar.Formatting.Time_Of
              (1937, 01, 01, 12, 0, 27, 0.87, False, 20),
            20, 2),
         "[5] Noon in the Netherlands:");

      Check (Test,
         "2014-12-25T23:00:00+01:00",
         RFC_3339.Image
           (RFC_3339.Value ("2014-12-25T23:00:00+01:00"),
            60, 0),
         "[6] Round trip");
   exception
      when Error : others => Test.Report_Exception (Error);
   end Write_As_RFC_3339;

end Natools.Time_IO.Tests;
