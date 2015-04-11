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

with Ada.Calendar.Formatting;
with Natools.Time_IO.RFC_3339;

package body Natools.Time_Keys.Tests is

   function Image (Date : Ada.Calendar.Time) return String;

   procedure Key_Test
     (Test : in out NT.Test;
      Time : in Ada.Calendar.Time;
      Expected_Key : in String;
      Max_Sub_Second_Digits : in Natural);

   procedure Roundtrip_Test
     (Test : in out NT.Test;
      Time : in Ada.Calendar.Time;
      Expected_Key : in String);


   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

   function Image (Date : Ada.Calendar.Time) return String is
   begin
      return Time_IO.RFC_3339.Image
        (Date => Date,
         Subsecond_Digits => Duration'Aft);
   end Image;


   procedure Key_Test
     (Test : in out NT.Test;
      Time : in Ada.Calendar.Time;
      Expected_Key : in String;
      Max_Sub_Second_Digits : in Natural)
   is
      Generated_Key : constant String := To_Key (Time, Max_Sub_Second_Digits);
   begin
      if Generated_Key /= Expected_Key then
         Test.Fail ("Generated key """ & Generated_Key
           & """, expected """ & Expected_Key & '"');
         Test.Info ("Time of generated key: "
           & Image (To_Time (Generated_Key)));
      end if;
   end Key_Test;


   procedure Roundtrip_Test
     (Test : in out NT.Test;
      Time : in Ada.Calendar.Time;
      Expected_Key : in String)
   is
      use type Ada.Calendar.Time;

      Generated_Key : constant String := To_Key (Time, 2);
      Recovered_Time : constant Ada.Calendar.Time := To_Time (Generated_Key);
   begin
      if Generated_Key /= Expected_Key then
         Test.Fail ("Generated key """ & Generated_Key
           & """, expected """ & Expected_Key & '"');
      end if;

      if Recovered_Time /= Time then
         Test.Fail ("Roundtrip time: " & Image (Recovered_Time)
           & ", original: " & Image (Time));
      end if;
   end Roundtrip_Test;



   -------------------------
   -- Complete Test Suite --
   -------------------------

   procedure All_Tests (Report : in out NT.Reporter'Class) is
   begin
      Roundtrips (Report);
      Subsecond_Rounding (Report);
   end All_Tests;



   ----------------------
   -- Individual Tests --
   ----------------------

   procedure Roundtrips (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Conversion Roundtrips");
   begin
      if Duration'Small <= 1.0 / 128.0 then
         Roundtrip_Test
           (Test,
            Ada.Calendar.Formatting.Time_Of (2015,  1, 14, 15, 16, 17,
              0.5 + 1.0 / 128.0),
            "VV1EFGHWW");
      end if;

      Roundtrip_Test
        (Test,
         Ada.Calendar.Formatting.Time_Of (2015,  1,  2,  3,  4,  5, 0.5),
         "VV12345W");
      Roundtrip_Test
        (Test,
         Ada.Calendar.Formatting.Time_Of (2047,  1, 14,  8, 44, 36),
         "V~1E8h_");
      Roundtrip_Test
        (Test,
         Ada.Calendar.Formatting.Time_Of (2020, 10,  9,  0,  9,  0),
         "V_A909");
      Roundtrip_Test
        (Test,
         Ada.Calendar.Formatting.Time_Of (2303,  9, 30, 23,  0,  0),
         "Z~9UN");
      Roundtrip_Test
        (Test,
         Ada.Calendar.Formatting.Time_Of (2304, 12, 31,  0,  0,  0),
         "_0CV");
   exception
      when Error : others => Test.Report_Exception (Error);
   end Roundtrips;


   procedure Subsecond_Rounding (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Overflow in subsecond rounding");
   begin
      if Duration'Small > 1.0 / 256.0 then
         Test.Skip ("Not enough precision in Duration");
         return;
      end if;

      Key_Test
        (Test,
         To_Time ("VV121231~V"),
         "VV121231~",
         2);

      Key_Test
        (Test,
         To_Time ("VV121231~X"),
         "VV121232",
         2);

      Key_Test
        (Test,
         To_Time ("VV124561~~V"),
         "VV124561~~",
         3);

      Key_Test
        (Test,
         To_Time ("VV124561~~X"),
         "VV124562",
         3);

      Key_Test
        (Test,
         Ada.Calendar.Formatting.Time_Of
           (2015, 2, 2,  1,  1,  1, 255.0 / 256.0),
         "VV22112",
         1);

      Key_Test
        (Test,
         Ada.Calendar.Formatting.Time_Of
           (2015, 2, 2,  1,  58, 59, 255.0 / 256.0),
         "VV221w",
         1);

      Key_Test
        (Test,
         Ada.Calendar.Formatting.Time_Of
           (2015, 2, 2,  22,  59, 59, 255.0 / 256.0),
         "VV22N",
         1);

      Key_Test
        (Test,
         Ada.Calendar.Formatting.Time_Of
           (2015, 2, 28, 23, 59, 59, 255.0 / 256.0),
         "VV31",
         1);

      Key_Test
        (Test,
         Ada.Calendar.Formatting.Time_Of
           (2016, 2, 28, 23, 59, 59, 255.0 / 256.0),
         "VW2T",
         1);

      Key_Test
        (Test,
         Ada.Calendar.Formatting.Time_Of
           (2015, 12, 31, 23, 59, 59, 255.0 / 256.0),
         "VW11",
         1);
   exception
      when Error : others => Test.Report_Exception (Error);
   end Subsecond_Rounding;

end Natools.Time_Keys.Tests;
