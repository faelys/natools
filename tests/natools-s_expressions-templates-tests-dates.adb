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

with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;
with Natools.S_Expressions.Parsers;
with Natools.S_Expressions.Test_Tools;
with Natools.S_Expressions.Templates.Dates;
with Natools.Static_Maps.S_Expressions.Templates.Dates.T;

package body Natools.S_Expressions.Templates.Tests.Dates is

   procedure Test_Render
     (Test : in out NT.Test;
      Template : in String;
      Expected : in String;
      Value : in Ada.Calendar.Time;
      Time_Zone : Ada.Calendar.Time_Zones.Time_Offset := 0);
      --  Run Template with Value and compare the result with Expected


   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

   procedure Test_Render
     (Test : in out NT.Test;
      Template : in String;
      Expected : in String;
      Value : in Ada.Calendar.Time;
      Time_Zone : Ada.Calendar.Time_Zones.Time_Offset := 0)
   is
      Input : aliased Test_Tools.Memory_Stream;
      Output : Test_Tools.Memory_Stream;
      Parser : Parsers.Stream_Parser (Input'Access);
   begin
      Input.Set_Data (To_Atom (Template));
      Parser.Next;
      Output.Set_Expected (To_Atom (Expected));
      Templates.Dates.Render (Output, Parser, Value, Time_Zone);
      Output.Check_Stream (Test);
   end Test_Render;



   -------------------------
   -- Complete Test Suite --
   -------------------------

   procedure All_Tests (Report : in out NT.Reporter'Class) is
   begin
      Composite_Components (Report);
      Forced_Time_Zone (Report);
      Padded_Components (Report);
      RFC_3339 (Report);
      Simple_Components (Report);
      Static_Hash_Map (Report);
   end All_Tests;


   -----------------------
   -- Inidividual Tests --
   -----------------------

   procedure Composite_Components (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Composite date and time formats");
   begin
      Test_Render
        (Test, "(YYYYMMDD)T(HHMMSS)",
         "20111125T082052",
         Ada.Calendar.Formatting.Time_Of (2011, 11, 25, 8, 20, 52));
      Test_Render
        (Test, "(big-endian-date ""."")1: (time "":"")",
         "2013.10.01 18:49:11",
         Ada.Calendar.Formatting.Time_Of (2013, 10, 1, 18, 49, 11));
      Test_Render
        (Test, "(little-endian-date ""/"")1: (time h m)1:s",
         "11/10/2013 21h51m08s",
         Ada.Calendar.Formatting.Time_Of (2013, 10, 11, 21, 51, 8));
      Test_Render
        (Test, "(big-endian-date 1: 1: )2:, (little-endian-time 1:;)",
         "2013 10 13, 00;47;15",
         Ada.Calendar.Formatting.Time_Of (2013, 10, 13, 15, 47, 0));
      Test_Render
        (Test, "(SSMMHH)(DDMMYYYY)",
         "01082129012014",
         Ada.Calendar.Formatting.Time_Of (2014, 1, 29, 21, 8, 1));
   exception
      when Error : others => Test.Report_Exception (Error);
   end Composite_Components;


   procedure Forced_Time_Zone (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Time zone set in template");
      Moment : constant Ada.Calendar.Time := Ada.Calendar.Formatting.Time_Of
        (2012, 8, 29, 22, 42, 16, Time_Zone => 0);
   begin
      Test_Render
        (Test, "(in-zone 0 (rfc-3339))",
         "2012-08-29T22:42:16Z", Moment);
      Test_Render
        (Test, "(in-zone CEST (rfc-3339))",
         "2012-08-30T00:42:16+02:00", Moment);
      Test_Render
        (Test, "(in-zone -01:12 (rfc-3339))",
         "2012-08-29T21:30:16-01:12", Moment);
      Test_Render
        (Test, "(in-zone +0130 (rfc-3339))",
         "2012-08-30T00:12:16+01:30", Moment);
      Test_Render
        (Test, "(in-zone -10 (rfc-3339))",
         "2012-08-29T12:42:16-10:00", Moment);
      Test_Render
        (Test, "(in-zone FOO (rfc-3339))",
         "", Moment);
      Test_Render
        (Test, "(in-zone BARST (rfc-3339))",
         "", Moment);
      Test_Render
        (Test, "(in-zone 12345 (rfc-3339))",
         "", Moment);
   exception
      when Error : others => Test.Report_Exception (Error);
   end Forced_Time_Zone;


   procedure Padded_Components (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Simple time components with padding");
      Template : constant String
        := "(padded-month)(padded-day)(padded-hour)(minute)(padded-second)";
   begin
      Test_Render
        (Test, Template,
         "1125082052",
         Ada.Calendar.Formatting.Time_Of (2011, 11, 25, 8, 20, 52));
      Test_Render
        (Test, Template,
         "1001184911",
         Ada.Calendar.Formatting.Time_Of (2013, 10, 1, 18, 49, 11));
      Test_Render
        (Test, Template,
         "1011215108",
         Ada.Calendar.Formatting.Time_Of (2013, 10, 11, 21, 51, 8));
      Test_Render
        (Test, Template,
         "1013154700",
         Ada.Calendar.Formatting.Time_Of (2013, 10, 13, 15, 47, 0));
      Test_Render
        (Test, Template,
         "012921801",
         Ada.Calendar.Formatting.Time_Of (2014, 1, 29, 21, 8, 1));
   exception
      when Error : others => Test.Report_Exception (Error);
   end Padded_Components;


   procedure RFC_3339 (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("RFC-3339 format");
   begin
      Test_Render
        (Test, "(rfc-3339)",
         "2011-11-25T08:20:52Z",
         Ada.Calendar.Formatting.Time_Of (2011, 11, 25, 8, 20, 52));
      Test_Render
        (Test, "(rfc-3339)",
         "2013-10-01T18:49:11Z",
         Ada.Calendar.Formatting.Time_Of (2013, 10, 1, 18, 49, 11));
      Test_Render
        (Test, "(rfc-3339)",
         "2013-10-11T21:51:08Z",
         Ada.Calendar.Formatting.Time_Of (2013, 10, 11, 21, 51, 8));
      Test_Render
        (Test, "(rfc-3339)",
         "2013-10-13T15:47:00Z",
         Ada.Calendar.Formatting.Time_Of (2013, 10, 13, 15, 47, 0));
      Test_Render
        (Test, "(rfc-3339)",
         "2014-01-29T21:08:01Z",
         Ada.Calendar.Formatting.Time_Of (2014, 1, 29, 21, 8, 1));
   exception
      when Error : others => Test.Report_Exception (Error);
   end RFC_3339;


   procedure Simple_Components (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Simple time components");
      Template : constant String
        := "3:Le "
         & "(day-of-week lundi mardi mercredi jeudi vendredi samedi dimanche)"
         & "1: "
         & "(day (suffix (er 1)))"
         & "1: "
         & "(month janvier février mars avril mai juin juillet"
         & " août septembre octobre novembre décembre)"
         & "1: (year)"
         & """ à """
         & "(hour) 1:h (padded-minute)"
         & "4: et (second)"
         & "1: (second (image-range secondes (seconde 1 0)))";
   begin
      Test_Render
        (Test, Template,
         "Le vendredi 25 novembre 2011 à 8h20 et 52 secondes",
         Ada.Calendar.Formatting.Time_Of (2011, 11, 25, 8, 20, 52));
      Test_Render
        (Test, Template,
         "Le mardi 1er octobre 2013 à 18h49 et 11 secondes",
         Ada.Calendar.Formatting.Time_Of (2013, 10, 1, 18, 49, 11));
      Test_Render
        (Test, Template,
         "Le vendredi 11 octobre 2013 à 21h51 et 8 secondes",
         Ada.Calendar.Formatting.Time_Of (2013, 10, 11, 21, 51, 8));
      Test_Render
        (Test, Template,
         "Le dimanche 13 octobre 2013 à 15h47 et 0 seconde",
         Ada.Calendar.Formatting.Time_Of (2013, 10, 13, 15, 47, 0));
      Test_Render
        (Test, Template,
         "Le mercredi 29 janvier 2014 à 21h08 et 1 seconde",
         Ada.Calendar.Formatting.Time_Of (2014, 1, 29, 21, 8, 1));
   exception
      when Error : others => Test.Report_Exception (Error);
   end Simple_Components;


   procedure Static_Hash_Map (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Parse errors in template");
   begin
      if not Natools.Static_Maps.S_Expressions.Templates.Dates.T then
         Test.Fail;
      end if;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Static_Hash_Map;

end Natools.S_Expressions.Templates.Tests.Dates;
