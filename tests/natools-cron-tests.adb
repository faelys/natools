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

package body Natools.Cron.Tests is

   --------------------
   -- Test Callbacks --
   --------------------

   overriding procedure Run (Self : in out Test_Callback) is
   begin
      Append (Self.Backend.all, Self.Symbol);
   end Run;


   overriding procedure Run (Self : in out Long_Callback) is
   begin
      Append (Self.Backend.all, Self.Open);
      delay Self.Wait;
      Append (Self.Backend.all, Self.Close);
   end Run;



   --------------------
   -- Bounded String --
   --------------------

   procedure Append (S : in out Bounded_String; C : Character) is
   begin
      S.Size := S.Size + 1;
      S.Data (S.Size) := C;
   end Append;


   function Get (S : Bounded_String) return String is
   begin
      return S.Data (1 .. S.Size);
   end Get;


   procedure Reset (S : in out Bounded_String) is
   begin
      S.Size := 0;
   end Reset;


   procedure Check
     (Test : in out NT.Test;
      Found : in Bounded_String;
      Expected : in String;
      Context : in String := "") is
   begin
      if Get (Found) /= Expected then
         if Context /= "" then
            Test.Fail (Context
              & ": found """ & Get (Found) & """, expected """
              & Expected & '"');
         else
            Test.Fail ("Found """ & Get (Found) & """, expected """
              & Expected & '"');
         end if;
      end if;
   end Check;



   -------------------------
   -- Complete Test Suite --
   -------------------------

   procedure All_Tests (Report : in out NT.Reporter'Class) is
   begin
      Basic_Usage (Report);
      Delete_While_Busy (Report);
      Insert_While_Busy (Report);
      Time_Collision (Report);
   end All_Tests;



   -----------------------
   -- Inidividual Tests --
   -----------------------

   procedure Basic_Usage (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Basic black-box usage");
      Total : constant Duration := 1.0;
      Tick : constant Duration := Total / 10;
      Half_Tick : constant Duration := Tick / 2;
      Log : aliased Bounded_String (256);
   begin
      declare
         Beat : constant Cron_Entry := Create
           (Tick, Test_Callback'(Backend => Log'Access, Symbol => '.'));
         pragma Unreferenced (Beat);

         Test_Entry : Cron_Entry;
      begin
         delay Half_Tick;
         Test_Entry.Set
           (Tick, Test_Callback'(Backend => Log'Access, Symbol => '1'));
         delay 3 * Tick + Half_Tick;
         Test_Entry.Reset;
         delay Half_Tick;
      end;

      Append (Log, '|');
      delay Tick / 10;

      declare
         use type Ada.Calendar.Time;

         Beat : constant Cron_Entry := Create
           ((Origin => Ada.Calendar.Clock + Half_Tick,
             Period => Tick),
            Test_Callback'(Backend => Log'Access, Symbol => '.'));
         pragma Unreferenced (Beat);

         Slow, Fast : Cron_Entry;
      begin
         Slow.Set
           (2 * Tick,
            Test_Callback'(Backend => Log'Access, Symbol => 's'));
         delay 2 * Tick;
         Fast.Set
           (Tick / 5,
            Test_Callback'(Backend => Log'Access, Symbol => 'f'));
         delay Tick + Half_Tick;
         Fast.Reset;
         delay Tick + Half_Tick;
      end;

      --  Timeline, in ticks:
      --  Beat: set at 0.0, finalized at 4.5, run at 1.0, 2.0, 3.0, 4.0.
      --  Test_Entry: set at 0.5, reset at 4.0, run at 1.5, 2.5, 3.5.
      --  Beat: set at 4.5, finalized at 9.5, run at 5.0, 6.0, 7.0, 8.0, 9.0.
      --  Slow: set at 4.5, finalized at 9.5, run at 6.5, 8.5.
      --  Fast: set at 6.5, reset at 8.0,
      --        run at 6.7, 6.9, 7.1, 7.3, 7.5, 7.7, 7.9

      Check (Test, Log, ".1.1.1.|..sff.fffff.s.");
   exception
      when Error : others => Test.Report_Exception (Error);
   end Basic_Usage;


   procedure Delete_While_Busy (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Delete entry while callback is running");
      Total : constant Duration := 0.01;
      Log : aliased Bounded_String (256);
   begin
      declare
         Test_Entry : Cron_Entry;
      begin
         Test_Entry.Set (Total / 8, Long_Callback'
           (Backend => Log'Access,
            Open => '(',
            Close => ')',
            Wait => Total / 4));
         delay Total / 4;
      end;

      Check (Test, Log, "(", "Before wait");
      delay Total / 2;
      Check (Test, Log, "()", "After wait");
   exception
      when Error : others => Test.Report_Exception (Error);
   end Delete_While_Busy;


   procedure Insert_While_Busy (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Insert entry while callback is running");
      Total : constant Duration := 0.1;
      Log : aliased Bounded_String (256);
   begin
      declare
         Long, Short : Cron_Entry;
      begin
         Long.Set
           (Total / 8,
            Long_Callback'
              (Backend => Log'Access,
               Open => '(',
               Close => ')',
               Wait => Total / 5));

         delay Total / 8 + Total / 16;

         Short.Set
           (Total / 8,
            Test_Callback'(Backend => Log'Access, Symbol => '.'));

         delay Total / 2 + Total / 8;
      end;

      --  Timeline: 0 . 1/8 . 1/4 .   3/8 . 1/2 .   5/8 . 3/4 . 7/8 . 1
      --  Set:      L       S
      --  Finalize:                                        *
      --  Ticks:         L     L  S    L  S  L  S    L  S  L
      --  Run:           <----L---->S  <----L---->S  <----L---->

      delay Total / 8;
      Check (Test, Log, "().().()");
   exception
      when Error : others => Test.Report_Exception (Error);
   end Insert_While_Busy;


   procedure Time_Collision (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Simultaneous activation of events");
      Total : constant Duration := 0.01;
      Tick : constant Duration := Total / 4;
      Log : aliased Bounded_String (256);
   begin
      declare
         use type Ada.Calendar.Time;
         Common : constant Periodic_Time := (Ada.Calendar.Clock + Tick, Tick);
         First, Second, Third : Cron_Entry;
      begin
         First.Set
           (Common, Test_Callback'(Backend => Log'Access, Symbol => '1'));
         Second.Set
           (Common, Test_Callback'(Backend => Log'Access, Symbol => '2'));
         Third.Set
           ((Origin => Common.Origin, Period => 2 * Common.Period),
            Test_Callback'(Backend => Log'Access, Symbol => '3'));
         delay Total - Tick / 2;
      end;

      Check (Test, Log, "12312123");
   exception
      when Error : others => Test.Report_Exception (Error);
   end Time_Collision;

end Natools.Cron.Tests;
