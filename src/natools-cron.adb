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

package body Natools.Cron is

   ------------------------
   -- Helper Subprograms --
   ------------------------

   function "<" (Left, Right : Periodic_Time) return Boolean is
      use type Ada.Calendar.Time;
   begin
      return
        Left.Origin < Right.Origin
        or else
        (Left.Origin = Right.Origin and then Left.Period < Right.Period);
   end "<";

   function "<" (Left, Right : Key_Type) return Boolean is
   begin
      return Left.Schedule < Right.Schedule;
   end "<";

   function Actual_Time (Item : in Periodic_Time) return Periodic_Time is
      use type Ada.Calendar.Time;
      Now    : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Future : Periodic_Time := Item;
   begin
      while Future.Origin < Now loop
         Future.Origin := Future.Origin + Future.Period;
      end loop;
      return Future;
   end Actual_Time;

   ----------------------
   -- Public Interface --
   ----------------------

   procedure Insert (Job      : in     Cron_Entry;
                     Schedule : in     Periodic_Time;
                     ID       :    out Entry_IDs) is
   begin
      Database.Insert (Job      => Job,
                       Schedule => Schedule,
                       ID       => ID);
   end Insert;

   procedure Reset (ID       : in     Entry_IDs;
                    Schedule : in     Periodic_Time) is
   begin
      Database.Reset (Schedule => Schedule,
                      ID       => ID);
   end Reset;

   procedure Delete (ID : in     Entry_IDs) is
   begin
      Database.Delete (ID => ID);
   end Delete;

   ------------------------
   -- Protected Database --
   ------------------------

   protected body Database is
      procedure Insert (Job      : in     Cron_Entry'Class;
                        Schedule : in     Periodic_Time;
                        ID       :    out Entry_IDs)
      is
         Next_Run : constant Periodic_Time := Actual_Time (Schedule);
      begin
         First_Changed :=
           First_Changed
           or else
           Map.Is_Empty
           or else
           Next_Run < Map.First_Key.Schedule;

         Map.Insert (Key      => (Schedule => Next_Run,
                                  ID       => Next_ID),
                     New_Item => Job);

         Next_ID := Next_ID + 1;
      end Insert;

      procedure Delete (ID : in     Entry_IDs)
      is
         use Entry_Maps;
         Cursor   : Entry_Maps.Cursor := Map.First;
         Is_First : Boolean := True;
      begin
         while Cursor /= No_Element loop
            if Entry_Maps.Key (Cursor).ID = ID then
               Map.Delete (Cursor);
               First_Changed := First_Changed or Is_First;
               exit;
            end if;

            Entry_Maps.Next (Cursor);
            Is_First := False;
         end loop;
      end Delete;

      procedure Reset (ID       : in     Entry_IDs;
                       Schedule : in     Periodic_Time) is
         use Entry_Maps;
         Cursor   : Entry_Maps.Cursor := Map.First;
         Is_First : Boolean := True;
      begin
         Locate_Job :
         loop
            exit Locate_Job when Cursor = No_Element;
            exit Locate_Job when Entry_Maps.Key (Cursor).ID = ID;
            Entry_Maps.Next (Cursor);
            Is_First := False;
         end loop Locate_Job;

         Update_Job :
         declare
            Job      : constant Cron_Entry'Class := Element (Cursor);
            Next_Run : constant Periodic_Time := Actual_Time (Schedule);
         begin
            Map.Delete (Cursor);

            First_Changed :=
              First_Changed
              or else
              Is_First
              or else
              Map.Is_Empty
              or else
              Next_Run < Map.First_Key.Schedule;

            Map.Insert (Key      => (ID       => ID,
                                     Schedule => Next_Run),
                        New_Item => Job);
         end Update_Job;
      end Reset;

      function Get_Next_Event return Ada.Calendar.Time is
         use Ada.Calendar;
      begin
         return Map.First_Key.Schedule.Origin;
      exception
         when Constraint_Error =>
            return Time_Of (Year  => Year_Number'Last,
                            Month => Month_Number'Last,
                            Day   => Day_Number'Last);
      end Get_Next_Event;

      procedure Run_Next_Event is
         use type Ada.Calendar.Time;
         use Entry_Maps;
         Key : Key_Type         := Map.First_Key;
         Job : Cron_Entry'Class := Map.First_Element;
      begin
         Job.Run;

         Key.Schedule.Origin := Key.Schedule.Origin + Key.Schedule.Period;

         Map.Delete_First;
         Map.Insert (Key      => Key,
                     New_Item => Job);
      end Run_Next_Event;

      entry Update_Notification when First_Changed is
      begin
         First_Changed := False;
      end Update_Notification;

   end Database;

   -----------------
   -- Worker Task --
   -----------------

   task body Worker is
      Time : Ada.Calendar.Time;
   begin
      Main :
      loop
         Wait_Loop :
         loop
            Time := Database.Get_Next_Event;

            select
               Database.Update_Notification;
            or
               delay until Time;
               exit Wait_Loop;
            end select;
         end loop Wait_Loop;

         Database.Run_Next_Event;
      end loop Main;
   end Worker;

end Natools.Cron;
