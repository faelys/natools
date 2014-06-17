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
-- Natools.Cron is a low-overhead, low-precision implementation of periodic --
-- callbacks, similar to UNIX cron daemon.                                  --
-- Note that callbacks are executed sequentially in a single thread, and    --
-- ticks may be skipped when computing resources lack.                      --
-- If you need more precision and/or more reliability, you might want to    --
-- consider using Ada.Real_Time.Timing_Events instead.                      --
------------------------------------------------------------------------------

with Ada.Calendar;

private with Ada.Containers.Indefinite_Ordered_Maps;

package Natools.Cron is

   type Entry_IDs is private;

   type Periodic_Time is
      record
         Origin : Ada.Calendar.Time;
         Period : Duration;
      end record;

   type Cron_Entry is abstract tagged null record;

   procedure Run (Job : in out Cron_Entry) is abstract;

   procedure Insert (Job      : in     Cron_Entry;
                     Schedule : in     Periodic_Time;
                     ID       :    out Entry_IDs);

   procedure Reset (ID       : in     Entry_IDs;
                    Schedule : in     Periodic_Time);

   procedure Delete (ID : in     Entry_IDs);

private

   type Entry_IDs is new Positive;

   type Key_Type is
      record
         Schedule : Periodic_Time;
         ID       : Entry_IDs;
      end record;

   function "<" (Left, Right : in Periodic_Time) return Boolean;
   function "<" (Left, Right : in Key_Type) return Boolean;

   function Actual_Time (Item : in Periodic_Time) return Periodic_Time;

   overriding
   function "=" (Left, Right : in Key_Type) return Boolean is abstract;

   package Entry_Maps is new Ada.Containers.Indefinite_Ordered_Maps
                               (Key_Type     => Key_Type,
                                Element_Type => Cron_Entry'Class);

   protected Database is
      procedure Insert (Job      : in     Cron_Entry'Class;
                        Schedule : in     Periodic_Time;
                        ID       :    out Entry_IDs);
      --  Insert Job into the database, adjusting Schedule.Origin to
      --  be in the future.

      procedure Delete (ID : in     Entry_IDs);
      --  Remove job ID from the database.

      procedure Reset (ID       : in     Entry_IDs;
                       Schedule : in     Periodic_Time);

      function Get_Next_Event return Ada.Calendar.Time;
      --  Return the time of the next scheduled event.

      procedure Run_Next_Event;
      --  Run the next active job and move it to its next execution slot.

      entry Update_Notification;
      --  Block as long as the next active item does not change

   private
      Map           : Entry_Maps.Map;
      First_Changed : Boolean := False;
      Next_ID       : Entry_IDs := 1;
   end Database;

   task Worker;

end Natools.Cron;
