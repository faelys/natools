------------------------------------------------------------------------------
-- Copyright (c) 2014-2017, Natacha Porté                                   --
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

private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Ordered_Maps;
private with Ada.Finalization;
private with Ada.Unchecked_Deallocation;
private with Natools.References;
private with Natools.Storage_Pools;

package Natools.Cron is

   type Callback is interface;

   procedure Run (Object : in out Callback) is abstract;


   type Periodic_Time is record
      Origin : Ada.Calendar.Time;
      Period : Duration;
   end record;


   type Cron_Entry is tagged limited private;
   pragma Preelaborable_Initialization (Cron_Entry);

   function Create
     (Time : in Periodic_Time;
      Callback : in Cron.Callback'Class)
     return Cron_Entry;
      --  Create a new entry with the given parameters

   function Create
     (Origin : in Ada.Calendar.Time;
      Callback : in Cron.Callback'Class)
     return Cron_Entry;
      --  Create a new entry that executes only once, at the given time

   function Create
     (Period : in Duration;
      Callback : in Cron.Callback'Class)
     return Cron_Entry;
      --  Create a new entry starting within a period from now

   procedure Set
     (Self : in out Cron_Entry;
      Time : in Periodic_Time;
      Callback : in Cron.Callback'Class);
      --  Reset an entry with the given parameters

   procedure Set
     (Self : in out Cron_Entry;
      Origin : in Ada.Calendar.Time;
      Callback : in Cron.Callback'Class);
      --  Reset entry with the given parameters, running only once

   procedure Set
     (Self : in out Cron_Entry;
      Period : in Duration;
      Callback : in Cron.Callback'Class);
      --  Reset entry with the given parameters, starting one period from now

   procedure Reset (Self : in out Cron_Entry);
      --  Clear internal state and remove associated entry from database.
      --  Note that if the callback procedure is currently running, it will
      --  continue until it returns, so the callback object may outlive
      --  the call to Reset, plan concurrency accordingly.

private

   package Callback_Refs is new References
     (Callback'Class,
      Storage_Pools.Access_In_Default_Pool'Storage_Pool,
      Storage_Pools.Access_In_Default_Pool'Storage_Pool);


   type Cron_Entry is new Ada.Finalization.Limited_Controlled with record
      Callback : Callback_Refs.Reference;
   end record;

   overriding procedure Finalize (Object : in out Cron_Entry);


   package Event_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Callback_Refs.Reference, Callback_Refs."=");

   type Event_List is new Callback with record
      List : Event_Lists.List;
   end record;

   overriding procedure Run (Self : in out Event_List);
      --  Sequentially run the contained events

   procedure Append
     (Self : in out Event_List;
      Ref : in Callback_Refs.Reference);
      --  Append Ref at the end of Self.List

   procedure Remove
     (Self : in out Event_List;
      Ref : in Callback_Refs.Reference;
      Removed : out Boolean);
      --  Remove Ref from Self.List, through a linear search

   function Is_Empty (Self : Event_List) return Boolean;
      --  Return whether Self contains any element


   function "<" (Left, Right : Periodic_Time) return Boolean;
      --  Comparison function for ordered map

   package Entry_Maps is new Ada.Containers.Ordered_Maps
     (Periodic_Time, Callback_Refs.Reference, "<", Callback_Refs."=");


   protected Database is
      procedure Insert
        (Time : in Periodic_Time;
         Callback : in Callback_Refs.Reference);
         --  Insert Callback into the database, adjusting Time.Origin
         --  to be in the future.

      procedure Remove (Callback : in Callback_Refs.Reference);
         --  Remove Callback from the database

      procedure Update (Callback : in Callback_Refs.Reference);
         --  Update Time.Origin associated with Callback so that
         --  it is in the future.

      procedure Get_First
        (Time : out Periodic_Time;
         Callback : out Callback_Refs.Reference);
         --  Return the next active callback, or an empty reference when
         --  the database is empty (to signal task termination).

      procedure Get_Event_List
        (Source : in Event_List;
         List : out Event_Lists.List);
         --  Initialize an event list from Source without
         --  any concurrent tampering of the list.

      entry Update_Notification;
         --  Block as long as the next active item does not change

   private
      Map : Entry_Maps.Map;
      First_Changed : Boolean := False;
   end Database;

   task type Worker is
   end Worker;

   type Worker_Access is access Worker;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Worker, Worker_Access);

   Global_Worker : Worker_Access := null;

end Natools.Cron;
