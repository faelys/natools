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
      return Left.Origin < Right.Origin
        or else (Left.Origin = Right.Origin
           and then Left.Period < Right.Period);
   end "<";



   ----------------------
   -- Public Interface --
   ----------------------

   function Create
     (Time : in Periodic_Time;
      Callback : in Cron.Callback'Class)
     return Cron_Entry is
   begin
      return Result : Cron_Entry do
         Result.Set (Time, Callback);
      end return;
   end Create;


   function Create
     (Period : in Duration;
      Callback : in Cron.Callback'Class)
     return Cron_Entry is
   begin
      return Result : Cron_Entry do
         Result.Set (Period, Callback);
      end return;
   end Create;


   procedure Set
     (Self : in out Cron_Entry;
      Time : in Periodic_Time;
      Callback : in Cron.Callback'Class)
   is
      function Create return Cron.Callback'Class;

      function Create return Cron.Callback'Class is
      begin
         return Callback;
      end Create;
   begin
      Self.Reset;
      Self.Callback.Replace (Create'Access);
      Database.Insert (Time, Self.Callback);
   end Set;


   procedure Set
     (Self : in out Cron_Entry;
      Period : in Duration;
      Callback : in Cron.Callback'Class) is
   begin
      Set (Self, (Ada.Calendar.Clock, Period), Callback);
   end Set;


   overriding procedure Finalize (Object : in out Cron_Entry) is
   begin
      if not Object.Callback.Is_Empty then
         Object.Reset;
      end if;
   end Finalize;


   procedure Reset (Self : in out Cron_Entry) is
   begin
      if not Self.Callback.Is_Empty then
         Database.Remove (Self.Callback);
         Self.Callback.Reset;
      end if;
   end Reset;



   ------------------------
   -- Protected Database --
   ------------------------

   protected body Database is
      procedure Insert
        (Time : in Periodic_Time;
         Callback : in Callback_Refs.Reference)
      is
         use type Ada.Calendar.Time;

         Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
         Actual_Time : Periodic_Time := Time;
      begin
         while Actual_Time.Origin < Now loop
            Actual_Time.Origin := Actual_Time.Origin + Actual_Time.Period;
         end loop;

         if Map.Is_Empty then
            if Global_Worker /= null and then Global_Worker.all'Terminated then
               Unchecked_Free (Global_Worker);
            end if;

            if Global_Worker = null then
               Global_Worker := new Worker;
            end if;
         else
            if Actual_Time < Map.First_Key then
               First_Changed := True;
            end if;
         end if;

         Map.Insert (Actual_Time, Callback);
      end Insert;


      procedure Remove (Callback : in Callback_Refs.Reference) is
         use type Callback_Refs.Reference;

         Cursor : Entry_Maps.Cursor := Map.First;
         Is_First : Boolean := True;
      begin
         while Entry_Maps.Has_Element (Cursor) loop
            if Entry_Maps.Element (Cursor) = Callback then
               Map.Delete (Cursor);

               if Is_First then
                  First_Changed := True;
               end if;

               exit;
            end if;

            Entry_Maps.Next (Cursor);
            Is_First := False;
         end loop;
      end Remove;


      procedure Update (Callback : in Callback_Refs.Reference) is
         use type Callback_Refs.Reference;
         Cursor : Entry_Maps.Cursor := Map.First;
      begin
         Search :
         while Entry_Maps.Has_Element (Cursor) loop
            if Entry_Maps.Element (Cursor) = Callback then
               declare
                  Old_Time : constant Periodic_Time := Entry_Maps.Key (Cursor);
               begin
                  Map.Delete (Cursor);
                  Insert (Old_Time, Callback);
               end;

               exit Search;
            end if;

            Entry_Maps.Next (Cursor);
         end loop Search;
      end Update;


      procedure Get_First
        (Time : out Periodic_Time;
         Callback : out Callback_Refs.Reference)
      is
         Cursor : constant Entry_Maps.Cursor := Map.First;
      begin
         if Entry_Maps.Has_Element (Cursor) then
            Time := Entry_Maps.Key (Cursor);
            Callback := Entry_Maps.Element (Cursor);
         else
            Callback := Callback_Refs.Null_Reference;
         end if;

         First_Changed := False;
      end Get_First;


      entry Update_Notification when First_Changed is
      begin
         null;
      end Update_Notification;

   end Database;



   -----------------
   -- Worker Task --
   -----------------

   task body Worker is
      Time : Periodic_Time;
      Callback : Callback_Refs.Reference;
      Waiting : Boolean;
   begin
      Main :
      loop
         Waiting := True;

         Wait_Loop :
         while Waiting loop
            Database.Get_First (Time, Callback);
            exit Main when Callback.Is_Empty;

            select
               Database.Update_Notification;
            or
               delay until Time.Origin;
               Waiting := False;
            end select;
         end loop Wait_Loop;

         Callback.Update.Data.Run;
         Database.Update (Callback);
      end loop Main;
   end Worker;

end Natools.Cron;
