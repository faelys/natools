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
-- IMPLEMENTATION NOTE:                                                     --
-- In case of synchronized callbacks (same Origin and Period), there is a   --
-- collision on the internal map key. Since it is not expected to happen    --
-- often, a simple but not-so-efficient solution is used:                   --
-- When a collision is encountered, the callback is replaced by an          --
-- Event_List callback seeded with the existing callback and the new one.   --
-- When removing a callback, if it's not found directly, and second linear  --
-- is performed, looking for Event_List objects and removing it from them.  --
------------------------------------------------------------------------------

package body Natools.Cron is

   function Create_Event_List
     (Ref_1, Ref_2 : Callback_Refs.Reference)
      return Callback_Refs.Reference;
      --  Create an Event_List object containing Ref_1 and Ref_2,
      --  and return a reference to it.



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


   function Create_Event_List
     (Ref_1, Ref_2 : Callback_Refs.Reference)
      return Callback_Refs.Reference
   is
      function Create return Callback'Class;

      function Create return Callback'Class is
         Result : Event_List;
      begin
         Result.Append (Ref_1);
         Result.Append (Ref_2);
         return Result;
      end Create;
   begin
      return Callback_Refs.Create (Create'Access);
   end Create_Event_List;



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

         declare
            Position : Entry_Maps.Cursor;
            Inserted : Boolean;
            Previous : Callback_Refs.Reference;
         begin
            Map.Insert (Actual_Time, Callback, Position, Inserted);

            if not Inserted then
               Previous := Entry_Maps.Element (Position);

               if Previous.Update.Data.all in Event_List then
                  Append
                    (Event_List (Previous.Update.Data.all),
                     Callback);
               else
                  Map.Replace_Element
                    (Position,
                     Create_Event_List (Previous, Callback));
               end if;
            end if;
         end;
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

               return;
            end if;

            Entry_Maps.Next (Cursor);
            Is_First := False;
         end loop;

         Is_First := True;
         Cursor := Map.First;
         while Entry_Maps.Has_Element (Cursor) loop
            if Entry_Maps.Element (Cursor).Update.Data.all in Event_List then
               declare
                  Mutator : constant Callback_Refs.Mutator
                    := Entry_Maps.Element (Cursor).Update;
                  List : Event_List renames Event_List (Mutator.Data.all);
                  Removed : Boolean;
               begin
                  List.Remove (Callback, Removed);

                  if Removed then
                     if List.Is_Empty then
                        Map.Delete (Cursor);

                        if Is_First then
                           First_Changed := True;
                        end if;
                     end if;

                     return;
                  end if;
               end;
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



   ----------------
   -- Event List --
   ----------------

   overriding procedure Run (Self : in out Event_List) is
   begin
      for Ref of Self.List loop
         Ref.Update.Data.Run;
      end loop;
   end Run;


   procedure Append
     (Self : in out Event_List;
      Ref : in Callback_Refs.Reference) is
   begin
      Self.List.Append (Ref);
   end Append;


   procedure Remove
     (Self : in out Event_List;
      Ref : in Callback_Refs.Reference;
      Removed : out Boolean)
   is
      use type Callback_Refs.Reference;
      Cursor : Event_Lists.Cursor := Self.List.First;
   begin
      Removed := False;

      while Event_Lists.Has_Element (Cursor) loop
         if Event_Lists.Element (Cursor) = Ref then
            Self.List.Delete (Cursor);
            Removed := True;
            return;
         end if;

         Event_Lists.Next (Cursor);
      end loop;
   end Remove;


   function Is_Empty (Self : Event_List) return Boolean is
   begin
      return Self.List.Is_Empty;
   end Is_Empty;

end Natools.Cron;
