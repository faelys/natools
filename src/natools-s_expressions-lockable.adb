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

package body Natools.S_Expressions.Lockable is

   ----------------
   -- Lock Stack --
   ----------------

   procedure Push_Level
     (Stack : in out Lock_Stack;
      Level : in Natural;
      State : out Lock_State) is
   begin
      State := (Depth => Stack.Depth, Level => Stack.Level);
      Stack := (Depth => Stack.Depth + 1, Level => Level);
   end Push_Level;


   procedure Pop_Level
     (Stack : in out Lock_Stack;
      State : in Lock_State;
      Allow_Gap : in Boolean := False) is
   begin
      if State.Depth = 0 then
         raise Constraint_Error with "Invalid stack state";
      elsif State.Depth >= Stack.Depth then
         raise Constraint_Error with "Trying to Pop a state outside of Stack";
      elsif not Allow_Gap and then State.Depth < Stack.Depth - 1 then
         raise Constraint_Error
           with "Trying to Pop several items without Allow_Gap";
      end if;

      Stack := (Depth => State.Depth, Level => State.Level);
   end Pop_Level;


   function Current_Level (Stack : Lock_Stack) return Natural is
   begin
      return Stack.Level;
   end Current_Level;



   -------------------------------------
   -- Lockable Wrapper Implementation --
   -------------------------------------

   function Current_Event (Object : in Wrapper) return Events.Event is
   begin
      if Object.Finished then
         return Events.End_Of_Input;
      else
         return Object.Backend.Current_Event;
      end if;
   end Current_Event;


   function Current_Atom (Object : in Wrapper) return Atom is
   begin
      if Object.Finished then
         raise Program_Error with "Current_Atom on finished wrapper";
      else
         return Object.Backend.Current_Atom;
      end if;
   end Current_Atom;


   function Current_Level (Object : in Wrapper) return Natural is
   begin
      if Object.Finished then
         return 0;
      else
         return Object.Backend.Current_Level - Current_Level (Object.Stack);
      end if;
   end Current_Level;


   procedure Query_Atom
     (Object : in Wrapper;
      Process : not null access procedure (Data : in Atom)) is
   begin
      if Object.Finished then
         raise Program_Error with "Query_Atom on finished wrapper";
      else
         Object.Backend.Query_Atom (Process);
      end if;
   end Query_Atom;


   procedure Read_Atom
     (Object : in Wrapper;
      Data : out Atom;
      Length : out Count) is
   begin
      if Object.Finished then
         raise Program_Error with "Read_Atom on finished wrapper";
      else
         Object.Backend.Read_Atom (Data, Length);
      end if;
   end Read_Atom;


   procedure Next
     (Object : in out Wrapper;
      Event : out Events.Event) is
   begin
      if Object.Finished then
         Event := Events.Error;
         return;
      end if;

      Object.Backend.Next (Event);

      if Event = Events.Close_List
        and then Object.Backend.Current_Level < Current_Level (Object.Stack)
      then
         Object.Finished := True;
         Event := Events.End_Of_Input;
      end if;
   end Next;


   procedure Lock
     (Object : in out Wrapper;
      State : out Lock_State) is
   begin
      Push_Level (Object.Stack, Object.Backend.Current_Level, State);
   end Lock;


   procedure Unlock
     (Object : in out Wrapper;
      State : in out Lock_State;
      Finish : in Boolean := True)
   is
      Previous_Level : constant Natural := Current_Level (Object.Stack);
   begin
      Pop_Level (Object.Stack, State);
      State := (0, 0);

      if Finish then
         declare
            Event : Events.Event;
         begin
            Event := Object.Backend.Current_Event;
            loop
               case Event is
                  when Events.Open_List | Events.Add_Atom =>
                     null;
                  when Events.Close_List =>
                     exit when Object.Backend.Current_Level < Previous_Level;
                  when Events.Error | Events.End_Of_Input =>
                     exit;
               end case;
               Object.Backend.Next (Event);
            end loop;
         end;
      end if;

      Object.Finished := Object.Backend.Current_Level
        < Current_Level (Object.Stack);
   end Unlock;

end Natools.S_Expressions.Lockable;
