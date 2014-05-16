------------------------------------------------------------------------------
-- Copyright (c) 2013-2014, Natacha Porté                                   --
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

package body Natools.S_Expressions.Interpreters is

   -----------------
   -- Interpreter --
   -----------------

   procedure Add_Command
     (Self : in out Interpreter;
      Name : in Atom;
      Cmd : in Command'Class) is
   begin
      Self.Commands.Insert (Name, Cmd);
      Self.Max_Length := Count'Max (Self.Max_Length, Name'Length);
   end Add_Command;


   procedure Add
     (Self : in out Interpreter;
      Name : in String;
      Cmd : in Command'Class) is
   begin
      Self.Add_Command (To_Atom (Name), Cmd);
   end Add;


   function Has_Command (Self : Interpreter; Name : Atom) return Boolean is
   begin
      return Self.Commands.Contains (Name);
   end Has_Command;


   function Is_Empty (Self : Interpreter) return Boolean is
   begin
      return Self.Commands.Is_Empty;
   end Is_Empty;


   procedure Set_Fallback
     (Self : in out Interpreter;
      Name : in Atom)
   is
      function Create return Atom;

      function Create return Atom is
      begin
         return Name;
      end Create;
   begin
      Self.Fallback_Name.Replace (Create'Access);
   end Set_Fallback;


   procedure Reset_Fallback (Self : in out Interpreter) is
   begin
      Self.Fallback_Name.Reset;
   end Reset_Fallback;


   not overriding procedure Execute
     (Self : in Interpreter;
      Expression : in out Lockable.Descriptor'Class;
      State : in out Shared_State;
      Context : in Shared_Context)
   is
      Event : Events.Event := Expression.Current_Event;
      Lock_State : Lockable.Lock_State;
   begin
      loop
         case Event is
            when Events.Add_Atom =>
               Self.Execute (State, Context, Expression.Current_Atom);
            when Events.Open_List =>
               Expression.Lock (Lock_State);
               begin
                  Expression.Next (Event);
                  if Event = Events.Add_Atom then
                     Self.Execute (State, Context, Expression);
                  end if;
               exception
                  when others =>
                     Expression.Unlock (Lock_State, False);
                     raise;
               end;
               Expression.Unlock (Lock_State);
            when Events.Close_List | Events.End_Of_Input | Events.Error =>
               exit;
         end case;

         Expression.Next (Event);
      end loop;
   end Execute;


   not overriding procedure Execute
     (Self : in Interpreter;
      Fallback : in Command'Class;
      Expression : in out Lockable.Descriptor'Class;
      State : in out Shared_State;
      Context : in Shared_Context)
   is
      procedure Dispatch (Process : not null access procedure
                            (Name : in Atom; Cmd : in Command'Class));
      procedure Process_Atom (Name : in Atom; Cmd : in Command'Class);
      procedure Process_Exp (Name : in Atom; Cmd : in Command'Class);

      procedure Dispatch (Process : not null access procedure
                            (Name : in Atom; Cmd : in Command'Class))
      is
         procedure Process_Fallback (Name : in Atom);

         procedure Process_Fallback (Name : in Atom) is
         begin
            Process (Name, Fallback);
         end Process_Fallback;

         Buffer : Atom (1 .. Self.Max_Length);
         Length : Count;
         Cursor : Command_Maps.Cursor;
      begin
         Expression.Read_Atom (Buffer, Length);
         if Length > Self.Max_Length then
            Expression.Query_Atom (Process_Fallback'Access);
         else
            Cursor := Self.Commands.Find (Buffer (1 .. Length));
            if Command_Maps.Has_Element (Cursor) then
               Command_Maps.Query_Element (Cursor, Process);
            else
               Process (Buffer (1 .. Length), Fallback);
            end if;
         end if;
      end Dispatch;

      procedure Process_Atom (Name : in Atom; Cmd : in Command'Class) is
      begin
         Cmd.Execute (State, Context, Name);
      end Process_Atom;

      procedure Process_Exp (Name : in Atom; Cmd : in Command'Class) is
         pragma Unreferenced (Name);
      begin
         Cmd.Execute (State, Context, Expression);
      end Process_Exp;

      Event : Events.Event := Expression.Current_Event;
      Lock_State : Lockable.Lock_State;
   begin
      loop
         case Event is
            when Events.Add_Atom =>
               Dispatch (Process_Atom'Access);

            when Events.Open_List =>
               Expression.Lock (Lock_State);
               begin
                  Expression.Next (Event);
                  if Event = Events.Add_Atom then
                     Dispatch (Process_Exp'Access);
                  end if;
               exception
                  when others =>
                     Expression.Unlock (Lock_State, False);
                     raise;
               end;
               Expression.Unlock (Lock_State);

            when Events.Close_List | Events.End_Of_Input | Events.Error =>
               exit;
         end case;

         Expression.Next (Event);
      end loop;
   end Execute;


   overriding procedure Execute
     (Self : in Interpreter;
      State : in out Shared_State;
      Context : in Shared_Context;
      Name : in Atom)
   is
      procedure Process_Atom (Key : in Atom; Cmd : in Command'Class);

      procedure Process_Atom (Key : in Atom; Cmd : in Command'Class) is
         pragma Unreferenced (Key);
      begin
         Cmd.Execute (State, Context, Name);
      end Process_Atom;

      Cursor : Command_Maps.Cursor;
   begin
      if Name'Length <= Self.Max_Length then
         Cursor := Self.Commands.Find (Name);
         if Command_Maps.Has_Element (Cursor) then
            Command_Maps.Query_Element (Cursor, Process_Atom'Access);
            return;
         end if;
      end if;

      if not Self.Fallback_Name.Is_Empty then
         Cursor := Self.Commands.Find (Self.Fallback_Name.Query.Data.all);
         if Command_Maps.Has_Element (Cursor) then
            Command_Maps.Query_Element (Cursor, Process_Atom'Access);
            return;
         end if;
      end if;

      raise Command_Not_Found
        with "Unknown command """ & To_String (Name) & '"';
   end Execute;


   overriding procedure Execute
     (Self : in Interpreter;
      State : in out Shared_State;
      Context : in Shared_Context;
      Cmd : in out Lockable.Descriptor'Class)
   is
      procedure Process_Exp (Name : in Atom; Actual : in Command'Class);

      procedure Process_Exp (Name : in Atom; Actual : in Command'Class) is
         pragma Unreferenced (Name);
      begin
         Actual.Execute (State, Context, Cmd);
      end Process_Exp;

      Buffer : Atom (1 .. Self.Max_Length);
      Length : Count;
      Cursor : Command_Maps.Cursor;
   begin
      if Cmd.Current_Event /= Events.Add_Atom then
         return;
      end if;

      Cmd.Read_Atom (Buffer, Length);

      if Length <= Self.Max_Length then
         Cursor := Self.Commands.Find (Buffer (1 .. Length));
         if Command_Maps.Has_Element (Cursor) then
            Command_Maps.Query_Element (Cursor, Process_Exp'Access);
            return;
         end if;
      end if;

      if not Self.Fallback_Name.Is_Empty then
         Cursor := Self.Commands.Find (Self.Fallback_Name.Query.Data.all);
         if Command_Maps.Has_Element (Cursor) then
            Command_Maps.Query_Element (Cursor, Process_Exp'Access);
            return;
         end if;
      end if;

      raise Command_Not_Found
        with "Unknown command """ & To_String (Cmd.Current_Atom) & '"';
   end Execute;



   --------------------------------------
   -- Interpreter Building Subprograms --
   --------------------------------------

   function Build (Commands : Command_Array) return Interpreter is
      Result : Interpreter;
   begin
      for I in Commands'Range loop
         Result.Add_Command
           (Commands (I).Name.Query.Data.all,
            Commands (I).Cmd.Query.Data.all);
      end loop;

      return Result;
   end Build;


   function Build (Commands : Command_Array; Fallback : String)
     return Interpreter
   is
      Result : Interpreter := Build (Commands);
   begin
      Result.Set_Fallback (To_Atom (Fallback));
      return Result;
   end Build;


   function Item (Name : String; Cmd : Command'Class)
     return Command_Description
   is
      function Get_Name return Atom;
      function Get_Command return Command'Class;

      function Get_Name return Atom is
      begin
         return To_Atom (Name);
      end Get_Name;

      function Get_Command return Command'Class is
      begin
         return Cmd;
      end Get_Command;
   begin
      return (Name => Atom_Refs.Create (Get_Name'Access),
              Cmd => Command_Refs.Create (Get_Command'Access));
   end Item;

end Natools.S_Expressions.Interpreters;
