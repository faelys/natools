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

with Natools.S_Expressions.Test_Tools;
with Natools.S_Expressions.Parsers;

package body Natools.S_Expressions.Lockable.Tests is

   -------------------------------
   -- Lockable.Descriptor Tests --
   -------------------------------

   function Test_Expression return Atom is
   begin
      return To_Atom ("(begin(command1 arg1.1 arg1.2)"
        & "(command2 (subcmd2.1 arg2.1.1) (subcmd2.3) arg2.4)"
        & "(command3 (subcmd3.1 ((subcmd3.1.1 arg3.1.1.1)) arg3.1.2))"
        & "end)5:extra");
   end Test_Expression;


   procedure Test_Interface
     (Test : in out NT.Test;
      Object : in out Lockable.Descriptor'Class)
   is
      Level_1, Level_2 : Lock_State;
      Base : Natural;
   begin
      Base := Object.Current_Level;
      Test_Tools.Next_And_Check (Test, Object, To_Atom ("begin"), Base);

      Test_Tools.Next_And_Check (Test, Object, Events.Open_List, Base + 1);
      Test_Tools.Next_And_Check (Test, Object, To_Atom ("command1"), Base + 1,
        "Before first lock:");
      Object.Lock (Level_1);
      Test_Tools.Test_Atom_Accessors (Test, Object, To_Atom ("command1"), 0,
        "After first lock:");
      Test_Tools.Next_And_Check (Test, Object, To_Atom ("arg1.1"), 0);
      Test_Tools.Next_And_Check (Test, Object, To_Atom ("arg1.2"), 0);
      Test_Tools.Next_And_Check (Test, Object, Events.End_Of_Input, 0,
        "Before first unlock:");
      Test_Tools.Test_Atom_Accessor_Exceptions (Test, Object);
      Object.Unlock (Level_1);

      declare
         Event : constant Events.Event := Object.Current_Event;
         Level : constant Natural := Object.Current_Level;
      begin
         if Event /= Events.Close_List then
            Test.Fail ("Current event is " & Events.Event'Image (Event)
              & ", expected Close_List");
         end if;
         if Level /= Base then
            Test.Fail ("Current level is" & Natural'Image (Level)
              & ", expected" & Natural'Image (Base));
         end if;
      end;

      Test_Tools.Next_And_Check (Test, Object, Events.Open_List, Base + 1);
      Test_Tools.Next_And_Check (Test, Object, To_Atom ("command2"), Base + 1,
        "Before second lock:");
      Object.Lock (Level_1);
      Test_Tools.Test_Atom_Accessors (Test, Object, To_Atom ("command2"), 0,
        "After second lock:");
      Test_Tools.Next_And_Check (Test, Object, Events.Open_List, 1);
      Test_Tools.Next_And_Check (Test, Object, To_Atom ("subcmd2.1"), 1,
        "Before inner lock:");
      Object.Lock (Level_2);
      Test_Tools.Test_Atom_Accessors (Test, Object, To_Atom ("subcmd2.1"), 0,
        "After inner lock:");
      Test_Tools.Next_And_Check (Test, Object, To_Atom ("arg2.1.1"), 0,
        "Before inner unlock:");
      Object.Unlock (Level_2, False);
      Test_Tools.Test_Atom_Accessors (Test, Object, To_Atom ("arg2.1.1"), 1,
        "After inner unlock:");
      Test_Tools.Next_And_Check (Test, Object, Events.Close_List, 0);
      Test_Tools.Next_And_Check (Test, Object, Events.Open_List, 1);
      Test_Tools.Next_And_Check (Test, Object, To_Atom ("subcmd2.3"), 1,
        "Before inner lock:");
      Object.Lock (Level_2);
      Test_Tools.Test_Atom_Accessors (Test, Object, To_Atom ("subcmd2.3"), 0,
        "After inner lock:");
      Test_Tools.Next_And_Check (Test, Object, Events.End_Of_Input, 0,
        "Before inner unlock:");
      Object.Unlock (Level_2, False);

      declare
         Event : constant Events.Event := Object.Current_Event;
         Level : constant Natural := Object.Current_Level;
      begin
         if Event /= Events.Close_List then
            Test.Fail ("Current event is " & Events.Event'Image (Event)
              & ", expected Close_List");
         end if;
         if Level /= 0 then
            Test.Fail ("Current level is" & Natural'Image (Level)
              & ", expected 0");
         end if;
      end;

      Object.Unlock (Level_1);

      Test_Tools.Next_And_Check (Test, Object, Events.Open_List, Base + 1);
      Test_Tools.Next_And_Check (Test, Object, To_Atom ("command3"), Base + 1,
        "Before third lock:");
      Object.Lock (Level_1);
      Test_Tools.Test_Atom_Accessors (Test, Object, To_Atom ("command3"), 0,
        "After third lock:");
      Test_Tools.Next_And_Check (Test, Object, Events.Open_List, 1);
      Test_Tools.Next_And_Check (Test, Object, To_Atom ("subcmd3.1"), 1);
      Test_Tools.Next_And_Check (Test, Object, Events.Open_List, 2);
      Test_Tools.Next_And_Check (Test, Object, Events.Open_List, 3);
      Object.Unlock (Level_1);

      Test_Tools.Next_And_Check (Test, Object, To_Atom ("end"), Base);
      Test_Tools.Next_And_Check (Test, Object, Events.Close_List, Base - 1);
      Test_Tools.Next_And_Check (Test, Object, To_Atom ("extra"), Base - 1);
      Object.Lock (Level_1);
      Test_Tools.Test_Atom_Accessors (Test, Object, To_Atom ("extra"), 0);
      Object.Unlock (Level_1);

      declare
         Event : constant Events.Event := Object.Current_Event;
      begin
         if Event /= Events.End_Of_Input then
            Test.Fail ("Last current event is "
              & Events.Event'Image (Event)
              & ", expected End_Of_Input");
         end if;
      end;
   end Test_Interface;



   -------------------------
   -- Complete Test Suite --
   -------------------------

   procedure All_Tests (Report : in out NT.Reporter'Class) is
   begin
      Test_Stack (Report);
      Test_Wrapper_Interface (Report);
      Test_Wrapper_Extra (Report);
   end All_Tests;



   ---------------------------
   -- Individual Test Cases --
   ---------------------------

   procedure Test_Stack (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Level stack");
   begin
      declare
         Stack : Lock_Stack;
         State : array (1 .. 4) of Lock_State;

         procedure Check_Level
           (Stack : in Lock_Stack;
            Expected : in Natural;
            Context : in String);
         procedure Dump_Data;

         procedure Check_Level
           (Stack : in Lock_Stack;
            Expected : in Natural;
            Context : in String)
         is
            Level : constant Natural := Current_Level (Stack);
         begin
            if Level /= Expected then
               Test.Fail (Context & ": level is"
                 & Natural'Image (Level) & ", expected"
                 & Natural'Image (Expected));
               Dump_Data;
            end if;
         end Check_Level;

         procedure Dump_Data is
         begin
            Test.Info ("   Stack: (Depth =>"
              & Natural'Image (Stack.Depth)
              & ", Level =>"
              & Natural'Image (Stack.Level) & ')');
            for I in State'Range loop
               Test.Info ("   State"
                 & Natural'Image (I)
                 & ": (Depth =>"
                 & Natural'Image (Stack.Depth)
                 & ", Level =>"
                 & Natural'Image (Stack.Level) & ')');
            end loop;
         end Dump_Data;
      begin
         Check_Level (Stack, 0, "1");
         Push_Level (Stack, 14, State (1));
         Check_Level (Stack, 14, "2");

         begin
            Pop_Level (Stack, State (2));
            Test.Fail ("No exception raised after popping blank state");
         exception
            when Constraint_Error =>
               null;
            when Error : others =>
               Test.Fail
                 ("Unexpected exception raised after popping blank state");
               Test.Report_Exception (Error, NT.Fail);
         end;

         Pop_Level (Stack, State (1));
         Check_Level (Stack, 0, "3");
         Push_Level (Stack, 15, State (1));
         Check_Level (Stack, 15, "4");
         Push_Level (Stack, 92, State (2));
         Check_Level (Stack, 92, "5");
         Push_Level (Stack, 65, State (3));
         Check_Level (Stack, 65, "6");
         Pop_Level (Stack, State (3));
         Check_Level (Stack, 92, "7");
         Push_Level (Stack, 35, State (3));
         Check_Level (Stack, 35, "8");
         Push_Level (Stack, 89, State (4));
         Check_Level (Stack, 89, "9");

         begin
            Pop_Level (Stack, State (3));
            Test.Fail ("No exception raised after popping a forbidden gap");
         exception
            when Constraint_Error =>
               null;
            when Error : others =>
               Test.Fail
                 ("Unexpected exception raised after popping a forbidden gap");
               Test.Report_Exception (Error, NT.Fail);
         end;

         Check_Level (Stack, 89, "10");
         Pop_Level (Stack, State (3), True);
         Check_Level (Stack, 92, "11");

         begin
            Pop_Level (Stack, State (4));
            Test.Fail ("No exception raised after popping stale state");
         exception
            when Constraint_Error =>
               null;
            when Error : others =>
               Test.Fail
                 ("Unexpected exception raised after popping stale state");
               Test.Report_Exception (Error, NT.Fail);
         end;

         Check_Level (Stack, 92, "12");
         Pop_Level (Stack, State (2));
         Check_Level (Stack, 15, "13");
         Pop_Level (Stack, State (1));
         Check_Level (Stack, 0, "14");
      end;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Test_Stack;


   procedure Test_Wrapper_Extra (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Extra tests of wrapper");
   begin
      declare
         Input : aliased Test_Tools.Memory_Stream;
         Parser : aliased Parsers.Stream_Parser (Input'Access);
         Tested : Wrapper (Parser'Access);
         State : Lock_State;
      begin
         Input.Set_Data (To_Atom ("(cmd1 arg1)(cmd2 4:arg2"));

         --  Check Events.Error is returned by Next when finished

         Test_Tools.Next_And_Check (Test, Tested, Events.Open_List, 1);
         Test_Tools.Next_And_Check (Test, Tested, To_Atom ("cmd1"), 1);
         Tested.Lock (State);
         Test_Tools.Next_And_Check (Test, Tested, To_Atom ("arg1"), 0);
         Test_Tools.Next_And_Check (Test, Tested, Events.End_Of_Input, 0);
         Test_Tools.Next_And_Check (Test, Tested, Events.Error, 0);
         Tested.Unlock (State);

         --  Run Unlock with End_Of_Input in backend

         Test_Tools.Next_And_Check (Test, Tested, Events.Open_List, 1);
         Test_Tools.Next_And_Check (Test, Tested, To_Atom ("cmd2"), 1);
         Tested.Lock (State);
         Test_Tools.Next_And_Check (Test, Tested, To_Atom ("arg2"), 0);
         Tested.Unlock (State);
      end;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Test_Wrapper_Extra;


   procedure Test_Wrapper_Interface (Report : in out NT.Reporter'Class) is
      Test : NT.Test
        := Report.Item ("Lockable.Descriptor interface of wrapper");
   begin
      declare
         Input : aliased Test_Tools.Memory_Stream;
         Parser : aliased Parsers.Stream_Parser (Input'Access);
         Tested : Wrapper (Parser'Access);
      begin
         Input.Set_Data (Test_Expression);
         Test_Tools.Next_And_Check (Test, Parser, Events.Open_List, 1);
         Test_Interface (Test, Tested);
      end;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Test_Wrapper_Interface;

end Natools.S_Expressions.Lockable.Tests;
