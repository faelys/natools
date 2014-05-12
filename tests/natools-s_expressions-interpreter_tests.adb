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

with Natools.S_Expressions.Caches;
with Natools.S_Expressions.Test_Tools;

package body Natools.S_Expressions.Interpreter_Tests is

   function Test_Interpreter return Test_Interpreters.Interpreter;

   function Invalid_Commands return Caches.Reference;


   ------------------------
   -- Helper Subprograms --
   ------------------------

   function Invalid_Commands return Caches.Reference is
      Cache : Caches.Reference;
      Short : constant Atom := To_Atom ("not-cmd");
      Long : constant Atom := To_Atom ("not-a-command");
   begin
      Cache.Append_Atom (Short);
      Cache.Open_List;
      Cache.Append_Atom (Short);
      Cache.Append_Atom (To_Atom ("arg"));
      Cache.Close_List;
      Cache.Append_Atom (Long);
      Cache.Open_List;
      Cache.Append_Atom (Long);
      Cache.Open_List;
      Cache.Close_List;
      Cache.Close_List;
      return Cache;
   end Invalid_Commands;


   function Test_Interpreter return Test_Interpreters.Interpreter is
      Template : Recorder;
   begin
      return Inter : Test_Interpreters.Interpreter do
         Inter.Add ("cmd", Template);
         Inter.Add ("command", Template);
      end return;
   end Test_Interpreter;



   ----------------------
   -- Recorder Command --
   ----------------------

   overriding procedure Execute
     (Self : in out Recorder;
      State : in out Printers.Printer'Class;
      Context : in Boolean;
      Name : in Atom)
   is
      pragma Unreferenced (Self);
   begin
      if Context then
         State.Append_Atom (Name);
      end if;
   end Execute;


   overriding procedure Execute
     (Self : in out Recorder;
      State : in out Printers.Printer'Class;
      Context : in Boolean;
      Cmd : in out Lockable.Descriptor'Class)
   is
      pragma Unreferenced (Self);
   begin
      if not Context then
         return;
      end if;

      declare
         Buffer : aliased Test_Tools.Memory_Stream;
         Serializer : Printers.Canonical (Buffer'Access);
      begin
         Printers.Transfer (Cmd, Serializer);
         State.Open_List;
         State.Append_Atom (Buffer.Get_Data);
         State.Close_List;
      end;
   end Execute;



   --------------------
   -- Raiser Command --
   --------------------

   overriding procedure Execute
     (Self : in out Raiser;
      State : in out Printers.Printer'Class;
      Context : in Boolean;
      Name : in Atom)
   is
      pragma Unreferenced (Self, State, Context, Name);
   begin
      raise Special_Exception;
   end Execute;


   overriding procedure Execute
     (Self : in out Raiser;
      State : in out Printers.Printer'Class;
      Context : in Boolean;
      Cmd : in out Lockable.Descriptor'Class)
   is
      pragma Unreferenced (Self, State, Context, Cmd);
   begin
      raise Special_Exception;
   end Execute;



   -------------------------
   -- Complete Test Suite --
   -------------------------

   procedure All_Tests (Report : in out NT.Reporter'Class) is
   begin
      Test_Basic_Usage (Report);
      Test_Unknown_Commands (Report);
      Test_Premanent_Fallback (Report);
      Test_Local_Fallback (Report);
      Test_Exception_Fallback (Report);
      Test_Inspection (Report);
   end All_Tests;



   ----------------------
   -- Individual Tests --
   ----------------------

   procedure Test_Basic_Usage (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Basic usage");
   begin
      declare
         Inter : Test_Interpreters.Interpreter := Test_Interpreter;
         Buffer : aliased Test_Tools.Memory_Stream;
         Printer : Printers.Canonical (Buffer'Access);
         Input : Caches.Reference;
         Cursor : Caches.Cursor;
      begin
         Input.Append_Atom (To_Atom ("cmd"));
         Input.Open_List;
         Input.Append_Atom (To_Atom ("cmd"));
         Input.Append_Atom (To_Atom ("foo"));
         Input.Append_Atom (To_Atom ("bar"));
         Input.Close_List;
         Input.Append_Atom (To_Atom ("command"));
         Input.Open_List;
         Input.Open_List;
         Input.Append_Atom (To_Atom ("comment"));
         Input.Close_List;
         Input.Close_List;
         Input.Open_List;
         Input.Append_Atom (To_Atom ("command"));
         Input.Open_List;
         Input.Close_List;
         Input.Close_List;

         Cursor := Input.First;

         Buffer.Set_Expected (To_Atom
           ("3:cmd(15:3:cmd3:foo3:bar)7:command(11:7:command())"));

         Inter.Execute (Cursor, Printer, True);
         Buffer.Check_Stream (Test);
      end;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Test_Basic_Usage;


   procedure Test_Exception_Fallback (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Local fallback raising an exception");
   begin
      declare
         Inter : Test_Interpreters.Interpreter := Test_Interpreter;
         Buffer : aliased Test_Tools.Memory_Stream;
         Printer : Printers.Canonical (Buffer'Access);
         Input : Caches.Reference;
         Cursor : Caches.Cursor;
         Fallback : Raiser;
      begin
         Input.Append_Atom (To_Atom ("cmd"));
         Input.Open_List;
         Input.Append_Atom (To_Atom ("unknown"));
         Input.Append_Atom (To_Atom ("argument"));
         Input.Close_List;
         Input.Close_List;
         Input.Open_List;
         Input.Append_Atom (To_Atom ("command"));
         Input.Close_List;
         Cursor := Input.First;

         Buffer.Set_Expected (To_Atom ("3:cmd"));

         begin
            Inter.Execute (Fallback, Cursor, Printer, True);
            Test.Fail ("No exception raised");
         exception
            when Special_Exception => null;
            when Error : others =>
               Test.Fail ("Wrong exception raised:");
               Test.Report_Exception (Error, NT.Fail);
         end;

         Buffer.Check_Stream (Test);

         Test_Tools.Next_And_Check (Test, Cursor, To_Atom ("argument"), 1);
         Test_Tools.Next_And_Check (Test, Cursor, Events.Close_List, 0);
         Test_Tools.Next_And_Check (Test, Cursor, Events.Open_List, 1);
         Test_Tools.Next_And_Check (Test, Cursor, To_Atom ("command"), 1);
         Test_Tools.Next_And_Check (Test, Cursor, Events.Close_List, 0);
         Test_Tools.Next_And_Check (Test, Cursor, Events.End_Of_Input, 0);
      end;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Test_Exception_Fallback;


   procedure Test_Inspection (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Inspection");
   begin
      declare
         Inter : Test_Interpreters.Interpreter;
      begin
         if not Inter.Is_Empty then
            Test.Fail ("Default interpreter is not empty");
         end if;

         if Inter.Has_Command (To_Atom ("cmd")) then
            Test.Fail ("Default interpreter has command ""cmd""");
         end if;

         Inter := Test_Interpreter;

         if Inter.Is_Empty then
            Test.Fail ("Test interpreter is empty");
         end if;

         if not Inter.Has_Command (To_Atom ("cmd")) then
            Test.Fail ("Test interpreter has not command ""cmd""");
         end if;

         if Inter.Has_Command (To_Atom ("not-a-cmd")) then
            Test.Fail ("Test interpreter has command ""not-a-cmd""");
         end if;

      end;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Test_Inspection;


   procedure Test_Local_Fallback (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Local fallback");
   begin
      declare
         Inter : Test_Interpreters.Interpreter := Test_Interpreter;
         Buffer : aliased Test_Tools.Memory_Stream;
         Printer : Printers.Canonical (Buffer'Access);
         Input : Caches.Reference := Invalid_Commands;
         Cursor : Caches.Cursor := Input.First;
         Fallback : Recorder;
      begin
         Input.Append_Atom (To_Atom ("cmd"));
         Buffer.Set_Expected (To_Atom
           ("7:not-cmd(14:7:not-cmd3:arg)13:not-a-command"
            & "(18:13:not-a-command())3:cmd"));

         Inter.Execute (Fallback, Cursor, Printer, True);

         Buffer.Check_Stream (Test);
      end;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Test_Local_Fallback;


   procedure Test_Premanent_Fallback (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Permanent fallback");
   begin
      declare
         Inter : Test_Interpreters.Interpreter := Test_Interpreter;
         Buffer : aliased Test_Tools.Memory_Stream;
         Printer : Printers.Canonical (Buffer'Access);
         Input : constant Caches.Reference := Invalid_Commands;
         Cursor : Caches.Cursor := Input.First;
      begin
         Buffer.Set_Expected (To_Atom
           ("7:not-cmd(14:7:not-cmd3:arg)13:not-a-command"
            & "(18:13:not-a-command())"));

         Inter.Set_Fallback (To_Atom ("cmd"));
         Inter.Execute (Cursor, Printer, True);

         Buffer.Check_Stream (Test);
      end;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Test_Premanent_Fallback;


   procedure Test_Unknown_Commands (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Unknown commands");
   begin
      declare
         Inter : Test_Interpreters.Interpreter := Test_Interpreter;
         Buffer : aliased Test_Tools.Memory_Stream;
         Printer : Printers.Canonical (Buffer'Access);
         Input : constant Caches.Reference := Invalid_Commands;
         Cursor : Caches.Cursor := Input.First;
      begin
         Inter.Set_Fallback (To_Atom ("cmd"));
         Inter.Reset_Fallback;

         begin
            Inter.Execute (Cursor, Printer, True);
            Test.Fail ("No exception raised after not-cmd");
         exception
            when Test_Interpreters.Command_Not_Found => null;
            when Error : others =>
               Test.Fail ("Unexpected exception raised after not-cmd");
               Test.Report_Exception (Error, NT.Fail);
         end;

         Test_Tools.Next_And_Check (Test, Cursor, Events.Open_List, 1);

         begin
            Inter.Execute (Cursor, Printer, True);
            Test.Fail ("No exception raised after (not-cmd)");
         exception
            when Test_Interpreters.Command_Not_Found => null;
            when Error : others =>
               Test.Fail ("Unexpected exception raised after (not-cmd)");
               Test.Report_Exception (Error, NT.Fail);
         end;

         Test_Tools.Next_And_Check (Test, Cursor, To_Atom ("arg"), 1);
         Test_Tools.Next_And_Check (Test, Cursor, Events.Close_List, 0);
         Test_Tools.Next_And_Check
           (Test, Cursor, To_Atom ("not-a-command"), 0);

         begin
            Inter.Execute (Cursor, Printer, True);
            Test.Fail ("No exception raised after not-a-command");
         exception
            when Test_Interpreters.Command_Not_Found => null;
            when Error : others =>
               Test.Fail ("Unexpected exception raised after not-a-command");
               Test.Report_Exception (Error, NT.Fail);
         end;

         Test_Tools.Next_And_Check (Test, Cursor, Events.Open_List, 1);

         begin
            Inter.Execute (Cursor, Printer, True);
            Test.Fail ("No exception raised after not-a-command");
         exception
            when Test_Interpreters.Command_Not_Found => null;
            when Error : others =>
               Test.Fail ("Unexpected exception raised after not-a-command");
               Test.Report_Exception (Error, NT.Fail);
         end;

         Test_Tools.Next_And_Check (Test, Cursor, Events.Open_List, 2);
         Test_Tools.Next_And_Check (Test, Cursor, Events.Close_List, 1);
         Test_Tools.Next_And_Check (Test, Cursor, Events.Close_List, 0);
         Test_Tools.Next_And_Check (Test, Cursor, Events.End_Of_Input, 0);

         Buffer.Check_Stream (Test);
      end;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Test_Unknown_Commands;

end Natools.S_Expressions.Interpreter_Tests;
