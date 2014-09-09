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

with System.Storage_Pools;

with GNAT.Debug_Pools;

with Natools.S_Expressions.Atom_Buffers;
with Natools.S_Expressions.Caches;
with Natools.S_Expressions.Generic_Caches;
with Natools.S_Expressions.Lockable.Tests;
with Natools.S_Expressions.Parsers;
with Natools.S_Expressions.Printers;
with Natools.S_Expressions.Test_Tools;

package body Natools.S_Expressions.Cache_Tests is

   Pool : GNAT.Debug_Pools.Debug_Pool;

   package Debug_Caches is new Generic_Caches
     (System.Storage_Pools.Root_Storage_Pool'Class (Pool),
      System.Storage_Pools.Root_Storage_Pool'Class (Pool),
      System.Storage_Pools.Root_Storage_Pool'Class (Pool));


   procedure Inject_Test (Printer : in out Printers.Printer'Class);
      --  Inject test S-expression into Pr

   function Canonical_Test return Atom;
      --  Return canonical encoding of test S-expression above


   ------------------------
   -- Helper Subprograms --
   ------------------------

   function Canonical_Test return Atom is
   begin
      return To_Atom ("5:begin(()(4:head4:tail))3:end");
   end Canonical_Test;


   procedure Inject_Test (Printer : in out Printers.Printer'Class) is
   begin
      Printer.Append_Atom (To_Atom ("begin"));
      Printer.Open_List;
      Printer.Open_List;
      Printer.Close_List;
      Printer.Open_List;
      Printer.Append_Atom (To_Atom ("head"));
      Printer.Append_Atom (To_Atom ("tail"));
      Printer.Close_List;
      Printer.Close_List;
      Printer.Append_Atom (To_Atom ("end"));
   end Inject_Test;


   -------------------------
   -- Complete Test Suite --
   -------------------------

   procedure All_Tests (Report : in out NT.Reporter'Class) is
   begin
      Default_Instantiation (Report);
      Debug_Instantiation (Report);
      Descriptor_Interface (Report);
      Lockable_Interface (Report);
      Replayable_Interface (Report);
   end All_Tests;


   -----------------------
   -- Inidividual Tests --
   -----------------------

   procedure Debug_Instantiation (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Debug instantiation");
      Buffer : Atom_Buffers.Atom_Buffer;

      procedure Put (S : in String);
      procedure Put_Line (S : in String);
      procedure Flush;
      procedure Info_Pool;

      procedure Put (S : in String) is
      begin
         Buffer.Append (To_Atom (S));
      end Put;

      procedure Put_Line (S : in String) is
      begin
         Test.Info (To_String (Buffer.Data) & S);
         Buffer.Soft_Reset;
      end Put_Line;

      procedure Flush is
      begin
         if Buffer.Length > 0 then
            Test.Info (To_String (Buffer.Data));
         end if;
         Buffer.Hard_Reset;
      end Flush;

      procedure Info_Pool is
         procedure Print_Info is new GNAT.Debug_Pools.Print_Info;
      begin
         Print_Info (Pool);
         Flush;
      end Info_Pool;
   begin
      declare
         Cache, Deep, Shallow : Debug_Caches.Reference;
      begin
         declare
            Empty_Cursor : Debug_Caches.Cursor := Deep.First;
            Event : Events.Event;
         begin
            Event := Empty_Cursor.Current_Event;
            if Event /= Events.End_Of_Input then
               Test.Fail ("Unexpected Empty_Cursor.Current_Event "
                 & Events.Event'Image (Event)
                 & " (expected End_Of_Input)");
            end if;

            Test_Tools.Next_And_Check
              (Test, Empty_Cursor, Events.End_Of_Input, 0);
         end;

         Inject_Test (Cache);

         declare
            First : Debug_Caches.Cursor := Cache.First;
            Output : aliased Test_Tools.Memory_Stream;
            Pr : Printers.Canonical (Output'Access);
         begin
            Output.Set_Expected (Canonical_Test);
            Printers.Transfer (First, Pr);
            Output.Check_Stream (Test);
         end;

         Deep := Cache.Duplicate;
         Shallow := Deep;
         Deep.Append_Atom (To_Atom ("more"));

         declare
            Other : Debug_Caches.Cursor := Deep.First;
            Output : aliased Test_Tools.Memory_Stream;
            Pr : Printers.Canonical (Output'Access);
         begin
            Output.Set_Expected (Canonical_Test & To_Atom ("4:more"));
            Printers.Transfer (Other, Pr);
            Output.Check_Stream (Test);
         end;

         declare
            Second : Debug_Caches.Cursor := Cache.First;
            Output : aliased Test_Tools.Memory_Stream;
            Pr : Printers.Canonical (Output'Access);
         begin
            Output.Set_Expected (Canonical_Test);
            Printers.Transfer (Second, Pr);
            Output.Check_Stream (Test);
         end;

         declare
            Second_Other : Debug_Caches.Cursor := Shallow.First;
            Output : aliased Test_Tools.Memory_Stream;
            Pr : Printers.Canonical (Output'Access);
         begin
            Output.Set_Expected (Canonical_Test & To_Atom ("4:more"));
            Printers.Transfer (Second_Other, Pr);
            Output.Check_Stream (Test);
         end;
      end;

      Info_Pool;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Debug_Instantiation;


   procedure Default_Instantiation (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Default instantiation");
   begin
      declare
         Cache, Deep, Shallow : Caches.Reference;
      begin
         declare
            Empty_Cursor : Caches.Cursor := Deep.First;
            Event : Events.Event;
         begin
            Event := Empty_Cursor.Current_Event;
            if Event /= Events.End_Of_Input then
               Test.Fail ("Unexpected Empty_Cursor.Current_Event "
                 & Events.Event'Image (Event)
                 & " (expected End_Of_Input)");
            end if;

            Test_Tools.Next_And_Check
              (Test, Empty_Cursor, Events.End_Of_Input, 0);
         end;
         Inject_Test (Cache);

         declare
            First : Caches.Cursor := Cache.First;
            Output : aliased Test_Tools.Memory_Stream;
            Pr : Printers.Canonical (Output'Access);
         begin
            Output.Set_Expected (Canonical_Test);
            Printers.Transfer (First, Pr);
            Output.Check_Stream (Test);
         end;

         Deep := Cache.Duplicate;
         Shallow := Deep;
         Deep.Append_Atom (To_Atom ("more"));

         declare
            Other : Caches.Cursor := Deep.First;
            Output : aliased Test_Tools.Memory_Stream;
            Pr : Printers.Canonical (Output'Access);
         begin
            Output.Set_Expected (Canonical_Test & To_Atom ("4:more"));
            Printers.Transfer (Other, Pr);
            Output.Check_Stream (Test);
         end;

         declare
            Second : Caches.Cursor := Cache.First;
            Output : aliased Test_Tools.Memory_Stream;
            Pr : Printers.Canonical (Output'Access);
         begin
            Output.Set_Expected (Canonical_Test);
            Printers.Transfer (Second, Pr);
            Output.Check_Stream (Test);
         end;

         declare
            Second_Other : Caches.Cursor := Shallow.First;
            Output : aliased Test_Tools.Memory_Stream;
            Pr : Printers.Canonical (Output'Access);
         begin
            Output.Set_Expected (Canonical_Test & To_Atom ("4:more"));
            Printers.Transfer (Second_Other, Pr);
            Output.Check_Stream (Test);
         end;
      end;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Default_Instantiation;


   procedure Descriptor_Interface (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Descriptor insterface");
   begin
      declare
         Cache : Caches.Reference;
         First, Second : Caches.Cursor;
      begin
         Cache.Append_Atom (To_Atom ("begin"));
         Cache.Open_List;
         Cache.Append_Atom (To_Atom ("command"));
         Cache.Open_List;
         Cache.Append_Atom (To_Atom ("first"));
         Cache.Append_Atom (To_Atom ("second"));
         Cache.Close_List;
         Cache.Close_List;
         Cache.Append_Atom (To_Atom ("end"));

         First := Cache.First;
         Second := First;

         Test_Tools.Test_Atom_Accessors (Test, First, To_Atom ("begin"), 0);
         Test_Tools.Test_Atom_Accessors (Test, Second, To_Atom ("begin"), 0);

         Test_Tools.Next_And_Check (Test, First, Events.Open_List, 1);
         Test_Tools.Test_Atom_Accessor_Exceptions (Test, First);
         Test_Tools.Next_And_Check (Test, First, To_Atom ("command"), 1);
         Test_Tools.Next_And_Check (Test, First, Events.Open_List, 2);
         Test_Tools.Next_And_Check (Test, Second, Events.Open_List, 1);
         Test_Tools.Next_And_Check (Test, First, To_Atom ("first"), 2);
         Test_Tools.Next_And_Check (Test, Second, To_Atom ("command"), 1);
         Test_Tools.Next_And_Check (Test, First, To_Atom ("second"), 2);
         Test_Tools.Next_And_Check (Test, First, Events.Close_List, 1);
         Test_Tools.Next_And_Check (Test, Second, Events.Open_List, 2);
         Test_Tools.Next_And_Check (Test, Second, To_Atom ("first"), 2);
         Test_Tools.Next_And_Check (Test, Second, To_Atom ("second"), 2);
         Test_Tools.Next_And_Check (Test, First, Events.Close_List, 0);
         Test_Tools.Next_And_Check (Test, Second, Events.Close_List, 1);
         Test_Tools.Test_Atom_Accessor_Exceptions (Test, Second);
         Test_Tools.Next_And_Check (Test, Second, Events.Close_List, 0);
         Test_Tools.Next_And_Check (Test, Second, To_Atom ("end"), 0);
         Test_Tools.Next_And_Check (Test, First, To_Atom ("end"), 0);
         Test_Tools.Next_And_Check (Test, First, Events.End_Of_Input, 0);
         Test_Tools.Next_And_Check (Test, Second, Events.End_Of_Input, 0);
      end;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Descriptor_Interface;


   procedure Lockable_Interface (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Lockable.Descriptor insterface");
   begin
      declare
         Cache : Caches.Reference;
      begin
         declare
            Input : aliased Test_Tools.Memory_Stream;
            Parser : Parsers.Stream_Parser (Input'Access);
         begin
            Input.Set_Data (Lockable.Tests.Test_Expression);
            Test_Tools.Next_And_Check (Test, Parser, Events.Open_List, 1);
            Printers.Transfer (Parser, Cache);
         end;

         declare
            Cursor : Caches.Cursor := Cache.First;
         begin
            Lockable.Tests.Test_Interface (Test, Cursor);
         end;
      end;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Lockable_Interface;


   procedure Replayable_Interface (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Replayable.Descriptor insterface");
   begin
      declare
         Cache : Caches.Reference;
         First, Second : Caches.Cursor;
      begin
         Cache.Append_Atom (To_Atom ("begin"));
         Cache.Open_List;
         Cache.Append_Atom (To_Atom ("command"));
         Cache.Open_List;
         Cache.Append_Atom (To_Atom ("first"));
         Cache.Append_Atom (To_Atom ("second"));
         Cache.Close_List;
         Cache.Close_List;
         Cache.Append_Atom (To_Atom ("end"));

         First := Cache.First;

         Test_Tools.Test_Atom_Accessors (Test, First, To_Atom ("begin"), 0);

         Test_Tools.Next_And_Check (Test, First, Events.Open_List, 1);
         Test_Tools.Test_Atom_Accessor_Exceptions (Test, First);
         Test_Tools.Next_And_Check (Test, First, To_Atom ("command"), 1);
         Second := First.Duplicate;
         Test_Tools.Next_And_Check (Test, First, Events.Open_List, 2);
         Test_Tools.Next_And_Check (Test, First, To_Atom ("first"), 2);
         Test_Tools.Next_And_Check (Test, First, To_Atom ("second"), 2);
         Test_Tools.Next_And_Check (Test, First, Events.Close_List, 1);
         Test_Tools.Next_And_Check (Test, Second, Events.Open_List, 2);
         Test_Tools.Next_And_Check (Test, Second, To_Atom ("first"), 2);
         Test_Tools.Next_And_Check (Test, Second, To_Atom ("second"), 2);
         Test_Tools.Next_And_Check (Test, First, Events.Close_List, 0);
         Test_Tools.Next_And_Check (Test, Second, Events.Close_List, 1);
         Test_Tools.Test_Atom_Accessor_Exceptions (Test, Second);
         Test_Tools.Next_And_Check (Test, Second, Events.Close_List, 0);
         Test_Tools.Next_And_Check (Test, Second, To_Atom ("end"), 0);
         Test_Tools.Next_And_Check (Test, First, To_Atom ("end"), 0);
         Test_Tools.Next_And_Check (Test, First, Events.End_Of_Input, 0);
         Test_Tools.Next_And_Check (Test, Second, Events.End_Of_Input, 0);
      end;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Replayable_Interface;

end Natools.S_Expressions.Cache_Tests;
