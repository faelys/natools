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

with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with Natools.S_Expressions.Atom_Buffers;
with Natools.S_Expressions.File_Readers;
with Natools.S_Expressions.File_Writers;
with Natools.S_Expressions.Test_Tools;

with GNAT.Debug_Pools;

package body Natools.S_Expressions.File_RW_Tests is

   package Stream_IO renames Ada.Streams.Stream_IO;

   subtype String_Holder is Ada.Strings.Unbounded.Unbounded_String;

   function Hold (S : String) return String_Holder
     renames Ada.Strings.Unbounded.To_Unbounded_String;

   function To_String (H : String_Holder) return String
     renames Ada.Strings.Unbounded.To_String;



   -------------------------
   -- Complete Test Suite --
   -------------------------

   procedure All_Tests (Report : in out NT.Reporter'Class) is
   begin
      Atom_IO (Report);
      S_Expression_IO (Report);
   end All_Tests;



   -----------------------
   -- Inidividual Tests --
   -----------------------

   procedure Atom_IO (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Atom-based reading");
      Payload : Atom (0 .. 255);
      Temporary_File_Name : String_Holder;
   begin
      for I in Payload'Range loop
         Payload (I) := Octet (I);
      end loop;

      Build_File :
      declare
         File : Stream_IO.File_Type;
      begin
         Stream_IO.Create (File, Stream_IO.Out_File, "");
         Stream_IO.Write (File, Payload);
         Temporary_File_Name := Hold (Stream_IO.Name (File));
         Stream_IO.Close (File);
      end Build_File;

      Read_Test :
      declare
         Reader : File_Readers.Atom_Reader
           := File_Readers.Reader (To_String (Temporary_File_Name));
      begin
         Test_Tools.Test_Atom (Test, Payload, Reader.Read);

         Small_Read :
         declare
            Buffer : Atom (1 .. 100) := (others => 0);
            Length : Count;
         begin
            Reader.Read (Buffer, Length);
            Test_Tools.Test_Atom
              (Test, Payload (0 .. Buffer'Length - 1), Buffer);

            if Length /= Payload'Length then
               Test.Fail ("Expected total length"
                 & Count'Image (Payload'Length)
                 & " in small read, found"
                 & Count'Image (Length));
            end if;
         end Small_Read;

         Large_Read :
         declare
            Buffer : Atom (1 .. 512) := (others => 0);
            Length : Count;
         begin
            Reader.Read (Buffer, Length);

            Test_Tools.Test_Atom
              (Test, Payload, Buffer (Buffer'First .. Length));
            Test_Tools.Test_Atom
              (Test,
               (1 .. Buffer'Length - Length => 0),
               Buffer (Length + 1 .. Buffer'Last));
         end Large_Read;

         Reader.Set_Filename (To_String (Temporary_File_Name));

         Buffer_Read :
         declare
            Buffer : Atom_Buffers.Atom_Buffer;
         begin
            Reader.Read (Buffer, 100);
            Test_Tools.Test_Atom (Test, Payload, Buffer.Data);
         end Buffer_Read;

         Block_Read :
         declare
            procedure Process (Block : in Atom);

            Offset : Count := 0;

            procedure Process (Block : in Atom) is
               Next : constant Count := Offset + Block'Length;
            begin
               Test_Tools.Test_Atom
                 (Test, Payload (Offset .. Next - 1), Block);
               Offset := Next;
            end Process;
         begin
            Reader.Block_Query (100, Process'Access);

            if Offset /= Payload'Last + 1 then
               Test.Fail ("Expected final offset"
                 & Count'Image (Payload'Last + 1)
                 & ", found"
                 & Count'Image (Offset));
            end if;

            Offset := 0;
            Reader.Block_Query (350, Process'Access);

            if Offset /= Payload'Last + 1 then
               Test.Fail ("Expected second final offset"
                 & Count'Image (Payload'Last + 1)
                 & ", found"
                 & Count'Image (Offset));
            end if;
         end Block_Read;

         Heap_Read :
         declare
            procedure Tester (Data : in Atom);
            procedure Raiser (Data : in Atom);

            Local_Exception : exception;

            procedure Tester (Data : in Atom) is
            begin
               Test_Tools.Test_Atom (Test, Payload, Data);
            end Tester;

            procedure Raiser (Data : in Atom) is
            begin
               raise Local_Exception;
            end Raiser;

            Pool : GNAT.Debug_Pools.Debug_Pool;

            type Local_Atom_Access is access Atom;
            for Local_Atom_Access'Storage_Pool use Pool;

            procedure Unchecked_Deallocation is new Ada.Unchecked_Deallocation
              (Atom, Local_Atom_Access);

            procedure Query is new File_Readers.Query
              (Local_Atom_Access, Unchecked_Deallocation);
         begin
            Query (Reader, Tester'Access);

            begin
               Query (Reader, Raiser'Access);
            exception
               when Local_Exception => null;
            end;
         end Heap_Read;
      end Read_Test;

   exception
      when Error : others => Test.Report_Exception (Error);
   end Atom_IO;


   procedure S_Expression_IO (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("S-expression writing and re-reading");
      Temporary_File_Name, Secondary_File_Name : String_Holder;
   begin
      First_Write :
      declare
         Writer : File_Writers.Writer := File_Writers.Create ("");
      begin
         Temporary_File_Name := Hold (Writer.Name);
         Writer.Append_Atom (To_Atom ("begin"));
         Writer.Open_List;
         Writer.Open_List;
         Writer.Close_List;
         Writer.Open_List;
         Writer.Append_Atom (To_Atom ("head"));
         Writer.Append_Atom (To_Atom ("tail"));
         Writer.Close_List;
         Writer.Close_List;
         Writer.Append_Atom (To_Atom ("end"));

         Writer.Create ("");
         Secondary_File_Name := Hold (Writer.Name);
         Writer.Open_List;
         Writer.Append_Atom (To_Atom ("first"));
         Writer.Append_Atom (To_Atom ("last"));
         Writer.Close_List;
      end First_Write;

      First_Read :
      declare
         Reader : File_Readers.S_Reader
           := File_Readers.Reader (To_String (Temporary_File_Name));
      begin
         Test_Tools.Test_Atom_Accessors (Test, Reader, To_Atom ("begin"), 0);
         Test_Tools.Next_And_Check (Test, Reader, Events.Open_List, 1);
         Test_Tools.Next_And_Check (Test, Reader, Events.Open_List, 2);
         Test_Tools.Next_And_Check (Test, Reader, Events.Close_List, 1);
         Test_Tools.Next_And_Check (Test, Reader, Events.Open_List, 2);
         Test_Tools.Next_And_Check (Test, Reader, To_Atom ("head"), 2);
         Test_Tools.Next_And_Check (Test, Reader, To_Atom ("tail"), 2);
         Test_Tools.Next_And_Check (Test, Reader, Events.Close_List, 1);
         Test_Tools.Next_And_Check (Test, Reader, Events.Close_List, 0);
         Test_Tools.Next_And_Check (Test, Reader, To_Atom ("end"), 0);
         Test_Tools.Next_And_Check (Test, Reader, Events.End_Of_Input, 0);

         Reader.Rewind;
         Test_Tools.Test_Atom_Accessors (Test, Reader, To_Atom ("begin"), 0);

         Reader.Set_Filename (To_String (Secondary_File_Name));
         Test_Tools.Next_And_Check (Test, Reader, To_Atom ("first"), 1);
         Test_Tools.Next_And_Check (Test, Reader, To_Atom ("last"), 1);
         Test_Tools.Next_And_Check (Test, Reader, Events.Close_List, 0);
         Test_Tools.Next_And_Check (Test, Reader, Events.End_Of_Input, 0);
      end First_Read;

      Second_Write :
      declare
         Writer : File_Writers.Writer
           := File_Writers.Open (To_String (Temporary_File_Name));
      begin
         Writer.Open_List;
         Writer.Append_Atom (To_Atom ("foo"));
         Writer.Append_Atom (To_Atom ("bar"));
         Writer.Open_List;
         Writer.Close_List;
         Writer.Close_List;

         Writer.Open (To_String (Secondary_File_Name));
         Writer.Open_List;
         Writer.Append_Atom (To_Atom ("unfinished"));
      end Second_Write;

      Raw_Read :
      begin
         Test_Tools.Test_Atom
           (Test,
            To_Atom ("5:begin(()(4:head4:tail))3:end(3:foo3:bar())"),
            File_Readers.Reader (To_String (Temporary_File_Name)).Read);
         Test_Tools.Test_Atom
           (Test,
            To_Atom ("(5:first4:last)(10:unfinished"),
            File_Readers.Reader (To_String (Secondary_File_Name)).Read);
      end Raw_Read;
   exception
      when Error : others => Test.Report_Exception (Error);
   end S_Expression_IO;

end Natools.S_Expressions.File_RW_Tests;
