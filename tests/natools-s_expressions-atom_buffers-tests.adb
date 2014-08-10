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

package body Natools.S_Expressions.Atom_Buffers.Tests is

   ----------------------
   -- Whole Test Suite --
   ----------------------

   procedure All_Tests (Report : in out NT.Reporter'Class) is
   begin
      Test_Block_Append (Report);
      Test_Octet_Append (Report);
      Test_Preallocate (Report);
      Test_Query (Report);
      Test_Query_Null (Report);
      Test_Reset (Report);
      Test_Reverse_Append (Report);
      Test_Invert (Report);
      Test_Empty_Append (Report);
   end All_Tests;



   ----------------------
   -- Individual Tests --
   ----------------------

   procedure Test_Block_Append (Report : in out NT.Reporter'Class) is
      Name : constant String := "Append in blocks";
      Data : Atom (0 .. 255);
   begin
      for O in Octet loop
         Data (Count (O)) := O;
      end loop;

      declare
         Buffer : Atom_Buffer;
      begin
         Buffer.Append (Data (0 .. 10));
         Buffer.Append (Data (11 .. 11));
         Buffer.Append (Data (12 .. 101));
         Buffer.Append (Data (102 .. 101));
         Buffer.Append (Data (102 .. 255));

         Test_Tools.Test_Atom (Report, Name, Data, Buffer.Data);
      end;
   exception
      when Error : others => Report.Report_Exception (Name, Error);
   end Test_Block_Append;


   procedure Test_Invert (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Invert procedure");
      Source : Atom (1 .. 10);
      Inverse : Atom (1 .. 10);
   begin
      for I in Source'Range loop
         Source (I) := 10 + Octet (I);
         Inverse (11 - I) := Source (I);
      end loop;

      declare
         Buffer : Atom_Buffer;
      begin
         Buffer.Invert;
         Test_Tools.Test_Atom (Test, Null_Atom, Buffer.Data);

         Buffer.Append (Source (1 .. 1));
         Buffer.Invert;
         Test_Tools.Test_Atom (Test, Source (1 .. 1), Buffer.Data);

         Buffer.Append (Source (2 .. 7));
         Buffer.Invert;
         Test_Tools.Test_Atom (Test, Inverse (4 .. 10), Buffer.Data);

         Buffer.Invert;
         Buffer.Append (Source (8 .. 10));
         Buffer.Invert;
         Test_Tools.Test_Atom (Test, Inverse, Buffer.Data);
      end;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Test_Invert;


   procedure Test_Octet_Append (Report : in out NT.Reporter'Class) is
      Name : constant String := "Append octet by octet";
      Data : Atom (0 .. 255);
   begin
      for O in Octet loop
         Data (Count (O)) := O;
      end loop;

      declare
         Buffer : Atom_Buffer;
      begin
         for O in Octet loop
            Buffer.Append (O);
         end loop;

         Test_Tools.Test_Atom (Report, Name, Data, Buffer.Data);
      end;
   exception
      when Error : others => Report.Report_Exception (Name, Error);
   end Test_Octet_Append;


   procedure Test_Preallocate (Report : in out NT.Reporter'Class) is
      Name : constant String := "Preallocation of memory";
   begin
      declare
         Buffer : Atom_Buffer;
      begin
         Buffer.Preallocate (256);

         declare
            Old_Accessor : Atom_Refs.Accessor := Buffer.Raw_Query;
         begin
            for O in Octet loop
               Buffer.Append (O);
            end loop;

            Report.Item (Name,
              NT.To_Result (Old_Accessor.Data = Buffer.Raw_Query.Data));
         end;
      end;
   exception
      when Error : others => Report.Report_Exception (Name, Error);
   end Test_Preallocate;


   procedure Test_Query (Report : in out NT.Reporter'Class) is
      Name : constant String := "Accessors";
      Data : Atom (0 .. 255);
   begin
      for O in Octet loop
         Data (Count (O)) := O;
      end loop;

      declare
         Buffer : Atom_Buffer;
      begin
         Buffer.Append (Data);

         if Buffer.Length /= Data'Length then
            Report.Item (Name, NT.Fail);
            Report.Info ("Buffer.Length returned" & Count'Image (Buffer.Length)
              & ", expected" & Count'Image (Data'Length));
            return;
         end if;

         if Buffer.Data /= Data then
            Report.Item (Name, NT.Fail);
            Report.Info ("Data, returning an Atom");
            Test_Tools.Dump_Atom (Report, Buffer.Data, "Found");
            Test_Tools.Dump_Atom (Report, Data, "Expected");
            return;
         end if;

         if Buffer.Raw_Query.Data.all /= Data then
            Report.Item (Name, NT.Fail);
            Report.Info ("Raw_Query, returning an accessor");
            Test_Tools.Dump_Atom (Report, Buffer.Raw_Query.Data.all, "Found");
            Test_Tools.Dump_Atom (Report, Data, "Expected");
            return;
         end if;

         if Buffer.Element (25) /= Data (24) then
            Report.Item (Name, NT.Fail);
            Report.Info ("Element, returning an octet");
            return;
         end if;

         declare
            O, P : Octet;
         begin
            Buffer.Pop (O);
            Buffer.Pop (P);

            if O /= Data (Data'Last)
              or P /= Data (Data'Last - 1)
              or Buffer.Data /= Data (Data'First .. Data'Last - 2)
            then
               Report.Item (Name, NT.Fail);
               Report.Info ("Pop of an octet: "
                 & Octet'Image (P) & " " & Octet'Image (O));
               Test_Tools.Dump_Atom (Report, Buffer.Data, "Remaining");
               Test_Tools.Dump_Atom (Report, Data, "Expected");
               return;
            end if;

            Buffer.Append ((P, O));

            if Buffer.Data /= Data then
               Report.Item (Name, NT.Fail);
               Report.Info ("Append back after Pop");
               Test_Tools.Dump_Atom (Report, Buffer.Data, "Found");
               Test_Tools.Dump_Atom (Report, Data, "Expected");
               return;
            end if;
         end;

         declare
            Retrieved : Atom (10 .. 310);
            Length : Count;
         begin
            Buffer.Read (Retrieved, Length);
            if Length /= Data'Length
              or else Retrieved (10 .. Length + 9) /= Data
            then
               Report.Item (Name, NT.Fail);
               Report.Info ("Read into an existing buffer");
               Report.Info ("Length returned" & Count'Image (Length)
                 & ", expected" & Count'Image (Data'Length));
               Test_Tools.Dump_Atom
                 (Report, Retrieved (10 .. Length + 9), "Found");
               Test_Tools.Dump_Atom (Report, Data, "Expected");
               return;
            end if;
         end;

         declare
            Retrieved : Atom (20 .. 50);
            Length : Count;
         begin
            Buffer.Read (Retrieved, Length);
            if Length /= Data'Length or else Retrieved /= Data (0 .. 30) then
               Report.Item (Name, NT.Fail);
               Report.Info ("Read into a buffer too small");
               Report.Info ("Length returned" & Count'Image (Length)
                 & ", expected" & Count'Image (Data'Length));
               Test_Tools.Dump_Atom (Report, Retrieved, "Found");
               Test_Tools.Dump_Atom (Report, Data (0 .. 30), "Expected");
               return;
            end if;
         end;

         declare
            procedure Check (Found : in Atom);

            Result : Boolean := False;

            procedure Check (Found : in Atom) is
            begin
               Result := Found = Data;
            end Check;
         begin
            Buffer.Query (Check'Access);
            if not Result then
               Report.Item (Name, NT.Fail);
               Report.Info ("Query with callback");
               return;
            end if;
         end;
      end;

      Report.Item (Name, NT.Success);
   exception
      when Error : others => Report.Report_Exception (Name, Error);
   end Test_Query;


   procedure Test_Query_Null (Report : in out NT.Reporter'Class) is
      Name : constant String := "Accessor variants against a null buffer";
   begin
      declare
         Buffer : Atom_Buffer;
      begin
         if Buffer.Data /= Null_Atom then
            Report.Item (Name, NT.Fail);
            Report.Info ("Data, returning an Atom");
            Test_Tools.Dump_Atom (Report, Buffer.Data, "Found");
            return;
         end if;

         if Buffer.Raw_Query.Data.all /= Null_Atom then
            Report.Item (Name, NT.Fail);
            Report.Info ("Raw_Query, returning an accessor");
            Test_Tools.Dump_Atom (Report, Buffer.Raw_Query.Data.all, "Found");
            return;
         end if;

         declare
            Retrieved : Atom (1 .. 10);
            Length : Count;
         begin
            Buffer.Read (Retrieved, Length);
            if Length /= 0 then
               Report.Item (Name, NT.Fail);
               Report.Info ("Read into an existing buffer");
               Report.Info ("Length returned" & Count'Image (Length)
                 & ", expected 0");
               return;
            end if;
         end;

         declare
            procedure Check (Found : in Atom);

            Result : Boolean := False;

            procedure Check (Found : in Atom) is
            begin
               Result := Found = Null_Atom;
            end Check;
         begin
            Buffer.Query (Check'Access);
            if not Result then
               Report.Item (Name, NT.Fail);
               Report.Info ("Query with callback");
               return;
            end if;
         end;
      end;

      Report.Item (Name, NT.Success);
   exception
      when Error : others => Report.Report_Exception (Name, Error);
   end Test_Query_Null;


   procedure Test_Reset (Report : in out NT.Reporter'Class) is
      Name : constant String := "Reset procedures";
   begin
      declare
         Buffer : Atom_Buffer;
      begin
         for O in Octet loop
            Buffer.Append (O);
         end loop;

         declare
            Accessor : Atom_Refs.Accessor := Buffer.Raw_Query;
         begin
            Buffer.Soft_Reset;

            if Buffer.Length /= 0 then
               Report.Item (Name, NT.Fail);
               Report.Info ("Soft reset left length"
                 & Count'Image (Buffer.Length));
               return;
            end if;

            if Buffer.Raw_Query.Data /= Accessor.Data then
               Report.Item (Name, NT.Fail);
               Report.Info ("Soft reset changed storage area");
               return;
            end if;

            if Buffer.Raw_Query.Data.all'Length /= Buffer.Capacity then
               Report.Item (Name, NT.Fail);
               Report.Info ("Available length inconsistency, recorded"
                 & Count'Image (Buffer.Capacity) & ", actual"
                 & Count'Image (Buffer.Raw_Query.Data.all'Length));
               return;
            end if;

            if Buffer.Data'Length /= Buffer.Used then
               Report.Item (Name, NT.Fail);
               Report.Info ("Used length inconsistency, recorded"
                 & Count'Image (Buffer.Used) & ", actual"
                 & Count'Image (Buffer.Data'Length));
               return;
            end if;
         end;

         for O in Octet'(10) .. Octet'(50) loop
            Buffer.Append (O);
         end loop;

         Buffer.Hard_Reset;

         if Buffer.Length /= 0
           or else Buffer.Capacity /= 0
           or else not Buffer.Ref.Is_Empty
         then
            Report.Item (Name, NT.Fail);
            Report.Info ("Hard reset did not completely clean structure");
         end if;
      end;

      Report.Item (Name, NT.Success);
   exception
      when Error : others => Report.Report_Exception (Name, Error);
   end Test_Reset;


   procedure Test_Reverse_Append (Report : in out NT.Reporter'Class) is
      Name : constant String := "procedure Append_Reverse";
      Source_1 : Atom (1 .. 10);
      Source_2 : Atom (1 .. 1);
      Source_3 : Atom (51 .. 65);
      Source_4 : Atom (1 .. 0);
      Source_5 : Atom (101 .. 114);
      Expected : Atom (1 .. 40);
   begin
      for I in Source_1'Range loop
         Source_1 (I) := 10 + Octet (I);
         Expected (Expected'First + Source_1'Last - I) := Source_1 (I);
      end loop;

      for I in Source_2'Range loop
         Source_2 (I) := 42;
         Expected (Expected'First + Source_1'Length + Source_2'Last - I)
           := Source_2 (I);
      end loop;

      for I in Source_3'Range loop
         Source_3 (I) := Octet (I);
         Expected (Expected'First + Source_1'Length + Source_2'Length
                     + I - Source_3'First)
           := Source_3 (I);
      end loop;

      for I in Source_5'Range loop
         Source_5 (I) := Octet (I);
         Expected (Expected'First + Source_1'Length + Source_2'Length
                     + Source_3'Length + Source_5'Last - I)
           := Source_5 (I);
      end loop;

      declare
         Buffer : Atom_Buffer;
      begin
         Buffer.Append_Reverse (Source_1);
         Buffer.Append_Reverse (Source_2);
         Buffer.Append (Source_3);
         Buffer.Append_Reverse (Source_4);
         Buffer.Append_Reverse (Source_5);

         Test_Tools.Test_Atom (Report, Name, Expected, Buffer.Data);
      end;
   exception
      when Error : others => Report.Report_Exception (Name, Error);
   end Test_Reverse_Append;


   procedure Test_Empty_Append (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Empty append on empty buffer");
   begin
      declare
         Buffer : Atom_Buffer;
      begin
         Buffer.Append (Null_Atom);
         Test_Tools.Test_Atom (Test, Null_Atom, Buffer.Data);
      end;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Test_Empty_Append;

end Natools.S_Expressions.Atom_Buffers.Tests;
