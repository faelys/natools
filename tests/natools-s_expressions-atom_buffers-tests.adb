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

            if Buffer.Raw_Query.Data.all'Length /= Buffer.Available then
               Report.Item (Name, NT.Fail);
               Report.Info ("Available length inconsistency, recorded"
                 & Count'Image (Buffer.Available) & ", actual"
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
           or else Buffer.Available /= 0
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

end Natools.S_Expressions.Atom_Buffers.Tests;

