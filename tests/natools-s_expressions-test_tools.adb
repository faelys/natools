------------------------------------------------------------------------------
-- Copyright (c) 2013, Natacha Porté                                        --
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

package body Natools.S_Expressions.Test_Tools is

   Hex_Digits : constant String := "0123456789ABCDEF";

   function Encode_Hex (Value : Offset; Length : Positive) return String;
   function Hex_Slice
     (Address : Offset;
      Address_Length : Positive;
      Data : Atom;
      Width : Positive)
     return String;

   function Is_Printable (Data : Octet) return Boolean;
   function Is_Printable (Data : Atom) return Boolean;
      --  Return whether Data can be dumped directed as a String or Character


   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

   function Encode_Hex (Value : Offset; Length : Positive) return String is
      I : Natural := Length;
      Digit : Natural;
      Current : Offset := Value;
   begin
      return Result : String (1 .. Length) := (others => '0') do
         while Current /= 0 and I /= 0 loop
            Digit := Natural (Current mod 16);
            Result (I) := Hex_Digits (Hex_Digits'First + Digit);
            I := I - 1;
            Current := Current / 16;
         end loop;
      end return;
   end Encode_Hex;


   function Hex_Slice
     (Address : Offset;
      Address_Length : Positive;
      Data : Atom;
      Width : Positive)
     return String
   is
      Total_Length : constant Positive
        := Address_Length + 4 + 4 * Width;
      Hex_Start : constant Positive := Address_Length + 2;
      Raw_Start : constant Positive := Hex_Start + 3 * Width + 1;
      Digit : Octet;
   begin
      return Result : String (1 .. Total_Length) := (others => ' ') do
         Result (1 .. Address_Length) := Encode_Hex (Address, Address_Length);

         for I in 0 .. Width - 1 loop
            exit when Data'First + Offset (I) not in Data'Range;

            Digit := Data (Data'First + Offset (I));

            Result (Hex_Start + 3 * I) := Hex_Digits (Hex_Digits'First
              + Natural (Digit / 16));
            Result (Hex_Start + 3 * I + 1) := Hex_Digits (Hex_Digits'First
              + Natural (Digit mod 16));

            if Is_Printable (Digit) then
               Result (Raw_Start + I) := Character'Val (Digit);
            else
               Result (Raw_Start + I) := '.';
            end if;
         end loop;
      end return;
   end Hex_Slice;


   function Is_Printable (Data : Octet) return Boolean is
   begin
      return Data in 32 .. 127;
   end Is_Printable;


   function Is_Printable (Data : Atom) return Boolean is
   begin
      if Data'Length > 100 then
         return False;
      end if;

      for I in Data'Range loop
         if not Is_Printable (Data (I)) then
            return False;
         end if;
      end loop;

      return True;
   end Is_Printable;



   ------------------
   -- Public Tools --
   ------------------

   procedure Dump_Atom
     (Report : in out NT.Reporter'Class;
      Data : in Atom;
      Label : in String := "")
   is
      I, Length : Offset := 0;
   begin
      if Is_Printable (Data) then
         if Label'Length > 0 then
            Report.Info (Label & ": """ & To_String (Data) & '"');
         else
            Report.Info ('"' & To_String (Data) & '"');
         end if;
      else
         if Label'Length > 0 then
            Report.Info
              (Label & ":" & Natural'Image (Data'Length) & " octets");
         end if;

         while I < Data'Length loop
            Length := Offset'Min (16, Data'Length - I);
            Report.Info (Hex_Slice
              (I, 8,
               Data (Data'First + I .. Data'First + I + Length - 1), 16));
            I := I + 16;
         end loop;
      end if;
   end Dump_Atom;


   procedure Dump_Atom  --  Cut and pasted code because generics crash gnat
     (Test : in out NT.Test;
      Data : in Atom;
      Label : in String := "")
   is
      I, Length : Offset := 0;
   begin
      if Is_Printable (Data) then
         if Label'Length > 0 then
            Test.Info (Label & ": """ & To_String (Data) & '"');
         else
            Test.Info ('"' & To_String (Data) & '"');
         end if;
      else
         if Label'Length > 0 then
            Test.Info
              (Label & ":" & Natural'Image (Data'Length) & " octets");
         end if;

         while I < Data'Length loop
            Length := Offset'Min (16, Data'Length - I);
            Test.Info (Hex_Slice
              (I, 8,
               Data (Data'First + I .. Data'First + I + Length - 1), 16));
            I := I + 16;
         end loop;
      end if;
   end Dump_Atom;


   procedure Test_Atom
     (Report : in out NT.Reporter'Class;
      Test_Name : in String;
      Expected : in Atom;
      Found : in Atom) is
   begin
      if Found = Expected then
         Report.Item (Test_Name, NT.Success);
      else
         Report.Item (Test_Name, NT.Fail);
         Dump_Atom (Report, Found, "Found");
         Dump_Atom (Report, Expected, "Expected");
      end if;
   end Test_Atom;


   procedure Test_Atom
     (Test : in out NT.Test;
      Expected : in Atom;
      Found : in Atom) is
   begin
      if Found /= Expected then
         Test.Fail;
         Dump_Atom (Test, Found, "Found");
         Dump_Atom (Test, Expected, "Expected");
      end if;
   end Test_Atom;


   procedure Test_Atom_Accessors
     (Test : in out NT.Test;
      Tested : in Descriptor'Class;
      Expected : in Atom;
      Expected_Level : in Integer := -1)
   is
      Print_Expected : Boolean := False;
   begin
      if Tested.Current_Event /= Events.Add_Atom then
         Test.Error ("Test_Atom_Accessors called with current event "
           & Events.Event'Image (Tested.Current_Event));
         return;
      end if;

      if Expected_Level >= 0 then
         Current_Level_Test :
         declare
            Level : constant Natural := Tested.Current_Level;
         begin
            if Level /= Expected_Level then
               Test.Fail ("Current_Level is"
                 & Integer'Image (Level)
                 & ", expected"
                 & Integer'Image (Expected_Level));
            end if;
         end Current_Level_Test;
      end if;

      Current_Atom_Test :
      declare
         Current_Atom : constant Atom := Tested.Current_Atom;
      begin
         if Current_Atom /= Expected then
            Print_Expected := True;
            Test.Fail;
            Dump_Atom (Test, Current_Atom, "Current_Atom");
         end if;
      end Current_Atom_Test;

      Query_Atom_Test :
      declare
         procedure Process (Data : in Atom);

         Calls : Natural := 0;
         Buffer : Atom_Buffers.Atom_Buffer;

         procedure Process (Data : in Atom) is
         begin
            Calls := Calls + 1;
            Buffer.Append (Data);
         end Process;
      begin
         Tested.Query_Atom (Process'Access);

         if Calls = 0 then
            Test.Fail ("Query_Atom did not call Process");
         elsif Calls > 1 then
            Test.Fail ("Query_Atom called Process" & Integer'Image (Calls)
              & " times");
            Print_Expected := True;
            Dump_Atom (Test, Buffer.Data, "Buffer");
         elsif Buffer.Data /= Expected then
            Print_Expected := True;
            Test.Fail;
            Dump_Atom (Test, Buffer.Data, "Query_Atom");
         end if;
      end Query_Atom_Test;

      Long_Read_Atom_Test :
      declare
         Buffer : Atom (21 .. Expected'Length + 30);
         Length : Count;
      begin
         Tested.Read_Atom (Buffer, Length);

         if Buffer (Buffer'First .. Buffer'First + Length - 1) /= Expected then
            Print_Expected := True;
            Test.Fail;
            Dump_Atom
              (Test,
               Buffer (Buffer'First .. Buffer'First + Length - 1),
               "Read_Atom");
         end if;
      end Long_Read_Atom_Test;

      Short_Read_Atom_Test :
      declare
         Buffer : Atom (11 .. Expected'Length / 2 + 10);
         Length : Count;
      begin
         Tested.Read_Atom (Buffer, Length);

         if Expected (Expected'First .. Expected'First + Buffer'Length - 1)
           /= Buffer
         then
            Print_Expected := True;
            Test.Fail;
            Dump_Atom (Test, Buffer, "Short Read_Atom");
         end if;
      end Short_Read_Atom_Test;

      if Print_Expected then
         Dump_Atom (Test, Expected, "Expected");
      end if;
   end Test_Atom_Accessors;


   procedure Test_Atom_Accessor_Exceptions
     (Test : in out NT.Test;
      Tested : in Descriptor'Class) is
   begin
      if Tested.Current_Event = Events.Add_Atom then
         Test.Error ("Test_Atom_Accessor_Exceptions during Events.Add_Atom");
         return;
      end if;

      Current_Atom_Test :
      begin
         declare
            Data : constant Atom := Tested.Current_Atom;
         begin
            Test.Fail ("No exception raised in Current_Atom");
            Dump_Atom (Test, Data, "Returned value");
         end;
      exception
         when Program_Error => null;
         when Error : others =>
            Test.Fail ("Wrong exception raised in Current_Atom");
            Test.Report_Exception (Error, NT.Fail);
      end Current_Atom_Test;

      Query_Atom_Test :
      declare
         procedure Process (Data : in Atom);

         Calls : Natural := 0;
         Buffer : Atom_Buffers.Atom_Buffer;

         procedure Process (Data : in Atom) is
         begin
            Calls := Calls + 1;
            Buffer.Append (Data);
         end Process;
      begin
         Tested.Query_Atom (Process'Access);

         Test.Fail ("No exception raised in Query_Atom");
         Dump_Atom (Test, Buffer.Data,
           "Buffer from" & Natural'Image (Calls) & " calls");
      exception
         when Program_Error => null;
         when Error : others =>
            Test.Fail ("Wrong exception raised in Query_Atom");
            Test.Report_Exception (Error, NT.Fail);
      end Query_Atom_Test;

      Read_Atom_Test :
      declare
         Buffer : Atom (0 .. 31) := (others => 46);
         Length : Count;
      begin
         Tested.Read_Atom (Buffer, Length);

         Test.Fail ("No exception raised in Read_Atom");
         Test.Info ("Returned Length:" & Count'Image (Length));
         Dump_Atom (Test, Buffer, "Output Buffer");
      exception
         when Program_Error => null;
         when Error : others =>
            Test.Fail ("Wrong exception raised in Read_Atom");
            Test.Report_Exception (Error, NT.Fail);
      end Read_Atom_Test;
   end Test_Atom_Accessor_Exceptions;



   -------------------
   -- Memory Stream --
   -------------------

   overriding procedure Read
     (Stream : in out Memory_Stream;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset) is
   begin
      Last := Item'First - 1;

      while Last + 1 in Item'Range
        and then Stream.Read_Pointer < Stream.Internal.Length
      loop
         Stream.Read_Pointer := Stream.Read_Pointer + 1;
         Last := Last + 1;
         Item (Last) := Stream.Internal.Element (Stream.Read_Pointer);
      end loop;
   end Read;


   overriding procedure Write
     (Stream : in out Memory_Stream;
      Item : in Ada.Streams.Stream_Element_Array) is
   begin
      if Stream.Read_Pointer >= Stream.Internal.Length then
         Stream.Internal.Soft_Reset;
         Stream.Read_Pointer := 0;
      end if;

      Stream.Internal.Append (Item);

      if not Stream.Mismatch then
         for I in Item'Range loop
            if Stream.Expect_Pointer + 1 > Stream.Expected.Length
              or else Stream.Expected.Element (Stream.Expect_Pointer + 1)
                /= Item (I)
            then
               Stream.Mismatch := True;
               exit;
            end if;

            Stream.Expect_Pointer := Stream.Expect_Pointer + 1;
         end loop;
      end if;
   end Write;


   function Get_Data (Stream : Memory_Stream) return Atom is
   begin
      return Stream.Internal.Data;
   end Get_Data;


   function Unread_Data (Stream : Memory_Stream) return Atom is
   begin
      if Stream.Read_Pointer < Stream.Internal.Length then
         return Stream.Internal.Raw_Query.Data.all
           (Stream.Read_Pointer + 1 .. Stream.Internal.Length);
      else
         return Null_Atom;
      end if;
   end Unread_Data;


   procedure Set_Data
     (Stream : in out Memory_Stream;
      Data : in Atom) is
   begin
      Stream.Internal.Soft_Reset;
      Stream.Internal.Append (Data);
   end Set_Data;


   function Unread_Expected (Stream : Memory_Stream) return Atom is
   begin
      if Stream.Expect_Pointer < Stream.Expected.Length then
         return Stream.Expected.Raw_Query.Data.all
           (Stream.Expect_Pointer + 1 .. Stream.Expected.Length);
      else
         return Null_Atom;
      end if;
   end Unread_Expected;


   procedure Set_Expected
     (Stream : in out Memory_Stream;
      Data : in Atom;
      Reset_Mismatch : in Boolean := True) is
   begin
      Stream.Expected.Soft_Reset;
      Stream.Expected.Append (Data);
      Stream.Expect_Pointer := 0;
      if Reset_Mismatch then
         Stream.Mismatch := False;
      end if;
   end Set_Expected;


   function Has_Mismatch (Stream : Memory_Stream) return Boolean is
   begin
      return Stream.Mismatch;
   end Has_Mismatch;


   procedure Reset_Mismatch (Stream : in out Memory_Stream) is
   begin
      Stream.Mismatch := False;
   end Reset_Mismatch;


   function Mismatch_Index (Stream : Memory_Stream) return Count is
   begin
      if Stream.Mismatch then
         return Stream.Expect_Pointer + 1;
      else
         return 0;
      end if;
   end Mismatch_Index;


   procedure Check_Stream
     (Stream : in Test_Tools.Memory_Stream;
      Test : in out NT.Test) is
   begin
      if Stream.Has_Mismatch or else Stream.Unread_Expected /= Null_Atom then
         if Stream.Has_Mismatch then
            Test.Fail ("Mismatch at position"
              & Count'Image (Stream.Mismatch_Index));

            declare
               Stream_Data : Atom renames Stream.Get_Data;
            begin
               Test_Tools.Dump_Atom
                 (Test,
                  Stream_Data (Stream_Data'First .. Stream.Mismatch_Index - 1),
                  "Matching data");
               Test_Tools.Dump_Atom
                 (Test,
                  Stream_Data (Stream.Mismatch_Index .. Stream_Data'Last),
                  "Mismatching data");
            end;
         end if;

         if Stream.Unread_Expected /= Null_Atom then
            Test.Fail;
            Test_Tools.Dump_Atom
              (Test,
               Stream.Unread_Expected,
               "Left to expect");
         end if;
      end if;
   end Check_Stream;

end Natools.S_Expressions.Test_Tools;
