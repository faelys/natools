------------------------------------------------------------------------------
-- Copyright (c) 2011, Natacha Porté                                        --
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

with Natools.Chunked_Strings.Tests.CXA4010;
with Natools.Chunked_Strings.Tests.CXA4011;
with Natools.Chunked_Strings.Tests.CXA4030;
with Natools.Chunked_Strings.Tests.CXA4031;
with Natools.Chunked_Strings.Tests.CXA4032;
with Natools.Accumulators.Tests;

package body Natools.Chunked_Strings.Tests is
   package NT renames Natools.Tests;

   procedure All_Blackbox_Tests (Report : in out Natools.Tests.Reporter'Class)
   is
      procedure Test_CXA4010 is new CXA4010;
      procedure Test_CXA4011 is new CXA4011;
      procedure Test_CXA4030 is new CXA4030;
      procedure Test_CXA4031 is new CXA4031;
      procedure Test_CXA4032 is new CXA4032;
   begin
      NT.Section (Report, "Blackbox tests of Chunked_Strings");
      Test_CXA4010 (Report);
      Test_CXA4011 (Report);
      Test_CXA4030 (Report);
      Test_CXA4031 (Report);
      Test_CXA4032 (Report);

      NT.Section (Report, "String_Accumulator interface");
      declare
         Acc : Chunked_String;
      begin
         Accumulators.Tests.Test (Report, Acc);
      end;
      NT.End_Section (Report);
      NT.End_Section (Report);
   end All_Blackbox_Tests;


   procedure All_Tests (Report : in out Natools.Tests.Reporter'Class) is
   begin
      NT.Section (Report, "All tests of Chunked_Strings");
      All_Blackbox_Tests (Report);
      NT.End_Section (Report);
   end All_Tests;



   procedure Dump (Report : in out Natools.Tests.Reporter'Class;
                   Dumped : in     Chunked_String)
   is
      package Maps renames Ada.Strings.Maps;
      use type Maps.Character_Set;

      procedure Print_Chunk (Index : Positive; Chunk : String_Access);
      procedure Print_Chunks (Data : Chunk_Array_Access);
      procedure Print_Line (Raw : String);

      Printable : constant Maps.Character_Set
        := Maps.To_Set (Maps.Character_Ranges'((Low => 'a', High => 'z'),
                                               (Low => 'A', High => 'Z'),
                                               (Low => '0', High => '9')))
        or Maps.To_Set (" -_");
      Non_Printable : constant Character := '.';

      procedure Print_Chunk (Index : Positive; Chunk : String_Access) is
         I : Natural;
      begin
         if Chunk = null then
            NT.Info (Report, "Chunk" & Positive'Image (Index) & ": null");
         else
            NT.Info (Report, "Chunk" & Positive'Image (Index) & ": "
                             & Natural'Image (Chunk.all'First) & " .."
                             & Natural'Image (Chunk.all'Last));
            I := Chunk.all'First;
            while I <= Chunk.all'Last loop
               Print_Line
                 (Chunk.all (I .. Positive'Min (Chunk.all'Last, I + 16)));
               I := I + 16;
            end loop;
         end if;
      end Print_Chunk;

      procedure Print_Chunks (Data : Chunk_Array_Access) is
      begin
         if Data = null then
            NT.Info (Report, "Null data");
         end if;
         if Data.all'Length = 0 then
            NT.Info (Report, "Empty data");
         end if;
         for C in Data.all'Range loop
            Print_Chunk (C, Data.all (C));
         end loop;
      end Print_Chunks;

      procedure Print_Line (Raw : String) is
         Hex  : constant String := "0123456789ABCDEF";
         Line : String (1 .. 4 * Raw'Length + 2) := (others => ' ');
      begin
         for I in Raw'Range loop
            declare
               Pos      : constant Natural := Character'Pos (Raw (I));
               High     : constant Natural := (Pos - 1) / 16;
               Low      : constant Natural := (Pos - 1) mod 16;
               Hex_Base : constant Positive
                 := Line'First + 3 * (I - Raw'First);
               Raw_Base : constant Positive
                 := Line'First + 3 * Raw'Length + 2 + (I - Raw'First);
            begin
               Line (Hex_Base) := Hex (Hex'First + High);
               Line (Hex_Base + 1) := Hex (Hex'First + Low);
               if Maps.Is_In (Raw (I), Printable) then
                  Line (Raw_Base) := Raw (I);
               else
                  Line (Raw_Base) := Non_Printable;
               end if;
            end;
         end loop;
         NT.Info (Report, Line);
      end Print_Line;
   begin
      NT.Info (Report, "Chunk_Size " & Positive'Image (Dumped.Chunk_Size)
                       & " (default" & Positive'Image (Default_Chunk_Size)
                       & ')');
      NT.Info (Report, "Allocation_Unit "
                       & Positive'Image (Dumped.Allocation_Unit)
                       & " (default" & Positive'Image (Default_Allocation_Unit)
                       & ')');
      NT.Info (Report, "Size " & Natural'Image (Dumped.Size));
      Print_Chunks (Dumped.Data);
   end Dump;


   procedure Test (Report    : in out Natools.Tests.Reporter'Class;
                   Test_Name : in     String;
                   Computed  : in     Chunked_String;
                   Reference : in     String) is
   begin
      if not Is_Valid (Computed) then
         NT.Item (Report, Test_Name, NT.Error);
         return;
      end if;
      if Computed = To_Chunked_String (Reference) then
         NT.Item (Report, Test_Name, NT.Success);
      else
         NT.Item (Report, Test_Name, NT.Fail);
         NT.Info (Report, "Computed  """ & To_String (Computed) & '"');
         NT.Info (Report, "Reference """ & Reference & '"');
      end if;
   end Test;


   procedure Test (Report    : in out Natools.Tests.Reporter'Class;
                   Test_Name : in     String;
                   Computed  : in     Chunked_String;
                   Reference : in     Chunked_String) is
   begin
      if not Is_Valid (Computed) then
         NT.Item (Report, Test_Name, NT.Error);
         return;
      end if;
      if not Is_Valid (Reference) then
         NT.Item (Report, Test_Name, NT.Error);
         return;
      end if;
      if Computed = Reference then
         NT.Item (Report, Test_Name, NT.Success);
      else
         NT.Item (Report, Test_Name, NT.Fail);
         NT.Info (Report, "Computed  """ & To_String (Computed) & '"');
         NT.Info (Report, "Reference """ & To_String (Reference) & '"');
      end if;
   end Test;


   procedure Test (Report    : in out Natools.Tests.Reporter'Class;
                   Test_Name : in     String;
                   Computed  : in     Natural;
                   Reference : in     Natural) is
   begin
      if Computed = Reference then
         NT.Item (Report, Test_Name, NT.Success);
      else
         NT.Item (Report, Test_Name, NT.Fail);
         NT.Info (Report, "Computed" & Natural'Image (Computed)
                          & ", expected" & Natural'Image (Reference));
      end if;
   end Test;

end Natools.Chunked_Strings.Tests;
