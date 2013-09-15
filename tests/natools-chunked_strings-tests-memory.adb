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

procedure Natools.Chunked_Strings.Tests.Memory
  (Report : in out Natools.Tests.Reporter'Class)
is
   function Allocated_Size (Source : in Chunked_String) return Natural;
      --  Return the number of allocated characters in Source


   function Allocated_Size (Source : in Chunked_String) return Natural is
   begin
      if Source.Data = null or else Source.Data'Last < 1 then
         return 0;
      end if;

      return (Source.Data'Last - 1) * Source.Chunk_Size
        + Source.Data (Source.Data'Last)'Last;
   end Allocated_Size;

   package NT renames Natools.Tests;
begin
   NT.Section (Report, "Extra tests for memory usage");

   declare
      Name : constant String := "Procedure Preallocate";
      CS : Chunked_String;
      Memory_Ref : Natural;
      Repeats : constant Positive := 50;
   begin
      Preallocate (CS, Repeats * Name'Length);
      Memory_Ref := Allocated_Size (CS);

      for I in 1 .. Repeats loop
         Append (CS, Name);
      end loop;

      if Memory_Ref /= Allocated_Size (CS) then
         NT.Item (Report, Name, NT.Fail);
         NT.Info (Report, "Memory after preallocation:"
           & Natural'Image (Memory_Ref));
         NT.Info (Report, "Memory after insertions:"
           & Natural'Image (Allocated_Size (CS)));
      else
         NT.Item (Report, Name, NT.Success);
      end if;
   exception
      when Error : others => NT.Report_Exception (Report, Name, Error);
   end;

   declare
      Name : constant String := "Procedure Free_Extra_Memory";
      CS : Chunked_String;
      Memory_Ref : Natural;
      Repeats : constant Positive := 50;
   begin
      Preallocate (CS, Repeats * Name'Length);
      Memory_Ref := Allocated_Size (CS);
      Free_Extra_Memory (CS);

      if Memory_Ref <= Allocated_Size (CS) then
         NT.Item (Report, Name, NT.Fail);
         NT.Info (Report, "Memory before:"
           & Natural'Image (Memory_Ref));
         NT.Info (Report, "Memory after:"
           &  Natural'Image (Allocated_Size (CS)));
      else
         NT.Item (Report, Name, NT.Success);
      end if;
   exception
      when Error : others => NT.Report_Exception (Report, Name, Error);
   end;

   Natools.Tests.End_Section (Report);
end Natools.Chunked_Strings.Tests.Memory;
