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

package body Natools.S_Expressions.File_Readers is

   package Stream_IO renames Ada.Streams.Stream_IO;


   overriding procedure Finalize (Object : in out Autoclose) is
   begin
      if Stream_IO.Is_Open (Object.File) then
         Stream_IO.Close (Object.File);
      end if;
   end Finalize;


   -------------------------
   -- S-Expression Reader --
   -------------------------

   function Reader (Name : String) return S_Reader is
   begin
      return Object : S_Reader do
         Stream_IO.Open (Object.Holder.File, Stream_IO.In_File, Name);
         Object.Next;
      end return;
   end Reader;


   procedure Set_Filename (Object : in out S_Reader; Name : in String) is
   begin
      if Stream_IO.Is_Open (Object.Holder.File) then
         Stream_IO.Close (Object.Holder.File);
      end if;

      Stream_IO.Open (Object.Holder.File, Stream_IO.In_File, Name);
      Object.Rewind;
   end Set_Filename;


   procedure Rewind (Object : in out S_Reader) is
   begin
      Stream_IO.Set_Index (Object.Holder.File, 1);
      Object.Reset;
      Object.Next;
   end Rewind;


   overriding procedure Read_More
     (Object : in out S_Reader;
      Buffer : out Atom_Buffers.Atom_Buffer)
   is
      Data : Ada.Streams.Stream_Element_Array (0 .. 127);
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      Stream_IO.Read (Object.Holder.File, Data, Last);

      if Last in Data'Range then
         Buffer.Append (Data (Data'First .. Last));
      end if;
   end Read_More;



   -----------------
   -- Atom Reader --
   -----------------

   function Reader (Name : String) return Atom_Reader is
   begin
      return Object : Atom_Reader do
         Stream_IO.Open (Object.File, Stream_IO.In_File, Name);
      end return;
   end Reader;


   procedure Set_Filename (Object : in out Atom_Reader; Name : in String) is
   begin
      if Stream_IO.Is_Open (Object.File) then
         Stream_IO.Close (Object.File);
      end if;

      Stream_IO.Open (Object.File, Stream_IO.In_File, Name);
   end Set_Filename;


   function Length (Object : Atom_Reader) return Count is
   begin
      return Count (Stream_IO.Size (Object.File));
   end Length;


   function Read (Object : Atom_Reader) return Atom is
      Result : Atom (1 .. Object.Length);
      Last : Count;
   begin
      Stream_IO.Set_Index (Object.File, 1);
      Stream_IO.Read (Object.File, Result, Last);
      pragma Assert (Last = Result'Last);
      return Result;
   end Read;


   procedure Read
     (Object : in Atom_Reader;
      Data : out Atom;
      Length : out Count) is
   begin
      Stream_IO.Set_Index (Object.File, 1);
      Stream_IO.Read (Object.File, Data, Length);
      Length := Object.Length;
   end Read;


   procedure Read
     (Object : in Atom_Reader;
      Buffer : out Atom_Buffers.Atom_Buffer;
      Block_Size : in Count := 1024)
   is
      Block : Atom (1 .. Block_Size);
      Last : Count;
   begin
      Buffer.Soft_Reset;
      Stream_IO.Set_Index (Object.File, 1);
      loop
         Stream_IO.Read (Object.File, Block, Last);
         exit when Last not in Block'Range;
         Buffer.Append (Block (Block'First .. Last));
      end loop;
   end Read;


   procedure Query
     (Object : in Atom_Reader;
      Process : not null access procedure (Data : in Atom))
   is
      Buffer : Atom_Access := null;
      Last : Count;
   begin
      Buffer := new Atom (1 .. Object.Length);
      Stream_IO.Set_Index (Object.File, 1);
      Stream_IO.Read (Object.File, Buffer.all, Last);
      pragma Assert (Last = Buffer'Last);
      Process (Buffer.all);
      Unchecked_Deallocation (Buffer);
   exception
      when others =>
         Unchecked_Deallocation (Buffer);
         raise;
   end Query;


   procedure Block_Query
     (Object : in Atom_Reader;
      Block_Size : in Count;
      Process : not null access procedure (Block : in Atom))
   is
      Block : Atom (1 .. Block_Size);
      Last : Count;
   begin
      Stream_IO.Set_Index (Object.File, 1);
      loop
         Stream_IO.Read (Object.File, Block, Last);
         exit when Last not in Block'Range;
         Process.all (Block (Block'First .. Last));
      end loop;
   end Block_Query;


   overriding procedure Finalize (Object : in out Atom_Reader) is
   begin
      if Stream_IO.Is_Open (Object.File) then
         Stream_IO.Close (Object.File);
      end if;
   end Finalize;

end Natools.S_Expressions.File_Readers;
