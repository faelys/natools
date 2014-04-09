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

------------------------------------------------------------------------------
-- Natools.S_Expressions.File_Readers provides types that read into files   --
-- on disk (using Stream_IO) and expose their data as S-expressions or as   --
-- atoms.                                                                   --
------------------------------------------------------------------------------

with Natools.S_Expressions.Atom_Buffers;
with Natools.S_Expressions.Lockable;

private with Ada.Finalization;
private with Ada.Streams.Stream_IO;
private with Natools.S_Expressions.Parsers;

package Natools.S_Expressions.File_Readers is

   type S_Reader is limited new Lockable.Descriptor with private;
      --  Make a disk file available as a S-expression

   function Reader (Name : String) return S_Reader;
      --  Create a S_Reader associated with file Name

   procedure Set_Filename (Object : in out S_Reader; Name : in String);
      --  Reset Object with the new file Name

   procedure Rewind (Object : in out S_Reader);
      --  Return to the beginning of the file


   type Atom_Reader is tagged limited private;
      --  Make a disk file available as a single atom

   function Reader (Name : String) return Atom_Reader;
      --  Create an Atom_Reader associated with file Name;

   procedure Set_Filename (Object : in out Atom_Reader; Name : in String);
      --  Reset Object with the new file Name

   function Length (Object : Atom_Reader) return Count;
      --  Return the file length

   function Read (Object : Atom_Reader) return Atom;
      --  Read the whole file and return it as a single atom

   procedure Read
     (Object : in Atom_Reader;
      Data : out Atom;
      Length : out Count);
      --  Read the whole file and store it in Data, along with its Length.
      --  If Data is too small, only the file prefix is read.

   procedure Read
     (Object : in Atom_Reader;
      Buffer : out Atom_Buffers.Atom_Buffer;
      Block_Size : in Count := 1024);
      --  Read the whole file into Buffer

   generic
      type Atom_Access is access Atom;
      with procedure Unchecked_Deallocation (X : in out Atom_Access);
   procedure Query
     (Object : in Atom_Reader;
      Process : not null access procedure (Data : in Atom));
      --  Read the whole file into a memory zone allocated through Atom_Access,
      --  and call Process on it.

   procedure Block_Query
     (Object : in Atom_Reader;
      Block_Size : in Count;
      Process : not null access procedure (Block : in Atom));
      --  Read file block by block, and call Process on each of them

private

   type Autoclose is new Ada.Finalization.Limited_Controlled with record
      File : Ada.Streams.Stream_IO.File_Type;
   end record;

   overriding procedure Finalize (Object : in out Autoclose);
      --  Close the underlying file if it was opened.
      --  Used because a compiler bug in GNAT prevents adding
      --  Ada.Finalization.Limited_Controlled as an ancestor of Parser,
      --  so close-on-finalization has to be provided through composition.

   type S_Reader is limited new Parsers.Parser with record
      Holder : Autoclose;
   end record;

   overriding procedure Read_More
     (Object : in out S_Reader;
      Buffer : out Atom_Buffers.Atom_Buffer);


   type Atom_Reader is new Ada.Finalization.Limited_Controlled with record
      File : Ada.Streams.Stream_IO.File_Type;
   end record;

   overriding procedure Finalize (Object : in out Atom_Reader);

end Natools.S_Expressions.File_Readers;
