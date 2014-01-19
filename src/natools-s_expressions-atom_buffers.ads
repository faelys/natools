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
-- Natools.S_Expressions.Atom_Buffers implements an unbounded Atom designed --
-- to be used as an input buffer, accumulating data and extracting it as a  --
-- single Atom object.                                                      --
-- It also provides an individual Octet accessor, used in parser internal   --
-- recursive buffer.                                                        --
------------------------------------------------------------------------------

with Natools.References;

package Natools.S_Expressions.Atom_Buffers is
   pragma Preelaborate (Atom_Buffers);

   type Dummy_Access is access Boolean;
      --  Used to access default storage pool

   package Atom_Refs is new Natools.References
     (Atom, Dummy_Access'Storage_Pool, Dummy_Access'Storage_Pool);

   type Atom_Buffer is tagged private;

   procedure Preallocate (Buffer : in out Atom_Buffer; Length : in Count);
      --  Preallocate enough memory to append Length octets without
      --  any further allocation.

   procedure Append (Buffer : in out Atom_Buffer; Data : in Atom);
   procedure Append (Buffer : in out Atom_Buffer; Data : in Octet);
      --  Append Data after the end of Buffer

   procedure Append_Reverse (Buffer : in out Atom_Buffer; Data : in Atom);
      --  Append bytes from Atom from last to first

   function Length (Buffer : Atom_Buffer) return Count;
   function Data (Buffer : Atom_Buffer) return Atom;
   procedure Query
     (Buffer : in Atom_Buffer;
      Process : not null access procedure (Data : in Atom));
   procedure Read
     (Buffer : in Atom_Buffer;
      Data : out Atom;
      Length : out Count);
   function Element (Buffer : Atom_Buffer; Position : Count) return Octet;
      --  Accessors to the whole buffer as an Atom

   procedure Pop (Buffer : in out Atom_Buffer; Data : out Octet);
      --  Remove last octet from Buffer and store it in Data

   function Raw_Query (Buffer : Atom_Buffer) return Atom_Refs.Accessor;
      --  Accessor to the whole allocated memory

   procedure Hard_Reset (Buffer : in out Atom_Buffer);
      --  Clear buffer and release internal memory

   procedure Soft_Reset (Buffer : in out Atom_Buffer);
      --  Clear buffer keeping internal memory

private

   type Atom_Buffer is tagged record
      Ref : Atom_Refs.Reference;
      Available, Used : Count := 0;
   end record;

end Natools.S_Expressions.Atom_Buffers;
