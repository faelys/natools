------------------------------------------------------------------------------
-- Copyright (c) 2013-2014, Natacha Porté                                   --
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
-- Natools.S_Expressions.Parsers implements an event-based S-expression     --
-- parser that reads from an internal cache updated through a dispatching   --
-- call.                                                                    --
--                                                                          --
-- Stream_Parser is a basic concrete parser that fills the internal cache   --
-- from a stream provided as a discriminant. It functionally replaces the   --
-- former Subparser type.                                                   --
------------------------------------------------------------------------------

with Ada.Streams;

with Natools.S_Expressions.Atom_Buffers;
with Natools.S_Expressions.Lockable;

package Natools.S_Expressions.Parsers is
   pragma Preelaborate (Natools.S_Expressions.Parsers);

   type Parser is abstract limited new Lockable.Descriptor with private;
   pragma Preelaborable_Initialization (Parser);

   procedure Read_More
     (Self : in out Parser;
      Buffer : out Atom_Buffers.Atom_Buffer)
     is abstract;
      --  Read data to be parsed.
      --  Leaving the buffer empty signals end of input stream.

   procedure Reset (Self : in out Parser; Hard : in Boolean := False);
      --  Reset internal state, and free internal memory if Hard

   overriding function Current_Event (Self : Parser) return Events.Event;
   overriding function Current_Atom (Self : Parser) return Atom;
   overriding function Current_Level (Self : Parser) return Natural;

   overriding procedure Query_Atom
     (Self : in Parser;
      Process : not null access procedure (Data : in Atom));

   overriding procedure Read_Atom
     (Self : in Parser;
      Data : out Atom;
      Length : out Count);

   overriding procedure Next (Self : in out Parser; Event : out Events.Event);

   overriding procedure Lock
     (Self : in out Parser;
      State : out Lockable.Lock_State);

   overriding procedure Unlock
     (Self : in out Parser;
      State : in out Lockable.Lock_State;
      Finish : in Boolean := True);



   type Stream_Parser (Input : access Ada.Streams.Root_Stream_Type'Class) is
     limited new Lockable.Descriptor with private;
   pragma Preelaborable_Initialization (Stream_Parser);



   type Memory_Parser (<>) is limited new Lockable.Descriptor with private;
   pragma Preelaborable_Initialization (Memory_Parser);

   not overriding function Create
     (Data : in Ada.Streams.Stream_Element_Array)
     return Memory_Parser;

   not overriding function Create_From_String
     (Data : in String) return Memory_Parser;

private

   type Internal_State is
     (Waiting,          --  waiting for a marker
      Base64_Atom,      --  reading an atom encoded in base 64
      Base64_Expr,      --  reading an expression encoded in base 64
      Hex_Atom,         --  reading an atom encoded in hexadecimal
      Number,           --  reading a number that can either be a verbatim
                        --    length prefix or an extended token
      Quoted_Atom,      --  reading an atom encoded in a C-like quoted string
      Token,            --  reading a token atom
      Verbatim_Atom);   --  reading a verbatim atom

   subtype Read_Buffer_Count is Count range 0 .. 4;

   type Read_Buffer is record
      Data   : Atom (0 .. 3);
      Length : Read_Buffer_Count;
   end record;

   type State_Data (State : Internal_State := Waiting) is record
      case State is
         when Waiting | Number | Token =>
            null;
         when Base64_Atom | Base64_Expr =>
            Chunk : Read_Buffer;
         when Hex_Atom =>
            Nibble_Buffer : Octet;
         when Quoted_Atom =>
            Escape : Read_Buffer;
         when Verbatim_Atom =>
            Size : Count;
      end case;
   end record;

   type Parser is abstract limited new Lockable.Descriptor with record
      Internal     : State_Data;
      Next_Event : Events.Event := Events.End_Of_Input;
      Latest       : Events.Event := Events.Error;
      Pending : Atom_Buffers.Atom_Buffer;
      Buffer       : Atom_Buffers.Atom_Buffer;
      Level        : Natural := 0;
      Lock_Stack : Lockable.Lock_Stack;
      Locked : Boolean := False;
   end record;

   type Stream_Parser (Input : access Ada.Streams.Root_Stream_Type'Class) is
     limited new Parser with null record;

   overriding procedure Read_More
     (Self : in out Stream_Parser;
      Buffer : out Atom_Buffers.Atom_Buffer);

   type Memory_Parser is limited new Parser with null record;

   overriding procedure Read_More
     (Self : in out Memory_Parser;
      Buffer : out Atom_Buffers.Atom_Buffer)
     is null;

end Natools.S_Expressions.Parsers;
