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
-- parser that reads from an input stream.                                  --
--                                                                          --
-- Subparser objects wrap together Parser and input Stream, exposing a      --
-- Descriptor interface. A subparser is constrained to its initial nesting  --
-- level, and reports end-of-input instead of reaching lower.               --
------------------------------------------------------------------------------

with Ada.Streams;

with Natools.S_Expressions.Atom_Buffers;
with Natools.S_Expressions.Lockable;

package Natools.S_Expressions.Parsers is
   pragma Preelaborate (Natools.S_Expressions.Parsers);

   type Parser is tagged private;

   function Current_Event (P : in Parser) return Events.Event;
   function Current_Atom (P : in Parser) return Atom;
   function Current_Level (P : in Parser) return Natural;

   procedure Query_Atom
     (P : in Parser;
      Process : not null access procedure (Data : in Atom));

   procedure Read_Atom
     (P      : in Parser;
      Data   : out Atom;
      Length : out Count);

   procedure Next_Event
     (P     : in out Parser;
      Input : not null access Ada.Streams.Root_Stream_Type'Class);


   type Subparser
     (Backend : access Parser;
      Input   : access Ada.Streams.Root_Stream_Type'Class)
   is new Lockable.Descriptor with private;

   overriding function Current_Event (P : in Subparser) return Events.Event;
   overriding function Current_Atom (P : in Subparser) return Atom;
   overriding function Current_Level (P : in Subparser) return Natural;

   overriding procedure Query_Atom
     (P : in Subparser;
      Process : not null access procedure (Data : in Atom));

   overriding procedure Read_Atom
     (P      : in Subparser;
      Data   : out Atom;
      Length : out Count);

   overriding procedure Next (P : in out Subparser; Event : out Events.Event);


   overriding procedure Lock
     (Object : in out Subparser;
      State : out Lockable.Lock_State);

   overriding procedure Unlock
     (Object : in out Subparser;
      State : in out Lockable.Lock_State;
      Finish : in Boolean := True);

   procedure Finish (P : in out Subparser);
      --  Read enough data to exhaust intial nesting level

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

   type Parser is tagged record
      Internal     : State_Data;
      Pending      : Events.Event := Events.End_Of_Input;
      Override     : Atom_Buffers.Atom_Buffer;
      Latest       : Events.Event := Events.Error;
      Buffer       : Atom_Buffers.Atom_Buffer;
      Level        : Natural := 0;
   end record;

   type Subparser
     (Backend : access Parser;
      Input   : access Ada.Streams.Root_Stream_Type'Class)
   is new Lockable.Descriptor with record
      Levels      : Lockable.Lock_Stack;
      Initialized : Boolean := False;
      Terminated  : Boolean := False;
   end record;

end Natools.S_Expressions.Parsers;
