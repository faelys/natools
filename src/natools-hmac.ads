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
-- Natools.HMAC provides an implementation of keyed-hash message            --
-- authentication code (HMAC) based on cryptographic hash function provided --
-- as formal parameters.                                                    --
------------------------------------------------------------------------------

with Ada.Streams;

generic
   type Hash_Context is private;
   Initial_Context : in Hash_Context;

   with procedure Update
     (Context : in out Hash_Context;
      Input : in Ada.Streams.Stream_Element_Array);

   with function Digest (Context : Hash_Context)
     return Ada.Streams.Stream_Element_Array;

   Block_Size_In_SE : in Ada.Streams.Stream_Element_Count;

package Natools.HMAC is

   type Context is private;

   procedure Setup
     (C : out Context;
      Key : in Ada.Streams.Stream_Element_Array);
   procedure Setup
     (C : out Context;
      Key : in String);
      --  Reset C with the given Key

   function Create (Key : Ada.Streams.Stream_Element_Array) return Context;
   function Create (Key : String) return Context;
      --  Create a new Context initialized with the given Key.
      --  This is equivalent to calling Setup on the returned object.

   procedure Update
     (C : in out Context;
      Input : in Ada.Streams.Stream_Element_Array);
   procedure Update
     (C : in out Context;
      Input : in String);
      --  Append Input to the HMACed message

   function Digest (C : Context) return Ada.Streams.Stream_Element_Array;
      --  Return the HMAC of the message given to C

   function Digest (Key : String; Message : Ada.Streams.Stream_Element_Array)
     return Ada.Streams.Stream_Element_Array;
   function Digest (Key, Message : Ada.Streams.Stream_Element_Array)
     return Ada.Streams.Stream_Element_Array;
      --  Return directly the HMAC of Message with the given Key

private

   type Context is record
      Key : Ada.Streams.Stream_Element_Array (1 .. Block_Size_In_SE);
      Hash : Hash_Context;
   end record;

end Natools.HMAC;
