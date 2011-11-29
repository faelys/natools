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

------------------------------------------------------------------------------
-- Natools.Accumulators is a collection of interfaces for data structures   --
-- that allow efficient accumulation of data.                               --
--                                                                          --
-- String_Accumulator is meant for creation of long strings through         --
-- repeated calls of Append, and later retrieval of the full buffer through --
-- one of the To_String subprograms. Length, Tail and Unappend are          --
-- helper utilities that might not be very efficient but can occasionnally  --
-- be useful. Hard_Reset and Soft_Reset both clear the internal state, with --
-- Soft_Reset aimed for speed while Hard_Reset aims for best memory release --
--                                                                          --
-- String_Accumulator_Stack adds a stack structure on top of                --
-- String_Accumulator, to allow temporary substrings to be created using    --
-- similar facilities. All operations on String_Accumulator except          --
-- Hard_Reset and Soft_Reset, when applied to String_Accumulator_Stack, are --
-- meant to be forwarded to the top accumulator of the stack. Push and Pop  --
-- change the stack state, while Hard_Reset and Soft_Reset apply to the     --
-- whole stack, with the same semantics as for String_Accumulator.          --
------------------------------------------------------------------------------

package Natools.Accumulators is
   pragma Pure (Accumulators);

   type String_Accumulator is interface;

   procedure Append (To : in out String_Accumulator; Text : String)
      is abstract;
      --  Append the given String to the internal buffer

   procedure Hard_Reset (Acc : in out String_Accumulator)
      is abstract;
      --  Empty the internal buffer and free all possible memory

   function Length (Acc : String_Accumulator) return Natural
      is abstract;
      --  Return the length of the internal buffer

   procedure Soft_Reset (Acc : in out String_Accumulator)
      is abstract;
      --  Empty the internal buffer for reuse

   function Tail (Acc : String_Accumulator; Size : Natural) return String
      is abstract;
      --  Return the last characters from the internal buffer

   function To_String (Acc : String_Accumulator) return String
      is abstract;
      --  Output the whole internal buffer as a String

   procedure To_String (Acc : String_Accumulator; Output : out String)
      is abstract;
      --  Write the whole internal buffer into the String, which must be
      --    large enough.

   procedure Unappend (From : in out String_Accumulator; Text : String)
      is abstract;
      --  Remove the given suffix from the internal buffer
      --  Do nothing if the given text is not a prefix the internal buffer



   type String_Accumulator_Stack is interface and String_Accumulator;

   procedure Push (Acc : in out String_Accumulator_Stack)
      is abstract;
      --  Push the current internal buffer and start with an empty one

   procedure Pop (Acc : in out String_Accumulator_Stack)
      is abstract;
      --  Drop the current internal buffer and use the previsouly pushed one
      --     instead
      --  Raise Program_Error when trying to pop the last internal buffer

end Natools.Accumulators;
