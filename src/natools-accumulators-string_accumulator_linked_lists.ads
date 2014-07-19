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
-- Natools.Accumulators.String_Accumulator_Linked_Lists is a simple         --
-- implementation of String_Accumulator_Stack using an external function to --
-- generate the String_Accumulator elements when the stack is grown.        --
------------------------------------------------------------------------------

private with Ada.Containers.Indefinite_Doubly_Linked_Lists;

package Natools.Accumulators.String_Accumulator_Linked_Lists is
   pragma Preelaborate (String_Accumulator_Linked_Lists);

   type String_Accumulator_Linked_List
     (Build : not null access function (Depth : Positive)
                              return String_Accumulator'Class)
      is new String_Accumulator_Stack with private;
   pragma Preelaborable_Initialization (String_Accumulator_Linked_List);

   procedure Append (To   : in out String_Accumulator_Linked_List;
                     Text : String);
      --  Append the given String to the internal buffer

   procedure Hard_Reset (Acc : in out String_Accumulator_Linked_List);
      --  Empty the internal buffer and free all possible memory

   function Length (Acc : String_Accumulator_Linked_List) return Natural;
      --  Return the length of the internal buffer

   procedure Push (Acc : in out String_Accumulator_Linked_List);
      --  Push the current internal buffer and start with an empty one

   procedure Pop (Acc : in out String_Accumulator_Linked_List);
      --  Drop the current internal buffer and use the previsouly pushed one
      --     instead
      --  Raise Program_Error when trying to pop the last internal buffer

   procedure Soft_Reset (Acc : in out String_Accumulator_Linked_List);
      --  Empty the internal buffer for reuse

   function Tail (Acc : String_Accumulator_Linked_List; Size : Natural)
      return String;
      --  Return the last characters from the internal buffer

   function To_String (Acc : String_Accumulator_Linked_List) return String;
      --  Output the whole internal buffer as a String

   procedure To_String (Acc : String_Accumulator_Linked_List;
                        Output : out String);
      --  Write the whole internal buffer into the String, which must be
      --    large enough.

   procedure Unappend (From : in out String_Accumulator_Linked_List;
                       Text : String);
      --  Remove the given suffix from the internal buffer
      --  Do nothing if the given text is not a prefix the internal buffer

private

   package Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists
     (Element_Type => String_Accumulator'Class);

   type String_Accumulator_Linked_List
     (Build : not null access function (Depth : Positive)
                              return String_Accumulator'Class)
      is new String_Accumulator_Stack with
   record
      Stack    : Lists.List;
      Position : Lists.Cursor;
   end record;

   procedure Initialize_If_Needed
     (Object : in out String_Accumulator_Linked_List);

end Natools.Accumulators.String_Accumulator_Linked_Lists;
