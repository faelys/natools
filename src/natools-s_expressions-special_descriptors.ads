------------------------------------------------------------------------------
-- Copyright (c) 2015, Natacha Porté                                        --
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
-- Natools.S_Expressions.Special_Descriptors provides descriptors blocked   --
-- in End_Of_Input or Error states.                                         --
-- The global variables should be task-safe, since there is no actual       --
-- change in any of the primitives.                                         --
------------------------------------------------------------------------------

with Natools.S_Expressions.Lockable;

package Natools.S_Expressions.Special_Descriptors is
   pragma Preelaborate;

   subtype Control_Event is Events.Event with Static_Predicate
     => Control_Event in Events.Error | Events.End_Of_Input;

   type Constant_Descriptor (Event : Control_Event) is new Lockable.Descriptor
     with null record;

   overriding function Current_Event (Object : in Constant_Descriptor)
     return Events.Event;

   overriding function Current_Atom (Object : in Constant_Descriptor)
     return Atom;

   overriding function Current_Level (Object : in Constant_Descriptor)
     return Natural;

   overriding procedure Query_Atom
     (Object : in Constant_Descriptor;
      Process : not null access procedure (Data : in Atom));

   overriding procedure Read_Atom
     (Object : in Constant_Descriptor;
      Data : out Atom;
      Length : out Count);

   overriding procedure Next
     (Object : in out Constant_Descriptor;
      Event : out Events.Event);

   overriding procedure Lock
     (Object : in out Constant_Descriptor;
      State : out Lockable.Lock_State);

   overriding procedure Unlock
     (Object : in out Constant_Descriptor;
      State : in out Lockable.Lock_State;
      Finish : in Boolean := True);


   Empty_Descriptor : Constant_Descriptor (Events.End_Of_Input);
   Error_Descriptor : Constant_Descriptor (Events.Error);

end Natools.S_Expressions.Special_Descriptors;
