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
-- Natools.S_Expressions.Lockable provides an interface for Descriptor that --
-- can be locked in the current nesting level, returning End_Of_Input       --
-- instead of Close_List of that level. That way the descriptor can         --
-- be safely handed to other code fragments without risking scope change.   --
--                                                                          --
-- A Wrapper type is also provided to add a lockable layer on top of a      --
-- basic Descriptor interface, however with the extra overhead of one more  --
-- call.                                                                    --
------------------------------------------------------------------------------

package Natools.S_Expressions.Lockable is
   pragma Pure (Lockable);

   type Lock_Stack is private;
   type Lock_State is private;

   procedure Push_Level
     (Stack : in out Lock_Stack;
      Level : in Natural;
      State : out Lock_State);
      --  Insert Level on top of Stack and return current State

   procedure Pop_Level
     (Stack : in out Lock_Stack;
      State : in Lock_State;
      Allow_Gap : in Boolean := False);
      --  Remove upper part of Stack, up to and including the entry pointed
      --  by State. Constraint_Error is raised if State does not point to a
      --  valid level in the stack, and if Allow_Gap is True and more than
      --  one item would be removed.

   function Current_Level (Stack : Lock_Stack) return Natural;
      --  Return the value on top of the stack

   function Null_State return Lock_State;
      --  Return an invalid Lock_State


   type Descriptor is limited interface and S_Expressions.Descriptor;

   procedure Lock
     (Object : in out Descriptor;
      State : out Lock_State)
     is abstract;
      --  Turn Object into a state where it cannot reach below or beyond
      --  current nesting level at Lock call.

   procedure Unlock
     (Object : in out Descriptor;
      State : in out Lock_State;
      Finish : in Boolean := True)
     is abstract;
      --  Undo the effects of previous Lock call, and unwind Object until the
      --  end of locked level (unless Finish is False).


   type Wrapper (Backend : access S_Expressions.Descriptor'Class)
     is new Descriptor with private;
      --  Wrapper layer on top of a non-lockable object, albeit with the
      --  performance penalty of an extra layer.

   function Current_Event (Object : in Wrapper) return Events.Event;
   function Current_Atom (Object : in Wrapper) return Atom;
   function Current_Level (Object : in Wrapper) return Natural;
   procedure Query_Atom
     (Object : in Wrapper;
      Process : not null access procedure (Data : in Atom));
   procedure Read_Atom
     (Object : in Wrapper;
      Data : out Atom;
      Length : out Count);
   procedure Next
     (Object : in out Wrapper;
      Event : out Events.Event);

   procedure Lock
     (Object : in out Wrapper;
      State : out Lock_State);
   procedure Unlock
     (Object : in out Wrapper;
      State : in out Lock_State;
      Finish : in Boolean := True);

private

   type Lock_State is record
      Level, Depth : Natural := 0;
   end record;

   type Lock_Stack is record
      Level : Natural := 0;
      Depth : Positive := 1;
   end record;

   type Wrapper (Backend : access S_Expressions.Descriptor'Class)
     is new Descriptor with record
      Stack : Lock_Stack;
      Finished : Boolean := False;
   end record;

end Natools.S_Expressions.Lockable;
