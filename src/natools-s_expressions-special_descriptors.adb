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

package body Natools.S_Expressions.Special_Descriptors is

   overriding function Current_Event
     (Object : in Constant_Descriptor) return Events.Event is
   begin
      return Object.Event;
   end Current_Event;


   overriding function Current_Atom (Object : in Constant_Descriptor)
     return Atom
   is
      pragma Unreferenced (Object);
   begin
      raise Program_Error with "Current_Atom called on a special descriptor";
      return Null_Atom;
   end Current_Atom;


   overriding function Current_Level (Object : in Constant_Descriptor)
     return Natural
   is
      pragma Unreferenced (Object);
   begin
      raise Program_Error with "Current_Level called on a special descriptor";
      return 0;
   end Current_Level;

   overriding procedure Query_Atom
     (Object : in Constant_Descriptor;
      Process : not null access procedure (Data : in Atom))
   is
      pragma Unreferenced (Object, Process);
   begin
      raise Program_Error with "Query_Atom called on a special descriptor";
   end Query_Atom;


   overriding procedure Read_Atom
     (Object : in Constant_Descriptor;
      Data : out Atom;
      Length : out Count)
   is
      pragma Unreferenced (Object, Data, Length);
   begin
      raise Program_Error with "Read_Atom called on a special descriptor";
   end Read_Atom;


   overriding procedure Next
     (Object : in out Constant_Descriptor;
      Event : out Events.Event)
   is
      pragma Unmodified (Object);
   begin
      Event := Object.Event;
   end Next;


   overriding procedure Lock
     (Object : in out Constant_Descriptor;
      State : out Lockable.Lock_State)
   is
      pragma Unreferenced (Object, State);
   begin
      raise Program_Error with "Lock called on a special descriptor";
   end Lock;


   overriding procedure Unlock
     (Object : in out Constant_Descriptor;
      State : in out Lockable.Lock_State;
      Finish : in Boolean := True)
   is
      pragma Unreferenced (Object, State, Finish);
   begin
      raise Program_Error with "Unlock called on a special descriptor";
   end Unlock;

end Natools.S_Expressions.Special_Descriptors;
