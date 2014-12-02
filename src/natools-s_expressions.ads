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
-- Natools.S_Expression declare basic types used in all children packages   --
-- dealing with S-expressions.                                              --
--                                                                          --
-- S-expressions here are defined as a half serialization mechanism, using  --
-- standard syntax from http://people.csail.mit.edu/rivest/Sexp.txt         --
--                                                                          --
-- Briefly, "atoms" are defined as a sequence of octets of any length, and  --
-- "lists" are defined as a sequence of items, each of which being either   --
-- an atom or another list. A S-expression is a sequence of octets that     --
-- represents such a list.                                                  --
--                                                                          --
-- So atoms are unstructured blob of data, supposed to be the serialization --
-- of some lower-level object (e.g. a string), and they are structured as   --
-- leaves in a tree.                                                        --
--                                                                          --
-- All S-expression code here assume that Stream_Element is actually an     --
-- 8-bit byte. So Octet, Atom and related types are derived from            --
-- Ada.Streams entries.                                                     --
------------------------------------------------------------------------------


with Ada.Streams;

package Natools.S_Expressions is
   pragma Pure (Natools.S_Expressions);


   -----------------
   -- Basic Types --
   -----------------

   subtype Octet is Ada.Streams.Stream_Element;

   subtype Offset is Ada.Streams.Stream_Element_Offset;
   subtype Count is Ada.Streams.Stream_Element_Count;
   subtype Atom is Ada.Streams.Stream_Element_Array;

   Null_Atom : constant Atom (1 .. 0) := (others => <>);

   function To_String (Data : in Atom) return String;
   function To_Atom (Data : in String) return Atom;

   function "<" (Left, Right : Atom) return Boolean
     renames Ada.Streams."<";
   function Less_Than (Left, Right : Atom) return Boolean;



   -----------------------------
   -- S-expression Descriptor --
   -----------------------------

   package Events is
      type Event is
        (Error,
         Open_List,
         Close_List,
         Add_Atom,
         End_Of_Input);
   end Events;

   type Descriptor is limited interface;
      --  Descriptor interface can be implemented by objects that can
      --  describe a S-expression to its holder, using an event-driven
      --  interface. The current event reports error conditions, or whether
      --  a beginning or end of list encountered, or whether a new atom is
      --  available.

   function Current_Event (Object : in Descriptor) return Events.Event
      is abstract;
      --  Return the current event in Object

   function Current_Atom (Object : in Descriptor) return Atom is abstract;
      --  Return the current atom in an Object whose state is Add_Atom

   function Current_Level (Object : in Descriptor) return Natural is abstract;
      --  Return the number of nested lists currently opened

   procedure Query_Atom
     (Object : in Descriptor;
      Process : not null access procedure (Data : in Atom)) is abstract;
      --  Read-in-place callback for the current atom in Object.
      --  Must only be called when current event in Object is Add_Event.

   procedure Read_Atom
     (Object : in Descriptor;
      Data : out Atom;
      Length : out Count) is abstract;
      --  Copy the current atom in Object to Data.
      --  Must only be called when current event in Object is Add_Event.

   procedure Next
     (Object : in out Descriptor;
      Event : out Events.Event) is abstract;
      --  Update Object to reflect the next event in the S-expression

   procedure Next (Object : in out Descriptor'Class);
      --  Call Next discarding current event

private

   use type Ada.Streams.Stream_Element;
   use type Ada.Streams.Stream_Element_Offset;
   use type Ada.Streams.Stream_Element_Array;
   use type Events.Event;

end Natools.S_Expressions;
