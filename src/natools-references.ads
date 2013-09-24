------------------------------------------------------------------------------
-- Copyright (c) 2013, Natacha Porté                                        --
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
-- Natools.References implements reference-counted smart pointer to any     --
-- type of objects.                                                         --
-- This is a basic implementation that does not support weak references or  --
-- concurrency. However since there is no internal state, operations on     --
-- non-overlapping objects should be thread-safe.                           --
------------------------------------------------------------------------------

with Ada.Finalization;
with System.Storage_Pools;

generic
   type Held_Data (<>) is limited private;
   Counter_Pool : in out System.Storage_Pools.Root_Storage_Pool'Class;
   Data_Pool : in out System.Storage_Pools.Root_Storage_Pool'Class;

package Natools.References is
   pragma Preelaborate (References);

   type Reference is new Ada.Finalization.Controlled with private;

   type Accessor (Data : not null access constant Held_Data) is
     limited private;
   type Mutator (Data : not null access Held_Data) is
     limited private;


   function Create
     (Constructor : not null access function return Held_Data)
      return Reference;
      --  Create a new held object and return a reference to it

   procedure Replace
     (Ref : in out Reference;
      Constructor : not null access function return Held_Data);
      --  Replace the object held in Ref with a newly created object

   procedure Reset (Ref : in out Reference);
      --  Empty Ref

   function Is_Empty (Ref : Reference) return Boolean;
      --  Check whether Ref refers to an actual object

   function "=" (Left, Right : Reference) return Boolean;
      --  Check whether Left and Right refer to the same object


   function Query (Ref : in Reference) return Accessor;
   pragma Inline (Query);
      --  Return a derefenciable constant access to the held object

   function Update (Ref : in Reference) return Mutator;
   pragma Inline (Update);
      --  Return a derefenciable mutable access to the held object

   procedure Query
     (Ref : in Reference;
      Process : not null access procedure (Object : in Held_Data));
      --  Call Process with the held object

   procedure Update
     (Ref : in Reference;
      Process : not null access procedure (Object : in out Held_Data));
      --  Call Process with the held object

   Null_Reference : constant Reference;

private

   type Counter is new Natural;

   type Counter_Access is access Counter;
   for Counter_Access'Storage_Pool use Counter_Pool;

   type Data_Access is access Held_Data;
   for Data_Access'Storage_Pool use Data_Pool;

   type Reference is new Ada.Finalization.Controlled with record
      Count : Counter_Access := null;
      Data : Data_Access := null;
   end record;

   overriding procedure Adjust (Object : in out Reference);
      --  Increate reference counter

   overriding procedure Finalize (Object : in out Reference);
      --  Decrease reference counter and release memory if needed

   type Accessor (Data : not null access constant Held_Data) is limited record
      Parent : Reference;
   end record;

   type Mutator (Data : not null access Held_Data) is limited record
      Parent : Reference;
   end record;

   Null_Reference : constant Reference
     := (Ada.Finalization.Controlled with Count => null, Data => null);

end Natools.References;
