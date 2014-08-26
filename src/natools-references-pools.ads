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
-- Natools.References.Pools provides a task-safe dynamic pool of            --
-- homogeneous references.                                                  --
------------------------------------------------------------------------------

private with Ada.Unchecked_Deallocation;

generic
package Natools.References.Pools is
   pragma Preelaborate;

   type Pool_Backend is limited private;
   pragma Preelaborable_Initialization (Pool_Backend);

   type Pool_Size is new Natural;

   protected type Pool is

      procedure Get (Ref : out Reference)
        with Post => not Ref.Is_Empty;
         --  Return an existing non-empty available reference from the pool,
         --  raising Constraint_Error when not possible.

      procedure Get
        (Constructor : not null access function return Held_Data;
         Ref : out Reference)
        with Post => not Ref.Is_Empty;
         --  Return an available reference from the pool, initializing it
         --  if needed, but without expanding the pool.
         --  Raise Constraint_Error when all references are in use.

      procedure Create
        (Constructor : not null access function return Held_Data;
         Ref : out Reference;
         Expand_Count : in Pool_Size := 1)
        with Pre => Expand_Count > 0, Post => not Ref.Is_Empty;
         --  Return a reference from the pool, creating it and/or initializing
         --  it if needed.

      procedure Preallocate
        (New_Item_Count : in Pool_Size;
         Constructor : access function return Held_Data := null);
         --  Add New_Item_Count references to the pool, using Constructor to
         --  initialize them if not null.

      procedure Release_Unused;
         --  Empty all references from the pool that are not used externally

      procedure Trim;
         --  Remove empty references from the pool, diminishing its capacity

      procedure Purge;
         --  Remove empty and available references from the pool.
         --  Equivalent to Release_Unused followed by Trim.

      function Capacity return Pool_Size;
         --  Return the number of references in the pool

      function Initialized_Size return Pool_Size;
         --  Return the number of non-empty references in the pool

      function Active_Size return Pool_Size;
         --  Return the number of externally-used references in the pool.
         --  WARNING: the result might be stale before it can be used by the
         --  client, do not take any sensitive decision from it.

      procedure Unchecked_Iterate
        (Process : not null access procedure (Ref : in Reference));
         --  Iterate over all references held in the pool.
         --  WARNING: Process must not call any potentially blocking operations
         --  or any operation on the current pool, and safety of any tampering
         --  with Ref or its referred object must be ensured independently.

   private
      Backend : Pool_Backend;
   end Pool;

private

   --  Basic types

   subtype Reference_Index is Pool_Size range 1 .. Pool_Size'Last;
   subtype Extended_Index is Pool_Size range 0 .. Pool_Size'Last;

   type Reference_Array is array (Reference_Index range <>) of Reference;

   type Reference_Array_Access is access Reference_Array;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Reference_Array, Reference_Array_Access);


   --  Dynamic array backend

   type Pool_Backend is new Ada.Finalization.Limited_Controlled with record
      Refs : Reference_Array_Access := null;
   end record;

   overriding procedure Finalize (Object : in out Pool_Backend);

   not overriding procedure Find
     (Container : in Pool_Backend;
      First_Available : out Extended_Index;
      First_Empty : out Extended_Index)
   with Post =>
     (First_Available = 0 or else
        (not Container.Refs (First_Available).Is_Empty
           and then Container.Refs (First_Available).Is_Last))
     and then (First_Empty = 0 or else Container.Refs (First_Empty).Is_Empty);

   not overriding function Length (Container : Pool_Backend) return Pool_Size
     is (if Container.Refs = null then 0 else Container.Refs'Length);

   not overriding procedure Preallocate
     (Container : in out Pool_Backend;
      New_Item_Count : in Pool_Size;
      Constructor : access function return Held_Data := null)
   with Post => (Container.Length = Container.Length'Old + New_Item_Count);

end Natools.References.Pools;
