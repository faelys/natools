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
-- Natools.Constant_Indefinite_Ordered_Sets provides an implementation of   --
-- indefinite ordered maps with immutable mapping, based on a sorted array. --
-- This makes it task-safe as long as the mapping is only read.             --
--                                                                          --
-- All the types exposed have referential semantics, in that assignment is  --
-- cheap and uses the same actual object. It is as task-safe as the current --
-- implementation of Natools.References.                                    --
-- Cursors also hold a reference, which is used to identify the parent map, --
-- so after an assignment or a call to Clear or Move, the link between the  --
-- map object and the cursor is broken, but the cursor is still usable and  --
-- behaves as the original version of the maps.                             --
--                                                                          --
-- There are four types defined here, depending on their restrictions and   --
-- safety against concurrent accesses:                                      --
--   * Constant_Map cannot be changed in any way, but is completely         --
--     task-safe (unless some referential magic is performed, like          --
--     tampering checks in standard containers)                             --
--   * Updatable_Map allows read-write operations on stored elements, but   --
--     it is up to the client to ensure there operations are task-safe,     --
--     e.g. by using an atomic or protected Element_Type.                   --
--   * Mutable_Map allows all standard operations, but creating a new       --
--     mapping copied from the original. This is likely to be very          --
--     inefficient, and it is up to the client to ensure task-safety of the --
--     underlying copy with regard to update operations.                    --
--     NOTE: Mutable_Map is not provided yet, since its usefullness is      --
--     really not obvious.                                                  --
--                                                                          --
-- All the subprograms here have the semantics of standard indefinite       --
-- ordered maps (see ARM A.18.6), except for tampering, which becomes       --
-- irrelevant.                                                              --
------------------------------------------------------------------------------

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Iterator_Interfaces;

private with Ada.Finalization;
private with Ada.Unchecked_Deallocation;
private with Natools.References;
private with Natools.Storage_Pools;

generic
   type Key_Type (<>) is private;
   type Element_Type (<>) is private;
   with function "<" (Left, Right : Key_Type) return Boolean is <>;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;

package Natools.Constant_Indefinite_Ordered_Maps is
   pragma Preelaborate;

   package Unsafe_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Key_Type, Element_Type);

   type Cursor is private;  --  with Type_Invariant => Is_Valid (Cursor);
   pragma Preelaborable_Initialization (Cursor);

   No_Element : constant Cursor;

   procedure Clear (Position : in out Cursor);
   function Is_Valid (Position : Cursor) return Boolean;

   function Has_Element (Position : Cursor) return Boolean;

   function Element (Position : Cursor) return Element_Type
     with Pre => Has_Element (Position) or else raise Constraint_Error;

   function Key (Position : Cursor) return Key_Type
     with Pre => Has_Element (Position) or else raise Constraint_Error;

   procedure Query_Element
     (Position : in Cursor;
      Process : not null access procedure (Key : in Key_Type;
                                           Element : in Element_Type))
     with Pre => Has_Element (Position) or else raise Constraint_Error;

   function Next (Position : Cursor) return Cursor;
   procedure Next (Position : in out Cursor);

   function Previous (Position : Cursor) return Cursor;
   procedure Previous (Position : in out Cursor);

   function "<" (Left, Right : Cursor) return Boolean
     with Pre => (Has_Element (Left) and then Has_Element (Right))
         or else raise Constraint_Error;
   function ">" (Left, Right : Cursor) return Boolean
     with Pre => (Has_Element (Left) and then Has_Element (Right))
         or else raise Constraint_Error;
   function "<" (Left : Cursor; Right : Key_Type) return Boolean
     with Pre => Has_Element (Left) or else raise Constraint_Error;
   function ">" (Left : Cursor; Right : Key_Type) return Boolean
     with Pre => Has_Element (Left) or else raise Constraint_Error;
   function "<" (Left : Key_Type; Right : Cursor) return Boolean
     with Pre => Has_Element (Right) or else raise Constraint_Error;
   function ">" (Left : Key_Type; Right : Cursor) return Boolean
     with Pre => Has_Element (Right) or else raise Constraint_Error;


   package Map_Iterator_Interfaces is new Ada.Iterator_Interfaces
     (Cursor, Has_Element);


   type Constant_Map is tagged private;
--   TODO: add aspects when they don't put GNAT in an infinite loop
--   with Constant_Indexing => Constant_Reference,
--        Default_Iterator => Iterate,
--        Iterator_Element => Element_Type;
   pragma Preelaborable_Initialization (Constant_Map);

   procedure Clear (Container : in out Constant_Map);
   function Create (Source : Unsafe_Maps.Map) return Constant_Map;
   procedure Move (Target : in out Constant_Map; Source : in out Constant_Map);
   procedure Replace
     (Container : in out Constant_Map;
      New_Items : in Unsafe_Maps.Map);
   function To_Unsafe_Map (Container : Constant_Map) return Unsafe_Maps.Map;

   function Is_Related (Container : Constant_Map; Position : Cursor)
     return Boolean;

   function "=" (Left, Right : Constant_Map) return Boolean;
   function Length (Container : Constant_Map) return Ada.Containers.Count_Type;
   function Is_Empty (Container : Constant_Map) return Boolean;

   function First (Container : Constant_Map) return Cursor;
   function First_Element (Container : Constant_Map) return Element_Type
     with Pre => (not Is_Empty (Container)) or else raise Constraint_Error;
   function First_Key (Container : Constant_Map) return Key_Type
     with Pre => (not Is_Empty (Container)) or else raise Constraint_Error;

   function Last (Container : Constant_Map) return Cursor;
   function Last_Element (Container : Constant_Map) return Element_Type
     with Pre => (not Is_Empty (Container)) or else raise Constraint_Error;
   function Last_Key (Container : Constant_Map) return Key_Type
     with Pre => (not Is_Empty (Container)) or else raise Constraint_Error;

   function Find (Container : Constant_Map; Key : Key_Type) return Cursor;
   function Element
     (Container : Constant_Map;
      Key : Key_Type)
     return Element_Type;

   function Floor (Container : Constant_Map; Key : Key_Type) return Cursor;
   function Ceiling (Container : Constant_Map; Key : Key_Type) return Cursor;
   function Contains (Container : Constant_Map; Key : Key_Type) return Boolean;

   procedure Iterate
     (Container : in Constant_Map;
      Process : not null access procedure (Position : in Cursor));

   procedure Reverse_Iterate
     (Container : in Constant_Map;
      Process : not null access procedure (Position : in Cursor));

   function Iterate (Container : in Constant_Map)
     return Map_Iterator_Interfaces.Reversible_Iterator'Class;

   function Iterate (Container : in Constant_Map; Start : in Cursor)
     return Map_Iterator_Interfaces.Reversible_Iterator'Class;

   function Iterate (Container : in Constant_Map; First, Last : in Cursor)
     return Map_Iterator_Interfaces.Reversible_Iterator'Class;


   type Constant_Reference_Type
     (Element : not null access constant Element_Type) is private
     with Implicit_Dereference => Element;

   function Constant_Reference
     (Container : aliased in Constant_Map;
      Position : in Cursor)
     return Constant_Reference_Type;

   function Constant_Reference
     (Container : aliased in Constant_Map;
      Key : in Key_Type)
     return Constant_Reference_Type;


   type Updatable_Map is new Constant_Map with private
     with Constant_Indexing => Constant_Reference,
          Variable_Indexing => Reference,
          Default_Iterator => Iterate,
          Iterator_Element => Element_Type;
   pragma Preelaborable_Initialization (Updatable_Map);

   procedure Update_Element
     (Container : in out Updatable_Map;
      Position : in Cursor;
      Process : not null access procedure (Key : in Key_Type;
                                           Element : in out Element_Type))
     with Pre => (Has_Element (Position) or else raise Constraint_Error)
      and then (Is_Related (Container, Position) or else raise Program_Error);


   type Reference_Type (Element : not null access Element_Type) is private
     with Implicit_Dereference => Element;

   function Reference
     (Container : aliased in out Updatable_Map;
      Position : in Cursor)
     return Reference_Type;

   function Reference
     (Container : aliased in out Updatable_Map;
      Key : in Key_Type)
     return Reference_Type;


private

   type Key_Access is access Key_Type;
   type Element_Access is access Element_Type;

   type Node is record
      Key :  not null Key_Access;
      Element :  not null Element_Access;
   end record;

   use type Ada.Containers.Count_Type;
   subtype Count_Type is Ada.Containers.Count_Type;
   subtype Index_Type is Count_Type range 1 .. Count_Type'Last;

   type Node_Array is array (Index_Type range <>) of Node;

   procedure Free is new Ada.Unchecked_Deallocation
     (Key_Type, Key_Access);
   procedure Free is new Ada.Unchecked_Deallocation
     (Element_Type, Element_Access);


   type Backend_Array (Size : Index_Type)  --  cannot be empty
     is new Ada.Finalization.Limited_Controlled with
   record
      Nodes : Node_Array (1 .. Size);
      Finalized : Boolean := False;
   end record;

   function Create
     (Size : Index_Type;
      Key_Factory : not null access function (Index : Index_Type)
         return Key_Type;
      Element_Factory : not null access function (Index : Index_Type)
         return Element_Type)
     return Backend_Array;

   overriding procedure Finalize (Object : in out Backend_Array);

   package Backend_Refs is new References
     (Backend_Array,
      Storage_Pools.Access_In_Default_Pool'Storage_Pool,
      Storage_Pools.Access_In_Default_Pool'Storage_Pool);

   function Make_Backend
     (Size : Count_Type;
      Key_Factory : not null access function (Index : Index_Type)
         return Key_Type;
      Element_Factory : not null access function (Index : Index_Type)
         return Element_Type)
     return Backend_Refs.Immutable_Reference;

   function Make_Backend (Map : Unsafe_Maps.Map)
     return Backend_Refs.Immutable_Reference;

   procedure Search
     (Nodes : in Node_Array;
      Key : in Key_Type;
      Floor : out Count_Type;
      Ceiling : out Count_Type);

   type Constant_Map is tagged record
      Backend : Backend_Refs.Immutable_Reference;
   end record;

   function Is_Empty (Container : Constant_Map) return Boolean
     is (Container.Backend.Is_Empty);

   type Updatable_Map is new Constant_Map with null record;

   type Cursor (Is_Empty : Boolean := True) is record
      case Is_Empty is
         when True => null;
         when False =>
            Index : Index_Type;
            Backend : Backend_Refs.Immutable_Reference;
      end case;
   end record;

   function Is_Valid (Position : Cursor) return Boolean
     is (Position.Is_Empty
        or else (not Position.Backend.Is_Empty
                 and then Position.Index <= Position.Backend.Query.Data.Size));

   function Has_Element (Position : Cursor) return Boolean
     is (not Position.Is_Empty);

   function Is_Related (Container : Constant_Map; Position : Cursor)
     return Boolean
     is (Backend_Refs."=" (Container.Backend, Position.Backend));


   type Constant_Reference_Type
     (Element : not null access constant Element_Type)
   is record
      Backend : Backend_Refs.Immutable_Reference;
   end record;


   type Reference_Type (Element : not null access Element_Type) is record
      Backend : Backend_Refs.Immutable_Reference;
   end record;


   type Iterator is new Map_Iterator_Interfaces.Reversible_Iterator with record
      Backend : Backend_Refs.Immutable_Reference;
      Start : Cursor := No_Element;
   end record;

   overriding function First (Object : Iterator) return Cursor;
   overriding function Last  (Object : Iterator) return Cursor;

   overriding function Next
     (Object   : Iterator;
      Position : Cursor) return Cursor
     is (Next (Position))
     with Pre => Position.Is_Empty
        or else Backend_Refs."=" (Position.Backend, Object.Backend);

   overriding function Previous
     (Object   : Iterator;
      Position : Cursor) return Cursor
     is (Previous (Position))
     with Pre => Position.Is_Empty
        or else Backend_Refs."=" (Position.Backend, Object.Backend);


   type Range_Iterator is new Map_Iterator_Interfaces.Reversible_Iterator
   with record
      Backend : Backend_Refs.Immutable_Reference;
      First_Position : Cursor;
      Last_Position : Cursor;
   end record;
--   with Dynamic_Predicate => not Range_Iterator.Backend.Is_Empty
--      and then Has_Element (Range_Iterator.First_Position)
--      and then Has_Element (Range_Iterator.Last_Position)
--      and then not Range_Iterator.First_Position
--         > Range_Iterator.Last_Position;

   overriding function First (Object : Range_Iterator) return Cursor;

   overriding function Last  (Object : Range_Iterator) return Cursor;

   overriding function Next
     (Object   : Range_Iterator;
      Position : Cursor) return Cursor
     with Pre => Position.Is_Empty
        or else Backend_Refs."=" (Position.Backend, Object.Backend);

   overriding function Previous
     (Object   : Range_Iterator;
      Position : Cursor) return Cursor
     with Pre => Position.Is_Empty
        or else Backend_Refs."=" (Position.Backend, Object.Backend);


   No_Element : constant Cursor := (Is_Empty => True);

end Natools.Constant_Indefinite_Ordered_Maps;
