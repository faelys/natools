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
-- Natools.S_Expressions.Generic_Caches provides a simple memory container  --
-- for S-expressions. The container is append-only, and provides cursors to --
-- replay it from start.                                                    --
-- This is a generic package that allow client-selected storage pools. An   --
-- instance with default storage pools is provided in                       --
-- Natools.S_Expressions.Caches.                                            --
-- The intended usage is efficient caching of S-expressions in memory. For  --
-- more flexible in-memory S-expression objects,                            --
-- see Natools.S_Expressions.Holders.                                       --
------------------------------------------------------------------------------

with System.Storage_Pools;

with Natools.S_Expressions.Printers;

private with Ada.Finalization;
private with Ada.Unchecked_Deallocation;
private with Natools.References;

generic
   Atom_Pool : in out System.Storage_Pools.Root_Storage_Pool'Class;
   Counter_Pool : in out System.Storage_Pools.Root_Storage_Pool'Class;
   Structure_Pool : in out System.Storage_Pools.Root_Storage_Pool'Class;

package Natools.S_Expressions.Generic_Caches is

   type Reference is new Printers.Printer with private;

   overriding procedure Open_List (Output : in out Reference);
   overriding procedure Append_Atom
     (Output : in out Reference; Data : in Atom);
   overriding procedure Close_List (Output : in out Reference);

   function Duplicate (Cache : Reference) return Reference;
      --  Create a new copy of the S-expression held in Cache and return it


   type Cursor is new Descriptor with private;

   overriding function Current_Event (Object : in Cursor) return Events.Event;
   overriding function Current_Atom (Object : in Cursor) return Atom;
   overriding function Current_Level (Object : in Cursor) return Natural;
   overriding procedure Query_Atom
     (Object : in Cursor;
      Process : not null access procedure (Data : in Atom));
   overriding procedure Read_Atom
     (Object : in Cursor;
      Data : out Atom;
      Length : out Count);
   overriding procedure Next
     (Object : in out Cursor;
      Event : out Events.Event);

   function First (Cache : Reference'Class) return Cursor;
      --  Create a new Cursor pointing at the beginning of Cache

private

   type Atom_Access is access Atom;
   for Atom_Access'Storage_Pool use Atom_Pool;
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Atom, Atom_Access);

   type Node;
   type Node_Access is access Node;
   for Node_Access'Storage_Pool use Structure_Pool;

   type Node_Kind is (Atom_Node, List_Node);

   type Node (Kind : Node_Kind) is record
      Parent : Node_Access;
      Next : Node_Access;

      case Kind is
         when Atom_Node => Data : Atom_Access;
         when List_Node => Child : Node_Access;
      end case;
   end record;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Node, Node_Access);


   type Tree is new Ada.Finalization.Limited_Controlled with record
      Root : Node_Access := null;
      Last : Node_Access := null;
      Opening : Boolean := False;
   end record;


   procedure Append
     (Exp : in out Tree;
      Kind : in Node_Kind;
      Data : in Atom_Access := null);
      --  Append a new node of the given Kind to Exp

   procedure Close_List (Exp : in out Tree);
      --  Close innermost list

   function Create_Tree return Tree;
      --  Create a new empty Tree

   function Duplicate (Source : Tree) return Tree;
      --  Deep copy of a Tree object

   overriding procedure Finalize (Object : in out Tree);
      --  Release all nodes contained in Object

   package Trees is new References (Tree, Structure_Pool, Counter_Pool);


   type Reference is new Printers.Printer with record
      Exp : Trees.Reference;
   end record;


   type Cursor is new Descriptor with record
      Exp : Trees.Reference := Trees.Null_Reference;
      Position : Node_Access := null;
      Opening : Boolean := False;
   end record;

end Natools.S_Expressions.Generic_Caches;
