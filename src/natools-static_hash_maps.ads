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
-- Natools.Static_Hash_Maps is a code generator that outputs hash map       --
-- packages based on GNAT.Perfect_Hash_Generator.                           --
-- This is mostly a text manipulation package, and no sanity checks are     --
-- performed on input strings, they are inserted as-is in the output files. --
------------------------------------------------------------------------------

private with Ada.Strings.Unbounded;
private with Ada.Containers.Doubly_Linked_Lists;

package Natools.Static_Hash_Maps is

   type Map_Node is private;

   function Node (Key, Name : String) return Map_Node;

   type Node_Array is array (Positive range <>) of Map_Node;

   type Map_Description is private;

   type Map_Array is array (Positive range <>) of Map_Description;

   procedure Reset (Self : out Map_Description);

   procedure Insert
     (Self : in out Map_Description;
      Key : in String;
      Element_Name : in String);
      --  Add the pair Key and element designated by Name.

   procedure Set_Element_Type
     (Self : in out Map_Description;
      Name : in String);
      --  String used to desginate the returning type of the element function.
      --  This must be set.

   procedure Set_Function_Name
     (Self : in out Map_Description;
      Name : in String);
      --  Element function name, defaulting to "Element"

   procedure Set_Hash_Package_Name
     (Self : in out Map_Description;
      Name : in String);
      --  Set the package name where the perfect hash function will be write.
      --  Defaults to "<map package>.<element type>_Hash".

   procedure Set_Not_Found
     (Self : in out Map_Description;
      Name : in String);
      --  If non-empty, when element function is called with an unknown key,
      --  return the elemnet named Name instead of raising Constraint_Error.

   function Map
     (Element_Type : String;
      Nodes : Node_Array;
      Hash_Package_Name : String := "";
      Function_Name : String := "Element";
      Not_Found : String := "")
     return Map_Description;
      --  Create a map in a single call


   type Map_Package is private;

   procedure Open
     (Self : in out Map_Package;
      Name : in String;
      Private_Child : in Boolean := False);
      --  Reset Self and initialize it with the givan package Name

   procedure Set_Description
     (Self : in out Map_Package;
      Description : in String);

   procedure Set_Extra_Declarations
     (Self : in out Map_Package;
      Declarations : in String);

   procedure Set_Private_Child
     (Self : in out Map_Package;
      Private_Child : in Boolean := True);

   procedure Set_Test_Child
     (Self : in out Map_Package;
      Test_Child : in String);

   procedure Add_Map (Self : in out Map_Package; Map : in Map_Description);
      --  Append a new Map to Self

   procedure Commit (Self : in out Map_Package);
      --  Write accumulated package description to disk

   procedure Close (Self : in out Map_Package);
      --  Drop all internal state


   procedure Generate_Package
     (Name : in String;
      Single_Map : in Map_Description;
      Private_Child : in Boolean := False);
   procedure Generate_Package
     (Name : in String;
      Maps : in Map_Array;
      Private_Child : in Boolean := False);
      --  Build an internal Map_Package and commit it to disk
      --  in a single operation.

private

   subtype String_Holder is Ada.Strings.Unbounded.Unbounded_String;

   function Hold (S : String) return String_Holder
     renames Ada.Strings.Unbounded.To_Unbounded_String;

   function To_String (H : String_Holder) return String
     renames Ada.Strings.Unbounded.To_String;


   type Map_Node is record
      Key, Name : String_Holder;
   end record;



   package Node_Lists is new Ada.Containers.Doubly_Linked_Lists (Map_Node);

   type Map_Description is record
      Element_Type : String_Holder;
      Hash_Package_Name : String_Holder;
      Function_Name : String_Holder;
      Not_Found : String_Holder;
      Nodes : Node_Lists.List;
   end record;


   package Map_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Map_Description);

   type Map_Package is record
      Name : String_Holder;
      Description : String_Holder;
      Extra_Declarations : String_Holder;
      Test_Child : String_Holder;
      Priv : Boolean;
      Maps : Map_Lists.List;
   end record;

end Natools.Static_Hash_Maps;
