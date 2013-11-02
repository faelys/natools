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
-- Natools.Indefinite_Holders is an implementation of a subset of Ada 2012  --
-- Containers.Indefinite_Holders, compatible with Ada 2005.                 --
--                                                                          --
-- WARNING: tampering checks are not implemented yet.                       --
------------------------------------------------------------------------------

private with Ada.Finalization;
private with Ada.Unchecked_Deallocation;

generic
   type Element_Type (<>) is private;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;

package Natools.Indefinite_Holders is
   pragma Preelaborate (Indefinite_Holders);
   --  pragma Remote_Types (Indefinite_Holders); is not supported yet.

   type Holder is tagged private;
   pragma Preelaborable_Initialization (Holder);

   Empty_Holder : constant Holder;

   function "=" (Left, Right : Holder) return Boolean;

   function To_Holder (New_Item : Element_Type) return Holder;

   function Is_Empty (Container : Holder) return Boolean;

   procedure Clear (Container : in out Holder);
   function Element (Container : Holder) return Element_Type;

   procedure Replace_Element (Container : in out Holder;
                              New_Item  : in     Element_Type);

   procedure Query_Element
     (Container : in Holder;
      Process   : not null access procedure (Element : in Element_Type));

   procedure Update_Element
     (Container : in out Holder;
      Process   : not null access procedure (Element : in out Element_Type));

   function Reference (Container : Holder) return access Element_Type;

   procedure Assign (Target : in out Holder; Source : in Holder);

   function Copy (Source : Holder) return Holder;

   procedure Move (Target : in out Holder; Source : in out Holder);

private

   type Element_Access is access Element_Type;

   procedure Free is new Ada.Unchecked_Deallocation
     (Element_Type, Element_Access);

   type Holder is new Ada.Finalization.Controlled with record
      Ref : Element_Access := null;
   end record;

   overriding procedure Adjust (Object : in out Holder);
   overriding procedure Finalize (Object : in out Holder);

   Empty_Holder : constant Holder := (Ada.Finalization.Controlled with null);

end Natools.Indefinite_Holders;
