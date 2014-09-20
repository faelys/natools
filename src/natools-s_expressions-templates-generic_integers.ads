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
-- Natools.S_Expressions.Templates.Generic_Integers provides a template     --
-- interpreter for integer rendering.                                       --
-- The following commands are recognized:                                   --
--   (align "left|right|center")                                            --
--   (base "symbol 0" "symbol 1" "symbol 2" ...)                            --
--   (left-padding "symbol")                                                --
--   (max-width "max width" ["overflow text"])                              --
--   (min-width "min width")                                                --
--   (padding "left-symbol" "right-symbol")                                 --
--   (padding "symbol")                                                     --
--   (right-padding "symbol")                                               --
--   (sign "plus sign" ["minus sign"])                                      --
--   (width "fixed width")                                                  --
--   (width "min width" "max width" ["overflow text"])                      --
------------------------------------------------------------------------------

with Ada.Streams;
with Natools.References;
with Natools.S_Expressions.Atom_Buffers;
with Natools.S_Expressions.Atom_Refs;
with Natools.S_Expressions.Lockable;
with Natools.Storage_Pools;

generic
   type T is range <>;
package Natools.S_Expressions.Templates.Generic_Integers is
   pragma Preelaborate;

   --------------------------
   -- High-Level Interface --
   --------------------------

   type Format is tagged private;

   function Render (Value : T; Template : Format) return Atom;
      --  Render Value according to Template

   procedure Parse
     (Template : in out Format;
      Expression : in out Lockable.Descriptor'Class);
      --  Read Expression to fill Template

   procedure Render
     (Output : in out Ada.Streams.Root_Stream_Type'Class;
      Template : in out Lockable.Descriptor'Class;
      Value : in T);
      --  Read a rendering format from Template and use it on Value

   procedure Render
     (Output : in out Ada.Streams.Root_Stream_Type'Class;
      Default_Format : in Format;
      Template : in out Lockable.Descriptor'Class;
      Value : in T);
      --  Read a rendering format from Template, using defaults
      --  from Default_Format, and use it on Value.


   ---------------------
   -- Auxiliary Types --
   ---------------------

   type Alignment is (Left_Aligned, Centered, Right_Aligned);
   type Width is range 0 .. 10000;

   subtype Base_T is T'Base;
   subtype Natural_T is Base_T range 0 .. Base_T'Last;


   type Atom_Array
     is array (Natural_T range <>) of Atom_Refs.Immutable_Reference;

   function Create (Atom_List : in out S_Expressions.Descriptor'Class)
     return Atom_Array;
      --  Build an array consisting of consecutive atoms found in Atom_List


   package Atom_Arrays is new References
     (Atom_Array,
      Storage_Pools.Access_In_Default_Pool'Storage_Pool,
      Storage_Pools.Access_In_Default_Pool'Storage_Pool);

   function Create (Atom_List : in out S_Expressions.Descriptor'Class)
     return Atom_Arrays.Immutable_Reference;
      --  Build an array reference consisting of
      --  consecutive atoms found in Atom_List.

   function Decimal return Atom_Arrays.Immutable_Reference
     with Post => not Decimal'Result.Is_Empty;
      --  Return a reference to usual decimal representation


   ---------------------
   -- Format Mutators --
   ---------------------

   procedure Set_Align (Object : in out Format; Value : in Alignment);

   procedure Set_Left_Padding
     (Object : in out Format;
      Symbol : in Atom_Refs.Immutable_Reference);
   procedure Set_Left_Padding
     (Object : in out Format;
      Symbol : in Atom);

   procedure Set_Maximum_Width (Object : in out Format; Value : in Width);

   procedure Set_Minimum_Width (Object : in out Format; Value : in Width);

   procedure Set_Negative_Sign
     (Object : in out Format;
      Sign : in Atom_Refs.Immutable_Reference);
   procedure Set_Negative_Sign
     (Object : in out Format;
      Sign : in Atom);

   procedure Set_Overflow_Message
     (Object : in out Format;
      Message : in Atom_Refs.Immutable_Reference);
   procedure Set_Overflow_Message
     (Object : in out Format;
      Message : in Atom);

   procedure Set_Positive_Sign
     (Object : in out Format;
      Sign : in Atom_Refs.Immutable_Reference);
   procedure Set_Positive_Sign
     (Object : in out Format;
      Sign : in Atom);

   procedure Set_Right_Padding
     (Object : in out Format;
      Symbol : in Atom_Refs.Immutable_Reference);
   procedure Set_Right_Padding
     (Object : in out Format;
      Symbol : in Atom);

   procedure Set_Symbols
     (Object : in out Format;
      Symbols : in Atom_Arrays.Immutable_Reference);
   procedure Set_Symbols
     (Object : in out Format;
      Expression : in out S_Expressions.Descriptor'Class);


private

   Base_10 : Atom_Arrays.Immutable_Reference;
      --  Cache for the often-used decimal representation

   procedure Reverse_Render
     (Value : in Natural_T;
      Symbols : in Atom_Array;
      Output : in out Atom_Buffers.Atom_Buffer;
      Length : out Width)
     with Pre => Symbols'Length >= 2 and Symbols'First = 0;
      --  Create a little-endian image of Value using the given symbol table

   type Format is tagged record
      Symbols : Atom_Arrays.Immutable_Reference;
      Positive_Sign : Atom_Refs.Immutable_Reference;
      Negative_Sign : Atom_Refs.Immutable_Reference;

      Minimum_Width : Width := 0;
      Align : Alignment := Right_Aligned;
      Left_Padding : Atom_Refs.Immutable_Reference;
      Right_Padding : Atom_Refs.Immutable_Reference;

      Maximum_Width : Width := Width'Last;
      Overflow_Message : Atom_Refs.Immutable_Reference;
   end record;

end Natools.S_Expressions.Templates.Generic_Integers;
