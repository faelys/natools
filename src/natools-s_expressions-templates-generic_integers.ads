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
--   (image (0 "symbol 0") (2 "symbol 2") ...)                              --
--   (max-width "max width" ["overflow text"])                              --
--   (min-width "min width")                                                --
--   (padding "left-symbol" "right-symbol")                                 --
--   (padding "symbol")                                                     --
--   (prefix ("prefix" 0 (10 20) ...) (("prefix" width) 2) ...)             --
--   (right-padding "symbol")                                               --
--   (sign "plus sign" ["minus sign"])                                      --
--   (suffix ("suffix" 0 (10 20) ...) (("suffix" width) 2) ...)             --
--   (width "fixed width")                                                  --
--   (width "min width" "max width" ["overflow text"])                      --
-- Top-level atoms are taken as the image for the next number.              --
------------------------------------------------------------------------------

with Ada.Containers.Ordered_Maps;
with Ada.Streams;
with Natools.References;
with Natools.S_Expressions.Atom_Buffers;
with Natools.S_Expressions.Atom_Refs;
with Natools.S_Expressions.Lockable;
with Natools.Storage_Pools;

generic
   type T is range <>;
   with function "<" (Left, Right : T) return Boolean is <>;
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


   type Interval is record
      First, Last : T;
   end record
     with Dynamic_Predicate => not (Interval.Last < Interval.First);

   function "<" (Left, Right : Interval) return Boolean
     is (Left.Last < Right.First);
      --  Strict non-overlap comparison

   type Displayed_Atom is record
      Image : Atom_Refs.Immutable_Reference;
      Width : Generic_Integers.Width;
   end record;

   package Atom_Maps is new Ada.Containers.Ordered_Maps
     (Interval, Displayed_Atom, "<");

   function Next_Index (Map : Atom_Maps.Map) return T
     is (if Map.Is_Empty then T'First else Map.Last_Key.Last + 1);
      --  Index of the next element to insert in sequential lists

   procedure Exclude
     (Map : in out Atom_Maps.Map;
      Values : in Interval);
      --  Remove the given interval from the map

   procedure Include
     (Map : in out Atom_Maps.Map;
      Values : in Interval;
      Image : in Atom_Refs.Immutable_Reference;
      Width : in Generic_Integers.Width);
      --  Add Image to the given interval, overwriting any existing values.
      --  If Image is empty, behave like Exclude.

   procedure Parse_Single_Affix
     (Map : in out Atom_Maps.Map;
      Expression : in out Lockable.Descriptor'Class);
      --  Parse Expression as an affix atom, followed by single numbers (atoms)
      --  or ranges (lists of two atoms).

   procedure Parse
     (Map : in out Atom_Maps.Map;
      Expression : in out Lockable.Descriptor'Class);
      --  Parse Expression as a list of single image expressions (see above)

   ---------------------
   -- Format Mutators --
   ---------------------

   procedure Set_Align (Object : in out Format; Value : in Alignment);

   procedure Append_Image
     (Object : in out Format;
      Image : in Atom_Refs.Immutable_Reference);
   procedure Append_Image
     (Object : in out Format;
      Image : in Atom);
   procedure Remove_Image (Object : in out Format; Value : in T);
   procedure Set_Image
     (Object : in out Format;
      Value : in T;
      Image : in Atom_Refs.Immutable_Reference);
   procedure Set_Image
     (Object : in out Format;
      Value : in T;
      Image : in Atom);

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

   procedure Remove_Prefix
     (Object : in out Format;
      Value : in T);
   procedure Set_Prefix
     (Object : in out Format;
      Value : in T;
      Prefix : in Atom_Refs.Immutable_Reference;
      Width : in Generic_Integers.Width);
   procedure Set_Prefix
     (Object : in out Format;
      Value : in T;
      Prefix : in Atom);
   procedure Set_Prefix
     (Object : in out Format;
      Value : in T;
      Prefix : in Atom;
      Width : in Generic_Integers.Width);
   procedure Remove_Prefix
     (Object : in out Format;
      Values : in Interval);
   procedure Set_Prefix
     (Object : in out Format;
      Values : in Interval;
      Prefix : in Atom_Refs.Immutable_Reference;
      Width : in Generic_Integers.Width);
   procedure Set_Prefix
     (Object : in out Format;
      Values : in Interval;
      Prefix : in Atom);
   procedure Set_Prefix
     (Object : in out Format;
      Values : in Interval;
      Prefix : in Atom;
      Width : in Generic_Integers.Width);

   procedure Set_Right_Padding
     (Object : in out Format;
      Symbol : in Atom_Refs.Immutable_Reference);
   procedure Set_Right_Padding
     (Object : in out Format;
      Symbol : in Atom);

   procedure Remove_Suffix
     (Object : in out Format;
      Value : in T);
   procedure Set_Suffix
     (Object : in out Format;
      Value : in T;
      Suffix : in Atom_Refs.Immutable_Reference;
      Width : in Generic_Integers.Width);
   procedure Set_Suffix
     (Object : in out Format;
      Value : in T;
      Suffix : in Atom);
   procedure Set_Suffix
     (Object : in out Format;
      Value : in T;
      Suffix : in Atom;
      Width : in Generic_Integers.Width);
   procedure Remove_Suffix
     (Object : in out Format;
      Values : in Interval);
   procedure Set_Suffix
     (Object : in out Format;
      Values : in Interval;
      Suffix : in Atom_Refs.Immutable_Reference;
      Width : in Generic_Integers.Width);
   procedure Set_Suffix
     (Object : in out Format;
      Values : in Interval;
      Suffix : in Atom);
   procedure Set_Suffix
     (Object : in out Format;
      Values : in Interval;
      Suffix : in Atom;
      Width : in Generic_Integers.Width);

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

      Images : Atom_Maps.Map;
      Prefix : Atom_Maps.Map;
      Suffix : Atom_Maps.Map;
   end record;

end Natools.S_Expressions.Templates.Generic_Integers;
