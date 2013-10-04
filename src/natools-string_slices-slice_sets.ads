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
-- Natools.String_Slices.Slice_Sets implements objects representing a set   --
-- of non-overlapping slices of a single common reference string, sorted    --
-- by appearance order. It can also be viewed as the reference string minus --
-- any subset of indices.                                                   --
--                                                                          --
-- It is implemented on top of Natools.String_Slices to reuse the           --
-- reference-counted string holder infrastructure.                          --
------------------------------------------------------------------------------

with Ada.Strings.Maps;

private with Ada.Containers.Ordered_Sets;

package Natools.String_Slices.Slice_Sets is
   pragma Preelaborate (Slice_Sets);

   type Slice_Set is tagged private;


   ----------------------------
   -- Conversion subprograms --
   ----------------------------

   function To_Slice (S : Slice_Set) return Slice;
      --  Return a slice matching S contents.
      --  It re-uses the existing string reference if possible, and otherwise
      --  a new one is created.

   function To_Slice_Set (S : String) return Slice_Set;
      --  Create a new slice set referring to the given string as a whole

   function To_Slice_Set (S : Slice) return Slice_Set;
      --  Create a new slice set containing only the given slice

   function To_String (Set : Slice_Set) return String;
      --  Return the string represented by the concatenation of slices in Set

   function To_String (Set : Slice_Set; Subrange : String_Range) return String;
   function To_String (Set : Slice_Set; First : Positive; Last : Natural)
     return String;
      --  Return the concatenation of slices in Set and inside Subrage


   ---------------------------------
   -- Basic slice-set subprograms --
   ---------------------------------

   procedure Clear (Set : in out Slice_Set);
      --  Clear the set, keeping the parent string reference

   function Element (Set : Slice_Set; Index : Positive) return Character;
      --  Return the character at the given index
      --  Raise Constraint_Error when the index is outside the slice set

   function First (Set : Slice_Set) return Positive;
      --  Return the lowest index in Set

   function Is_Empty (Set : Slice_Set) return Boolean;
      --  Return whether any slice exists in Set

   function Is_In (Set : Slice_Set; Index : Natural) return Boolean;
      --  Return whether Index is inside Set

   function Is_Null (Set : Slice_Set) return Boolean;
      --  Return whether Set is empty and without parent string reference

   function Is_Valid (Set : Slice_Set) return Boolean;
      --  Check whether Set is in a consistent internal state

   function Last (Set : Slice_Set) return Natural;
      --  Return the largest index in Set

   function Next (Set : Slice_Set; Index : Natural) return Natural;
      --  Return smallest valid index in Set greater than Index

   procedure Next (Set : in Slice_Set; Index : in out Natural);
      --  Update Index to the following valid value

   function Previous (Set : Slice_Set; Index : Natural) return Natural;
      --  Return the greatest valid index in Set smaller than Index

   procedure Previous (Set : in Slice_Set; Index : in out Natural);
      --  Update Index to the preceeding valid value

   function Total_Length (Set : Slice_Set) return Natural;
      --  Return the number of characters in the slice set


   ----------------------------
   -- Operation on slice set --
   ----------------------------

   procedure Add_Slice
     (Set : in out Slice_Set;
      First : in Positive;
      Last : in Natural);
   procedure Add_Slice (Set : in out Slice_Set; Bounds : in String_Range);
   procedure Add_Slice (Set : in out Slice_Set; S : in Slice);
      --  Add the given slice to the set.
      --  Raise Constraint_Error when it overlaps existing slices in the set.

   procedure Include_Slice
     (Set : in out Slice_Set;
      First : in Positive;
      Last : in Natural);
   procedure Include_Slice (Set : in out Slice_Set; Bounds : in String_Range);
   procedure Include_Slice (Set : in out Slice_Set; S : in Slice);
      --  Merge the given slice with the existing set. This is an ensemblist
      --  union that allows overlaps.

   procedure Exclude_Slice
     (Set : in out Slice_Set;
      First : in Positive;
      Last : in Natural);
   procedure Exclude_Slice (Set : in out Slice_Set; Bounds : in String_Range);
      --  Subtract the given range form Set

   procedure Restrict (Set : in out Slice_Set; Bounds : in String_Range);
   procedure Restrict
     (Set : in out Slice_Set;
      First : in Positive;
      Last : in Natural);
      --  Subract from Set indices outside of Bounds

   function Subset (Set : Slice_Set; Bounds : String_Range) return Slice_Set;
   function Subset (Set : Slice_Set; First : Positive; Last : Natural)
     return Slice_Set;
      --  Return a slice set containing indices from Set that are inside Bounds

   procedure Cut_Before (Set : in out Slice_Set; Index : in Positive);
      --  Split the slice containing Index just before it


   ---------------
   -- Iterators --
   ---------------

   procedure Trim_Slices
     (Set : in out Slice_Set;
      Trim : not null access function (Slice : String) return String_Range);
      --  Iterate over slices in Set, and allow the callback to return
      --  a subrange.

   procedure Query_Slices
     (Set : in Slice_Set;
      Process : not null access procedure (S : in Slice));
      --  Call Process with each slice in Set

   function Find_Slice
     (Set : Slice_Set;
      From : Positive;
      Test : not null access function (Slice : String) return Boolean;
      Going : Ada.Strings.Direction := Ada.Strings.Forward)
     return String_Range;
      --  Iterate over slices, starting at From, and return the bounds of
      --  the first slice where Test returns True.

   function Find_Slice
     (Set : Slice_Set;
      Test : not null access function (Slice : String) return Boolean;
      Going : Ada.Strings.Direction := Ada.Strings.Forward)
     return String_Range;
      --  Variant of Find_Slice spanning the whole slice set


   ----------------------
   -- Search functions --
   ----------------------

   function Index
     (Source : Slice_Set;
      Set : Ada.Strings.Maps.Character_Set;
      From : Positive;
      Test : Ada.Strings.Membership := Ada.Strings.Inside;
      Going : Ada.Strings.Direction := Ada.Strings.Forward)
     return Natural;
      --  Equivalent to Ada.Strings.Fixed.Index restriced on the indices in Set

   function Index
     (Source : Slice_Set;
      Set : Ada.Strings.Maps.Character_Set;
      Test : Ada.Strings.Membership := Ada.Strings.Inside;
      Going : Ada.Strings.Direction := Ada.Strings.Forward)
     return Natural;
      --  Equivalent to Ada.Strings.Fixed.Index restriced on the indices in Set

private

   function "<" (Left, Right : String_Range) return Boolean;
      --  Comparison of the first bound, used for the ordered set

   package Range_Sets is new Ada.Containers.Ordered_Sets (String_Range);
   subtype Range_Set is Range_Sets.Set;

   function Is_Overlapping (Bounds : String_Range; Set : Range_Set)
     return Boolean;
      --  Return whether Bounds contains any index also in Set

   function Is_Valid (Set : Range_Set) return Boolean;
      --  Check whether intervals in Set are non-empty and non-overlapping

   function Total_Span (Set : Range_Set) return String_Range;
      --  Return the range formed by first and last indices in the whole set

   procedure Include_Range (Set : in out Range_Set; Bounds : in String_Range);
   procedure Exclude_Range (Set : in out Range_Set; Bounds : in String_Range);
      --  Perform ensemble union and subtraction of index sets


   type Slice_Set is tagged record
      Bounds : Range_Set;
      Ref : String_Refs.Reference;
   end record;

end Natools.String_Slices.Slice_Sets;
