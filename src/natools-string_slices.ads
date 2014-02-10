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
-- Natools.String_Slices provide an object that represents a substring of a --
-- shared parent string.                                                    --
------------------------------------------------------------------------------

private with Natools.References;
private with Natools.Storage_Pools;

package Natools.String_Slices is
   pragma Preelaborate (String_Slices);

   -----------------------
   -- String range type --
   -----------------------

   type String_Range is record
      First  : Positive;
      Length : Natural;
   end record;

   function Is_In (Point : Natural; Reference : String_Range) return Boolean;
   function Is_Subrange (Sample, Reference : String_Range) return Boolean;
      --  Inclusion tests

   function Last (Self : String_Range) return Natural;
      --  Return last bound of the given range

   function To_Range (First : Positive; Last : Natural) return String_Range;
      --  Create a range with the given bounds

   function Get_Range (S : String) return String_Range;
      --  Return the String_Range representation of S index range.
      --  Semantically equivalent to (To_Range (S'First, S'Last))
      --  and to (String_Range'(First => S'First, Length => S'Length)).

   procedure Set_First (Self : in out String_Range; New_First : in Positive);
      --  Update first bound keeping last bound intact

   procedure Set_Last (Self : in out String_Range; New_Last : in Natural);
      --  Update range for the given last bound, keeping the first one intact

   procedure Set_Length (Self : in out String_Range; New_Length : in Natural);
      --  Basic mutator included for completeness sake

   function Image (Interval : String_Range) return String;
      --  Interval representation of the given range


   ----------------
   -- Slice type --
   ----------------

   type Slice is tagged private;

   Null_Slice : constant Slice;


   --------------------------
   -- Conversion functions --
   --------------------------

   function To_Slice (S : String) return Slice;
      --  Create a new slice containing the whole given string

   function To_String (S : Slice) return String;
      --  Return the string represented by the slice


   ---------------
   -- Accessors --
   ---------------

   procedure Export (S : in Slice; Output : out String);
      --  Fill Output with string contents in S
      --  Raise Constraint_Error when Output'Length /= Length(S)

   procedure Query
     (S : in Slice;
      Process : not null access procedure (Text : in String));
      --  Query the string object directly from memory

   function Get_Range (S : Slice) return String_Range;
      --  Return the range embedded in S

   function First (S : Slice) return Positive;
      --  Return the lowest index of S

   function Last (S : Slice) return Natural;
      --  Return the largest index of S

   function Length (S : Slice) return Natural;
      --  Return the length of S


   ---------------
   -- Extenders --
   ---------------

   --  These subprograms allow access to the parent string beyond the
   --  current range. However Constraint_Error is raised when trying to reach
   --  beyond the parent string range.

   function Parent (S : Slice) return Slice;
      --  Return a slice representing the whole string available

   function Extend (S : Slice; New_Range : in String_Range) return Slice;
   function Extend (S : Slice; First : Positive; Last : Natural) return Slice;
   procedure Extend (S : in out Slice; New_Range : in String_Range);
   procedure Extend (S : in out Slice; First : in Positive; Last : in Natural);
      --  Extend the range represented by S


   -----------------
   -- Restrictors --
   -----------------

   --  All the subprograms here raise Constraint_Error when the new range
   --  is not a subrange of the source range.

   function Subslice (S : Slice; New_Range : String_Range) return Slice;
   function Subslice (S : Slice; First : Positive; Last : Natural)
     return Slice;
      --  Return a subslice of S

   procedure Restrict (S : in out Slice; New_Range : in String_Range);
   procedure Restrict
     (S : in out Slice; First : in Positive; Last : in Natural);
      --  Update the range in S

   procedure Set_First (S : in out Slice; New_First : in Positive);
      --  Update the range of S keeping the upper bound intact

   procedure Set_Last (S : in out Slice; New_Last : in Natural);
      --  Update the range of S keeping the lower bound intact

   procedure Set_Length (S : in out Slice; New_Length : in Natural);
      --  Truncate S range to the given length, keeping the lower bound intact


   ----------------------
   -- Slice comparison --
   ----------------------

   function Is_Empty (S : Slice) return Boolean;
      --  Return whether the slice represents an empty string.
      --  Semantically equivalent to (To_String (S) = "").

   function Is_Null (S : Slice) return Boolean;
      --  Return whether the slice has a parent string

   function Is_Related (Left, Right : Slice) return Boolean;
      --  Return whether both slices have the same parent string

   function Is_Subslice (S, Reference : Slice) return Boolean;
      --  Return whether S represent of a subrange of Reference with the
      --  same parent string.

   function Duplicate (S : Slice) return Slice;
      --  Create a new parent string and a slice designating it.
      --  This does not copy parts of S parent string outside of S range.
      --  Semantically equivalent to (To_Slice (To_String (S))).

private

   package String_Refs is new References
     (String,
      Storage_Pools.Access_In_Default_Pool'Storage_Pool,
      Storage_Pools.Access_In_Default_Pool'Storage_Pool);

   type Slice is tagged record
      Bounds : String_Range := (1, 0);
      Ref : String_Refs.Reference;
   end record;

   Null_Slice : constant Slice := ((1, 0), Ref => <>);

end Natools.String_Slices;
