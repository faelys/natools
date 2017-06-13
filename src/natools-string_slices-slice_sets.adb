------------------------------------------------------------------------------
-- Copyright (c) 2013-2016, Natacha Porté                                   --
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

with Ada.Strings.Fixed;

package body Natools.String_Slices.Slice_Sets is

   package Fixed renames Ada.Strings.Fixed;


   ---------------------------
   -- Range_Set subprograms --
   ---------------------------

   function Is_Overlapping (Bounds : String_Range; Set : Range_Set)
     return Boolean
   is
      Cursor : Range_Sets.Cursor := Set.Floor (Bounds);
   begin
      if Range_Sets.Has_Element (Cursor) then
         if Bounds.First <= Last (Range_Sets.Element (Cursor)) then
            return True;
         end if;

         Range_Sets.Next (Cursor);
      else
         Cursor := Set.First;
      end if;

      if Range_Sets.Has_Element (Cursor)
        and then Range_Sets.Element (Cursor).First <= Last (Bounds)
      then
         return True;
      end if;

      return False;
   end Is_Overlapping;


   function Is_Valid (Set : Range_Set) return Boolean is
      Cursor : Range_Sets.Cursor := Set.First;
      Prev, Cur : String_Range;
   begin
      if not Range_Sets.Has_Element (Cursor) then
         return True;
      end if;

      Prev := Range_Sets.Element (Cursor);

      if Prev.Length = 0 then
         return False;
      end if;

      Range_Sets.Next (Cursor);
      while Range_Sets.Has_Element (Cursor) loop
         Cur := Range_Sets.Element (Cursor);

         if Cur.Length = 0 then
            return False;
         end if;

         pragma Assert (Prev.First <= Cur.First);

         if Is_In (Last (Prev), Cur) then
            return False;
         end if;

         Prev := Cur;
         Range_Sets.Next (Cursor);
      end loop;

      return True;
   end Is_Valid;


   function Total_Span (Set : Range_Set) return String_Range is
      Result : String_Range := (1, 0);
      Cursor : Range_Sets.Cursor := Set.First;
   begin
      if not Range_Sets.Has_Element (Cursor) then
         return Result;
      end if;

      Result.First := Range_Sets.Element (Cursor).First;

      Cursor := Set.Last;
      Set_Last (Result, Last (Range_Sets.Element (Cursor)));

      return Result;
   end Total_Span;


   procedure Include_Range
     (Set : in out Range_Set; Bounds : in String_Range)
   is
      Cursor : Range_Sets.Cursor := Set.Floor (Bounds);
      Next : Range_Sets.Cursor;
      Actual : String_Range := Bounds;
      R : String_Range;
   begin
      if Range_Sets.Has_Element (Cursor) then
         R := Range_Sets.Element (Cursor);
         Next := Range_Sets.Next (Cursor);

         --  Do nothing if the given range is already covered

         if Is_Subrange (Actual, R) then
            return;
         end if;

         --  Merge with previous range if overlapping

         if Is_In (Actual.First, R) then
            Set_First (Actual, R.First);
            Set.Delete (Cursor);
         end if;
      else
         Next := Set.First;
      end if;

      while Range_Sets.Has_Element (Next) loop
         Cursor := Next;
         R := Range_Sets.Element (Cursor);
         exit when not Is_In (R.First, Actual);
         Next := Range_Sets.Next (Cursor);

         if Is_Subrange (R, Actual) then
            Set.Delete (Cursor);
         else
            pragma Assert (Last (R) > Last (Actual));
            Set_Last (Actual, Last (R));
            Set.Delete (Cursor);
         end if;
      end loop;

      Set.Insert (Actual);
      pragma Assert (Is_Valid (Set));
   end Include_Range;


   procedure Exclude_Range
     (Set : in out Range_Set; Bounds : in String_Range)
   is
      Cursor : Range_Sets.Cursor;
      R : String_Range;
   begin
      if Bounds.Length = 0 then
         return;
      end if;

      Cursor := Set.Floor (Bounds);

      if Range_Sets.Has_Element (Cursor) then
         R := Range_Sets.Element (Cursor);

         if R.First < Bounds.First then
            if Is_In (Bounds.First, R) then
               if Is_In (Last (Bounds) + 1, R) then
                  Set.Insert (To_Range (Last (Bounds) + 1, Last (R)));
               end if;

               Set_Last (R, Bounds.First - 1);
               pragma Assert (R.Length > 0);
               Set.Replace_Element (Cursor, R);
            end if;

            Range_Sets.Next (Cursor);
         end if;
      else
         Cursor := Set.First;
      end if;

      while Range_Sets.Has_Element (Cursor)
        and then Is_Subrange (Range_Sets.Element (Cursor), Bounds)
      loop
         declare
            Next : constant Range_Sets.Cursor := Range_Sets.Next (Cursor);
         begin
            Set.Delete (Cursor);
            Cursor := Next;
         end;
      end loop;

      if Range_Sets.Has_Element (Cursor)
        and then Is_In (Last (Bounds) + 1, Range_Sets.Element (Cursor))
      then
         R := Range_Sets.Element (Cursor);
         Set_First (R, Last (Bounds) + 1);
         Set.Replace_Element (Cursor, R);
      end if;

      pragma Assert (Is_Valid (Set));
   end Exclude_Range;



   -------------------------------
   -- Public helper subprograms --
   -------------------------------

   function "<" (Left, Right : String_Range) return Boolean is
   begin
      return Left.First < Right.First;
   end "<";



   ----------------------------
   -- Conversion subprograms --
   ----------------------------

   function To_Slice (S : Slice_Set) return Slice is
      use type Ada.Containers.Count_Type;
   begin
      if S.Ref.Is_Empty then
         return Null_Slice;
      end if;

      if S.Bounds.Is_Empty then
         return Slice'(Bounds => (1, 0),
                       Ref => S.Ref);
      elsif S.Bounds.Length = 1 then
         return Slice'(Bounds => S.Bounds.First_Element,
                       Ref => S.Ref);
      end if;

      return To_Slice (To_String (S));
   end To_Slice;


   function To_Slice_Set (S : String) return Slice_Set is
      function Factory return String;

      function Factory return String is
      begin
         return S;
      end Factory;

      Result : Slice_Set;
   begin
      Result.Ref := String_Refs.Create (Factory'Access);
      if S'Length > 0 then
         Result.Bounds.Insert ((S'First, S'Length));
      end if;
      return Result;
   end To_Slice_Set;


   function To_Slice_Set (S : Slice) return Slice_Set is
      Result : Slice_Set;
   begin
      Result.Ref := S.Ref;
      if S.Bounds.Length > 0 then
         Result.Bounds.Insert (S.Bounds);
      end if;
      return Result;
   end To_Slice_Set;


   function To_String (Set : Slice_Set) return String is
      Cursor : Range_Sets.Cursor := Set.Bounds.First;
      R : String_Range;
      I : Positive := 1;
   begin
      return Result : String (1 .. Set.Total_Length) do
         while Range_Sets.Has_Element (Cursor) loop
            R := Range_Sets.Element (Cursor);
            Result (I .. I + R.Length - 1)
              := Set.Ref.Query.Data.all (R.First .. Last (R));
            I := I + R.Length;
            Range_Sets.Next (Cursor);
         end loop;
         pragma Assert (I = Result'Last + 1);
      end return;
   end To_String;


   function To_String (Set : Slice_Set; Subrange : String_Range)
     return String is
   begin
      return Set.Subset (Subrange).To_String;
   end To_String;


   function To_String (Set : Slice_Set; First : Positive; Last : Natural)
     return String is
   begin
      return Set.Subset (To_Range (First, Last)).To_String;
   end To_String;



   ---------------------------------
   -- Basic slice-set subprograms --
   ---------------------------------

   procedure Clear (Set : in out Slice_Set) is
   begin
      Set.Bounds.Clear;
   end Clear;


   function Element (Set : Slice_Set; Index : Positive) return Character is
   begin
      if not Is_In (Set, Index) then
         raise Constraint_Error;
      end if;

      return Set.Ref.Query.Data.all (Index);
   end Element;


   function First (Set : Slice_Set) return Positive is
      Cursor : constant Range_Sets.Cursor := Set.Bounds.First;
   begin
      if Range_Sets.Has_Element (Cursor) then
         return Range_Sets.Element (Cursor).First;
      else
         return 1;
      end if;
   end First;


   function Is_Empty (Set : Slice_Set) return Boolean is
   begin
      return Set.Bounds.Is_Empty;
   end Is_Empty;


   function Is_In (Set : Slice_Set; Index : Natural) return Boolean is
      Cursor : Range_Sets.Cursor;
   begin
      if Index = 0 or else Set.Ref.Is_Empty or else Set.Bounds.Is_Empty then
         return False;
      end if;

      Cursor := Set.Bounds.Floor ((Index, 0));

      return Range_Sets.Has_Element (Cursor)
        and then Is_In (Index, Range_Sets.Element (Cursor));
   end Is_In;


   function Is_Null (Set : Slice_Set) return Boolean is
   begin
      return Set.Ref.Is_Empty;
   end Is_Null;


   function Is_Valid (Set : Slice_Set) return Boolean is
   begin
      if Set.Ref.Is_Empty then
         return Set.Bounds.Is_Empty;
      else
         return Is_Subrange (Total_Span (Set.Bounds),
                             Get_Range (Set.Ref.Query.Data.all))
           and then Is_Valid (Set.Bounds);
      end if;
   end Is_Valid;


   function Last (Set : Slice_Set) return Natural is
      Cursor : constant Range_Sets.Cursor := Set.Bounds.Last;
   begin
      if Range_Sets.Has_Element (Cursor) then
         return Last (Range_Sets.Element (Cursor));
      else
         return 0;
      end if;
   end Last;


--  Multistep version:
--   function Next (Set : Slice_Set; Index : Natural; Steps : Positive := 1)
--     return Natural
--   is
--      Cursor : Range_Sets.Cursor;
--      Target : Positive := Index + Steps;
--      Skipped : Natural;
--      R : String_Range;
--   begin
--      if Index = 0 or else Set.Ref.Is_Empty or else Set.Bounds.Is_Empty then
--         raise Constraint_Error;
--      end if;
--
--      Cursor := Set.Bounds.Floor ((Index, 0));
--
--      if not Range_Sets.Has_Element (Cursor) then
--         raise Constraint_Error with "Next with index out of bounds";
--      end if;
--
--      R := Range_Sets.Element (Cursor);
--      loop
--         if Is_In (Target, R) then
--            return Target;
--         end if;
--
--         Skipped := Last (R) + 1;
--         Range_Sets.Next (Cursor);
--         exit when not Range_Sets.Has_Element (Cursor);
--         R := Range_Sets.Element (Cursor);
--         Skipped := R.First - Skipped;
--         Target := Target + Skipped;
--      end loop;
--
--      return 0;
--   end Next;

   function Next (Set : Slice_Set; Index : Natural) return Natural is
      Cursor : Range_Sets.Cursor;
   begin
      if Index = 0 or else Set.Ref.Is_Empty or else Set.Bounds.Is_Empty then
         raise Constraint_Error;
      end if;

      Cursor := Set.Bounds.Floor ((Index, 0));

      if not Range_Sets.Has_Element (Cursor) then
         raise Constraint_Error with "Next with index out of bounds";
      end if;

      if Is_In (Index + 1, Range_Sets.Element (Cursor)) then
         return Index + 1;
      else
         Range_Sets.Next (Cursor);
         if Range_Sets.Has_Element (Cursor) then
            return Range_Sets.Element (Cursor).First;
         else
            return 0;
         end if;
      end if;
   end Next;


   procedure Next (Set : in Slice_Set; Index : in out Natural) is
   begin
      Index := Next (Set, Index);
   end Next;


--  Multistep version:
--  function Previous (Set : Slice_Set; Index : Natural; Steps : Positive := 1)
--     return Natural
--   is
--      Cursor : Range_Sets.Cursor;
--      Target : Positive;
--      Prev_First : Positive;
--      Skipped : Natural;
--      R : String_Range;
--   begin
--      if Index = 0 or else Set.Ref.Is_Empty or else Set.Bounds.Is_Empty then
--         raise Constraint_Error;
--      end if;
--
--      if Steps >= Index then
--         return 0;
--      end if;
--      Target := Index - Steps;
--
--      Cursor := Set.Bounds.Floor ((Index, 0));
--      if not Range_Sets.Has_Element (Cursor) then
--         raise Constraint_Error with "Previous with index out of bounds";
--      end if;
--
--      loop
--         R := Range_Sets.Element (Cursor);
--         if Is_In (Target, R) then
--            return Target;
--         end if;
--
--         Prev_First := R.First;
--         Range_Sets.Previous (Cursor);
--         exit when not Range_Sets.Has_Element (Cursor);
--         R := Range_Sets.Element (Cursor);
--
--         Skipped := Prev_First - (Last (R) + 1);
--         exit when Skipped >= Target;
--         Target := Target - Skipped;
--      end loop;
--
--      return 0;
--   end Previous;

   function Previous (Set : Slice_Set; Index : Natural) return Natural is
      Cursor : Range_Sets.Cursor;
   begin
      if Index = 0 or else Set.Ref.Is_Empty or else Set.Bounds.Is_Empty then
         raise Constraint_Error;
      end if;

      Cursor := Set.Bounds.Floor ((Index, 0));

      if not Range_Sets.Has_Element (Cursor) then
         raise Constraint_Error with "Previous with index out of bounds";
      end if;

      if Is_In (Index - 1, Range_Sets.Element (Cursor)) then
         return Index - 1;
      else
         Range_Sets.Previous (Cursor);
         if Range_Sets.Has_Element (Cursor) then
            return Last (Range_Sets.Element (Cursor));
         else
            return 0;
         end if;
      end if;
   end Previous;


   procedure Previous (Set : in Slice_Set; Index : in out Natural) is
   begin
      Index := Previous (Set, Index);
   end Previous;


   function Total_Length (Set : Slice_Set) return Natural is
      Cursor : Range_Sets.Cursor := Set.Bounds.First;
      Result : Natural := 0;
   begin
      while Range_Sets.Has_Element (Cursor) loop
         Result := Result + Range_Sets.Element (Cursor).Length;
         Range_Sets.Next (Cursor);
      end loop;

      return Result;
   end Total_Length;



   ----------------------------
   -- Operation on slice set --
   ----------------------------

   procedure Add_Slice (Set : in out Slice_Set; Bounds : in String_Range) is
   begin
      if Bounds.Length = 0 then
         return;
      end if;

      if Set.Ref.Is_Empty then
         raise Constraint_Error with "Cannot add range to null slice set";
      end if;

      if not Is_Subrange (Bounds, Get_Range (Set.Ref.Query.Data.all)) then
         raise Constraint_Error with "Add slice outside of parent";
      end if;

      if Is_Overlapping (Bounds, Set.Bounds) then
         raise Constraint_Error with "Add an overlapping slice to a set";
      end if;

      Set.Bounds.Insert (Bounds);
   end Add_Slice;


   procedure Add_Slice (Set : in out Slice_Set; S : in Slice) is
      use type String_Refs.Immutable_Reference;
   begin
      if S.Bounds.Length = 0 then
         return;
      end if;

      if Set.Ref.Is_Empty then
         pragma Assert (Set.Bounds.Is_Empty);
         Set.Ref := S.Ref;
         Set.Bounds.Insert (S.Bounds);
         return;
      end if;

      if Set.Ref /= S.Ref then
         raise Constraint_Error with
           "Addition of an unrelated slice to a slice set";
      end if;

      if Is_Overlapping (S.Bounds, Set.Bounds) then
         raise Constraint_Error with
           "Addition of an overlapping slice to a slice set";
      end if;

      Set.Bounds.Insert (S.Bounds);
   end Add_Slice;


   procedure Add_Slice
     (Set : in out Slice_Set;
      First : in Positive;
      Last : in Natural) is
   begin
      Add_Slice (Set, To_Range (First, Last));
   end Add_Slice;


   procedure Include_Slice
     (Set : in out Slice_Set; Bounds : in String_Range) is
   begin
      if Bounds.Length = 0 then
         return;
      end if;

      if Set.Ref.Is_Empty then
         raise Constraint_Error with "Cannot include range to null slice set";
      end if;

      if not Is_Subrange (Bounds, Get_Range (Set.Ref.Query.Data.all)) then
         raise Constraint_Error with "Include slice outside of parent";
      end if;

      Include_Range (Set.Bounds, Bounds);
   end Include_Slice;


   procedure Include_Slice (Set : in out Slice_Set; S : in Slice) is
      use type String_Refs.Immutable_Reference;
   begin
      if S.Bounds.Length = 0 then
         return;
      end if;

      if Set.Ref.Is_Empty then
         pragma Assert (Set.Bounds.Is_Empty);
         Set.Ref := S.Ref;
         Set.Bounds.Insert (S.Bounds);
         return;
      end if;

      if Set.Ref /= S.Ref then
         raise Constraint_Error with
           "Addition of an unrelated slice to a slice set";
      end if;

      Include_Range (Set.Bounds, S.Bounds);
   end Include_Slice;


   procedure Include_Slice
     (Set : in out Slice_Set;
      First : in Positive;
      Last : in Natural) is
   begin
      Include_Slice (Set, To_Range (First, Last));
   end Include_Slice;


   procedure Exclude_Slice
     (Set : in out Slice_Set; Bounds : in String_Range) is
   begin
      if Bounds.Length = 0 then
         return;
      end if;

      if Set.Ref.Is_Empty then
         raise Constraint_Error with
           "Cannot exclude range from null slice set";
      end if;

      Exclude_Range (Set.Bounds, Bounds);
   end Exclude_Slice;


   procedure Exclude_Slice
     (Set : in out Slice_Set;
      First : in Positive;
      Last : in Natural) is
   begin
      Exclude_Slice (Set, To_Range (First, Last));
   end Exclude_Slice;


   procedure Restrict (Set : in out Slice_Set; Bounds : in String_Range) is
   begin
      if Set.Ref.Is_Empty then
         raise Constraint_Error with "Cannot restrict null slice set";
      end if;

      if Bounds.Length = 0 then
         Set.Bounds.Clear;
      else
         declare
            Set_First : constant Positive := Set.First;
            Set_Last  : constant Natural  := Set.Last;
         begin
            if Set_First < Bounds.First then
               Exclude_Range
                 (Set.Bounds,
                  To_Range (Set_First, Bounds.First - 1));
            end if;

            if Set_Last > Last (Bounds) then
               Exclude_Range
                 (Set.Bounds,
                  To_Range (Last (Bounds) + 1, Set_Last));
            end if;
         end;
      end if;
   end Restrict;


   procedure Restrict
     (Set : in out Slice_Set;
      First : in Positive;
      Last : in Natural) is
   begin
      Restrict (Set, To_Range (First, Last));
   end Restrict;


   function Subset (Set : Slice_Set; Bounds : String_Range) return Slice_Set is
      Result : Slice_Set;
      Cursor : Range_Sets.Cursor;
      R : String_Range;
   begin
      if Set.Ref.Is_Empty then
         raise Constraint_Error with "Subset of null slice set";
      end if;

      Result.Ref := Set.Ref;

      if Bounds.Length = 0 or else Set.Bounds.Is_Empty then
         return Result;
      end if;

      Cursor := Set.Bounds.Floor (Bounds);
      if Range_Sets.Has_Element (Cursor) then
         R := Range_Sets.Element (Cursor);
         if R.First < Bounds.First then
            if Is_In (Bounds.First, R) then
               Set_First (R, Bounds.First);
               if Is_In (Last (Bounds), R) then
                  Set_Last (R, Last (Bounds));
               end if;
               Result.Bounds.Insert (R);
            end if;
            Range_Sets.Next (Cursor);
         end if;
      else
         Cursor := Set.Bounds.First;
      end if;

      while Range_Sets.Has_Element (Cursor) loop
         R := Range_Sets.Element (Cursor);

         if Is_Subrange (R, Bounds) then
            Result.Bounds.Insert (R);
         else
            if Is_In (Last (Bounds), R) then
               Set_Last (R, Last (Bounds));
               Result.Bounds.Insert (R);
            end if;
            exit;
         end if;

         Range_Sets.Next (Cursor);
      end loop;

      return Result;
   end Subset;


   function Subset (Set : Slice_Set; First : Positive; Last : Natural)
     return Slice_Set is
   begin
      return Subset (Set, To_Range (First, Last));
   end Subset;


   procedure Cut_Before (Set : in out Slice_Set; Index : in Positive) is
      Cursor : Range_Sets.Cursor;
      Lower, Upper : String_Range;
   begin
      if Set.Ref.Is_Empty or else Set.Bounds.Is_Empty then
         raise Constraint_Error;
      end if;

      Cursor := Set.Bounds.Floor ((Index, 0));

      if not Range_Sets.Has_Element (Cursor) then
         raise Constraint_Error;
      end if;

      Lower := Range_Sets.Element (Cursor);

      if not Is_In (Index, Lower) then
         raise Constraint_Error;
      end if;

      if Lower.First = Index then
         return;  --  nothing to do
      end if;

      Upper := Lower;
      Set_Last (Lower, Index - 1);
      Set_First (Upper, Index);
      Set.Bounds.Delete (Cursor);
      Set.Bounds.Insert (Lower);
      Set.Bounds.Insert (Upper);
   end Cut_Before;



   ---------------
   -- Iterators --
   ---------------

   procedure Trim_Slices
     (Set : in out Slice_Set;
      Trim : not null access function (Slice : String) return String_Range)
   is
      Cursor : Range_Sets.Cursor := Set.Bounds.First;
      Old_Range, New_Range : String_Range;
   begin
      while Range_Sets.Has_Element (Cursor) loop
         Old_Range := Range_Sets.Element (Cursor);
         New_Range := Trim.all
           (Set.Ref.Query.Data.all (Old_Range.First .. Last (Old_Range)));

         if New_Range.Length = 0 then
            declare
               Next : constant Range_Sets.Cursor := Range_Sets.Next (Cursor);
            begin
               Set.Bounds.Delete (Cursor);
               Cursor := Next;
            end;
         else
            if not Is_Subrange (New_Range, Old_Range) then
               raise Constraint_Error with "Trim not returning a subrange";
            end if;

            Set.Bounds.Replace_Element (Cursor, New_Range);
            Range_Sets.Next (Cursor);
         end if;
      end loop;
   end Trim_Slices;


   procedure Query_Slices
     (Set : in Slice_Set;
      Process : not null access procedure (S : in Slice))
   is
      Cursor : Range_Sets.Cursor := Set.Bounds.First;
   begin
      while Range_Sets.Has_Element (Cursor) loop
         Process.all (Slice'(Range_Sets.Element (Cursor), Set.Ref));
         Range_Sets.Next (Cursor);
      end loop;
   end Query_Slices;



   ----------------------
   -- Search functions --
   ----------------------

   function Find_Slice
     (Set : Slice_Set;
      From : Positive;
      Test : not null access function (Slice : String) return Boolean;
      Going : Ada.Strings.Direction := Ada.Strings.Forward)
     return String_Range
   is
      Cursor : Range_Sets.Cursor;
      Update : access procedure (C : in out Range_Sets.Cursor);
      R : String_Range;
   begin
      if Set.Ref.Is_Empty then
         raise Constraint_Error with "Find_Slice on null slice set";
      end if;

      case Going is
         when Ada.Strings.Forward  => Update := Range_Sets.Next'Access;
         when Ada.Strings.Backward => Update := Range_Sets.Previous'Access;
      end case;

      Cursor := Set.Bounds.Floor ((From, 0));

      while Range_Sets.Has_Element (Cursor) loop
         R := Range_Sets.Element (Cursor);

         if Test.all (Set.Ref.Query.Data.all (R.First .. Last (R))) then
            return R;
         end if;

         Update.all (Cursor);
      end loop;

      return (1, 0);
   end Find_Slice;


   function Find_Slice
     (Set : Slice_Set;
      Test : not null access function (Slice : String) return Boolean;
      Going : Ada.Strings.Direction := Ada.Strings.Forward)
     return String_Range is
   begin
      case Going is
         when Ada.Strings.Forward =>
            return Find_Slice (Set, Set.First, Test, Going);
         when Ada.Strings.Backward =>
            return Find_Slice (Set, Set.Last, Test, Going);
      end case;
   end Find_Slice;


   function Index
     (Source : Slice_Set;
      Set : Ada.Strings.Maps.Character_Set;
      From : Positive;
      Test : Ada.Strings.Membership := Ada.Strings.Inside;
      Going : Ada.Strings.Direction := Ada.Strings.Forward)
     return Natural
   is
      Cursor : Range_Sets.Cursor;
      Update : access procedure (C : in out Range_Sets.Cursor);
      R : String_Range;
      Result : Natural := 0;
   begin
      case Going is
         when Ada.Strings.Forward  => Update := Range_Sets.Next'Access;
         when Ada.Strings.Backward => Update := Range_Sets.Previous'Access;
      end case;

      Cursor := Source.Bounds.Floor ((From, 0));

      if not Range_Sets.Has_Element (Cursor) then
         raise Ada.Strings.Index_Error;
      end if;

      R := Range_Sets.Element (Cursor);

      if Is_In (From, R) then
         Result := Fixed.Index
           (Source.Ref.Query.Data.all (R.First .. Last (R)),
            Set,
            From,
            Test,
            Going);
      end if;

      while Result = 0 loop
         Update.all (Cursor);
         if not Range_Sets.Has_Element (Cursor) then
            return 0;
         end if;

         R := Range_Sets.Element (Cursor);
         Result := Fixed.Index
           (Source.Ref.Query.Data.all (R.First .. Last (R)),
            Set,
            Test,
            Going);
      end loop;

      return Result;
   end Index;


   function Index
     (Source : Slice_Set;
      Set : Ada.Strings.Maps.Character_Set;
      Test : Ada.Strings.Membership := Ada.Strings.Inside;
      Going : Ada.Strings.Direction := Ada.Strings.Forward)
     return Natural is
   begin
      case Going is
         when Ada.Strings.Forward =>
            return Index (Source, Set, Source.First, Test, Going);
         when Ada.Strings.Backward =>
            return Index (Source, Set, Source.Last, Test, Going);
      end case;
   end Index;

end Natools.String_Slices.Slice_Sets;
