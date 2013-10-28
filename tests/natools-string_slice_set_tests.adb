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

with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;

with Natools.String_Slices.Slice_Sets;

package body Natools.String_Slice_Set_Tests is

   package Slice_Sets renames Natools.String_Slices.Slice_Sets;

   Parent_String : constant String (11 .. 54)
     := "The quick brown fox jumps over the lazy dog.";
   --   .123456789.123456789.123456789.123456789.1234
   --   1         2         3         4         5

   procedure Info_Fail
     (Report : in out NT.Reporter'Class;
      Name : in String;
      Reported : in out Boolean;
      Info : in String);
      --  Report failure if not already reported, and append Info

   procedure Dump
     (Report : in out NT.Reporter'Class;
      Set : in Slice_Sets.Slice_Set);
      --  Dump the given slice set in Report.Info



   ------------------------
   -- Helper subprograms --
   ------------------------

   procedure Dump
     (Report : in out NT.Reporter'Class;
      Set : in Slice_Sets.Slice_Set)
   is
      procedure Range_Image (Slice : in String_Slices.Slice);

      Parent, Image : Ada.Strings.Unbounded.Unbounded_String;
      Parent_Range : String_Slices.String_Range;
      Parent_Seen : Boolean := False;

      procedure Range_Image (Slice : in String_Slices.Slice) is
      begin
         if not Parent_Seen then
            Parent := Ada.Strings.Unbounded.To_Unbounded_String
              (Slice.Parent.To_String);
            Parent_Range := Slice.Parent.Get_Range;
            Parent_Seen := True;
         end if;

         Ada.Strings.Unbounded.Append (Image, " ");
         Ada.Strings.Unbounded.Append
           (Image,
            String_Slices.Image (Slice.Get_Range));
      end Range_Image;
   begin
      Set.Query_Slices (Range_Image'Access);

      Report.Info ("Parent: "
         & String_Slices.Image (Parent_Range)
         & " """ & Ada.Strings.Unbounded.To_String (Parent) & '"');
      Report.Info ("Slices:" & Ada.Strings.Unbounded.To_String (Image));
   end Dump;


   procedure Info_Fail
     (Report : in out NT.Reporter'Class;
      Name : in String;
      Reported : in out Boolean;
      Info : in String) is
   begin
      if not Reported then
         Report.Item (Name, NT.Fail);
         Reported := True;
      end if;

      Report.Info (Info);
   end Info_Fail;



   ----------------------
   -- Test collections --
   ----------------------

   procedure All_Tests (Report : in out NT.Reporter'Class) is
   begin
      Test_Conversions (Report);
      Test_Exceptions (Report);
      Test_Interval_Arithmetic (Report);
      Test_Navigation (Report);
      Test_Reductions (Report);
      Test_Slices (Report);
      Test_Tokenization (Report);
   end All_Tests;



   ----------------------
   -- Individual tests --
   ----------------------

   procedure Test_Conversions (Report : in out NT.Reporter'Class) is
      Name : constant String
        := "Conversions between slices, slice sets and strings";
      Reported : Boolean := False;
   begin
      declare
         use type String_Slices.Slice;
         use type Slice_Sets.Slice_Set;

         Parent_Slice : constant String_Slices.Slice
           := String_Slices.To_Slice (Parent_String);
         Range_1 : constant String_Slices.String_Range
           := (Parent_String'First, 10);
         Range_2 : constant String_Slices.String_Range
           := (Parent_String'Last - 9, 10);
         Range_3 : constant String_Slices.String_Range
           := String_Slices.To_Range
               (Parent_String'First + 6, Parent_String'Last - 7);
         Set : Slice_Sets.Slice_Set;
      begin
         if not Set.Is_Valid then
            Info_Fail (Report, Name, Reported, "Default set is invalid");
            Dump (Report, Set);
         end if;

         if not Set.Is_Null then
            Info_Fail (Report, Name, Reported, "Default set is not null");
            Dump (Report, Set);
         end if;

         if not Set.Is_Empty then
            Info_Fail (Report, Name, Reported, "Null set is not empty");
            Dump (Report, Set);
         end if;

         if Slice_Sets.To_Slice_Set (String_Slices.Null_Slice) /= Set then
            Info_Fail (Report, Name, Reported,
              "To_Slice_Set (Null_Slice) /= Null_Set");
         end if;

         if Set.To_Slice /= String_Slices.Null_Slice then
            Info_Fail (Report, Name, Reported,
              "To_Slice (Null_Set) /= Null_Slice");
         end if;

         if Set.To_String /= "" then
            Info_Fail (Report, Name, Reported, "To_String (Null_Set) /= """"");
         end if;

         Set := Slice_Sets.To_Slice_Set (Parent_Slice);
         Set.Clear;

         if Set.Is_Null then
            Info_Fail (Report, Name, Reported, "Cleared set is null");
         end if;

         if not Set.Is_Empty then
            Info_Fail (Report, Name, Reported, "Cleared set is not empty");
            Dump (Report, Set);
         end if;

         if Set.To_String /= "" then
            Info_Fail (Report, Name, Reported,
              "To_String (Empty_Set) /= """"");
         end if;

         if not String_Slices.Is_Subslice (Set.To_Slice, Parent_Slice) then
            Info_Fail (Report, Name, Reported,
              "To_Slice (Empty_Set) is not a subslice of parent.");
         end if;

         Set.Add_Slice (Range_1);

         if not Set.Is_Valid then
            Info_Fail (Report, Name, Reported, "Invalid slice singleton");
            Dump (Report, Set);
         end if;

         if Set.Is_Null then
            Info_Fail (Report, Name, Reported, "Slice singleton is null");
         end if;

         if Set.Is_Empty then
            Info_Fail (Report, Name, Reported, "Slice singleton is empty");
         end if;

         if Set.To_String
           /= Parent_String (Range_1.First .. String_Slices.Last (Range_1))
         then
            Info_Fail (Report, Name, Reported,
              "Found """ & Set.To_String & '"');
            Info_Fail (Report, Name, Reported, "Expected """
              & Parent_String (Range_1.First .. String_Slices.Last (Range_1))
              & '"');
            Dump (Report, Set);
         end if;

         if not String_Slices.Is_Subslice (Set.To_Slice, Parent_Slice) then
            Info_Fail (Report, Name, Reported,
               "To_Slice (Singleton) is not a subslice of parent.");
         end if;

         Set.Add_Slice (Range_2);

         if not Set.Is_Valid then
            Info_Fail (Report, Name, Reported, "Invalid normal set");
            Dump (Report, Set);
         end if;

         if Set.Is_Null then
            Info_Fail (Report, Name, Reported, "Normal set is null");
         end if;

         if Set.Is_Empty then
            Info_Fail (Report, Name, Reported, "Normal set is empty");
         end if;

         if Set.To_String
           /= Parent_String (Range_1.First .. String_Slices.Last (Range_1))
              & Parent_String (Range_2.First .. String_Slices.Last (Range_2))
         then
            Info_Fail (Report, Name, Reported,
              "Found """ & Set.To_String & '"');
            Info_Fail (Report, Name, Reported, "Expected """
              & Parent_String (Range_1.First .. String_Slices.Last (Range_1))
              & Parent_String (Range_2.First .. String_Slices.Last (Range_2))
              & '"');
            Dump (Report, Set);
         end if;

         if String_Slices.Is_Related (Set.To_Slice, Parent_Slice) then
            Info_Fail (Report, Name, Reported,
              "To_Slice (Complex_Set) related to parent slice");
            Dump (Report, Set);
         end if;

         if Set.To_String (Range_3)
           /= Parent_String (Range_3.First .. String_Slices.Last (Range_1))
              & Parent_String (Range_2.First .. String_Slices.Last (Range_3))
         then
            Info_Fail (Report, Name, Reported,
              "Found """ & Set.To_String (Range_3) & '"');
            Info_Fail (Report, Name, Reported, "Expected """
              & Parent_String (Range_3.First .. String_Slices.Last (Range_1))
              & Parent_String (Range_2.First .. String_Slices.Last (Range_3))
              & '"');
            Dump (Report, Set);
         end if;

         if Set.To_String (Range_3.First, String_Slices.Last (Range_3))
           /= Parent_String (Range_3.First .. String_Slices.Last (Range_1))
              & Parent_String (Range_2.First .. String_Slices.Last (Range_3))
         then
            Info_Fail (Report, Name, Reported, "Found """
              & Set.To_String (Range_3.First, String_Slices.Last (Range_3))
              & '"');
            Info_Fail (Report, Name, Reported, "Expected """
              & Parent_String (Range_3.First .. String_Slices.Last (Range_1))
              & Parent_String (Range_2.First .. String_Slices.Last (Range_3))
              & '"');
            Dump (Report, Set);
         end if;
      end;

      if not Reported then
         Report.Item (Name, NT.Success);
      end if;
   exception
      when Error : others => Report.Report_Exception (Name, Error);
   end Test_Conversions;


   procedure Test_Exceptions (Report : in out NT.Reporter'Class) is
      Parent_Slice : constant String_Slices.Slice
        := String_Slices.To_Slice (Parent_String);
      Set : Slice_Sets.Slice_Set;
   begin
      Report.Section ("Exceptions raised");

      declare
         Name : constant String := "Add_Slice to null set";
      begin
         Set.Add_Slice (15, 19);
         Report.Item (Name, NT.Fail);
         Report.Info ("No exception has been raised.");
      exception
         when Constraint_Error =>
            Report.Item (Name, NT.Success);
         when Error : others =>
            Report.Item (Name, NT.Fail);
            Report.Info ("Wrong exception "
              & Ada.Exceptions.Exception_Name (Error)
              & " has been raised.");
      end;

      declare
         Name : constant String := "Exclude_Slice from null set";
      begin
         Set.Exclude_Slice (15, 19);
         Report.Item (Name, NT.Fail);
         Report.Info ("No exception has been raised.");
      exception
         when Constraint_Error =>
            Report.Item (Name, NT.Success);
         when Error : others =>
            Report.Item (Name, NT.Fail);
            Report.Info ("Wrong exception "
              & Ada.Exceptions.Exception_Name (Error)
              & " has been raised.");
      end;

      declare
         Name : constant String := "Include_Slice to null set";
      begin
         Set.Include_Slice (15, 19);
         Report.Item (Name, NT.Fail);
         Report.Info ("No exception has been raised.");
      exception
         when Constraint_Error =>
            Report.Item (Name, NT.Success);
         when Error : others =>
            Report.Item (Name, NT.Fail);
            Report.Info ("Wrong exception "
              & Ada.Exceptions.Exception_Name (Error)
              & " has been raised.");
      end;

      declare
         Name : constant String := "Restrict null set";
      begin
         Set.Restrict (15, 19);
         Report.Item (Name, NT.Fail);
         Report.Info ("No exception has been raised.");
      exception
         when Constraint_Error =>
            Report.Item (Name, NT.Success);
         when Error : others =>
            Report.Item (Name, NT.Fail);
            Report.Info ("Wrong exception "
              & Ada.Exceptions.Exception_Name (Error)
              & " has been raised.");
      end;

      declare
         Name : constant String := "Subset of null set";
         New_Set : Slice_Sets.Slice_Set;
      begin
         New_Set := Set.Subset (15, 19);
         Report.Item (Name, NT.Fail);
         Report.Info ("No exception has been raised.");
         Report.Info ("Result: """ & New_Set.To_String & '"');
      exception
         when Constraint_Error =>
            Report.Item (Name, NT.Success);
         when Error : others =>
            Report.Item (Name, NT.Fail);
            Report.Info ("Wrong exception "
              & Ada.Exceptions.Exception_Name (Error)
              & " has been raised.");
      end;

      declare
         Name : constant String := "Cut_Before in null set";
      begin
         Set.Cut_Before (15);
         Report.Item (Name, NT.Fail);
         Report.Info ("No exception has been raised.");
      exception
         when Constraint_Error =>
            Report.Item (Name, NT.Success);
         when Error : others =>
            Report.Item (Name, NT.Fail);
            Report.Info ("Wrong exception "
              & Ada.Exceptions.Exception_Name (Error)
              & " has been raised.");
      end;

      declare
         Name : constant String := "Find_Slice in null set";

         function Find_Anything (Slice : String) return Boolean;

         function Find_Anything (Slice : String) return Boolean is
            pragma Unreferenced (Slice);
         begin
            return True;
         end Find_Anything;

         Result : String_Slices.String_Range;
      begin
         Result := Set.Find_Slice (Find_Anything'Access);
         Report.Item (Name, NT.Fail);
         Report.Info ("No exception has been raised.");
         Report.Info ("Result: " & String_Slices.Image (Result));
      exception
         when Constraint_Error =>
            Report.Item (Name, NT.Success);
         when Error : others =>
            Report.Item (Name, NT.Fail);
            Report.Info ("Wrong exception "
              & Ada.Exceptions.Exception_Name (Error)
              & " has been raised.");
      end;

      Set := Slice_Sets.To_Slice_Set (Parent_Slice);
      Set.Exclude_Slice (20, 29);

      declare
         Name : constant String := "Incorrect Index in Element";
         C : Character;
      begin
         C := Set.Element (25);
         Report.Item (Name, NT.Fail);
         Report.Info ("No exception has been raised.");
         Report.Info ("Return value: " & Character'Image (C));
      exception
         when Constraint_Error =>
            Report.Item (Name, NT.Success);
         when Error : others =>
            Report.Item (Name, NT.Fail);
            Report.Info ("Wrong exception "
              & Ada.Exceptions.Exception_Name (Error)
              & " has been raised.");
      end;

      declare
         Name : constant String := "Include_Slice outside of parent string";
      begin
         Set.Include_Slice (Parent_String'Last + 10, Parent_String'Last + 19);
         Report.Item (Name, NT.Fail);
         Report.Info ("No exception has been raised.");
      exception
         when Constraint_Error =>
            Report.Item (Name, NT.Success);
         when Error : others =>
            Report.Item (Name, NT.Fail);
            Report.Info ("Wrong exception "
              & Ada.Exceptions.Exception_Name (Error)
              & " has been raised.");
      end;

      declare
         Name : constant String := "Add_Slice outside of parent string";
      begin
         Set.Add_Slice (Parent_String'Last + 10, Parent_String'Last + 19);
         Report.Item (Name, NT.Fail);
         Report.Info ("No exception has been raised.");
      exception
         when Constraint_Error =>
            Report.Item (Name, NT.Success);
         when Error : others =>
            Report.Item (Name, NT.Fail);
            Report.Info ("Wrong exception "
              & Ada.Exceptions.Exception_Name (Error)
              & " has been raised.");
      end;

      declare
         Name : constant String := "Add_Slice with overlapping range";
      begin
         Set.Add_Slice (15, 24);
         Report.Item (Name, NT.Fail);
         Report.Info ("No exception has been raised.");
      exception
         when Constraint_Error =>
            Report.Item (Name, NT.Success);
         when Error : others =>
            Report.Item (Name, NT.Fail);
            Report.Info ("Wrong exception "
              & Ada.Exceptions.Exception_Name (Error)
              & " has been raised.");
      end;

      declare
         Name : constant String := "Add_Slice with overlapping slice";
      begin
         Set.Add_Slice (Parent_Slice.Subslice (15, 24));
         Report.Item (Name, NT.Fail);
         Report.Info ("No exception has been raised.");
      exception
         when Constraint_Error =>
            Report.Item (Name, NT.Success);
         when Error : others =>
            Report.Item (Name, NT.Fail);
            Report.Info ("Wrong exception "
              & Ada.Exceptions.Exception_Name (Error)
              & " has been raised.");
      end;

      declare
         Name : constant String := "Unrelated Add_Slice";
      begin
         Set.Add_Slice (String_Slices.To_Slice (Parent_String (22 .. 26)));
         Report.Item (Name, NT.Fail);
         Report.Info ("No exception has been raised.");
      exception
         when Constraint_Error =>
            Report.Item (Name, NT.Success);
         when Error : others =>
            Report.Item (Name, NT.Fail);
            Report.Info ("Wrong exception "
              & Ada.Exceptions.Exception_Name (Error)
              & " has been raised.");
      end;

      declare
         Name : constant String := "Unrelated Include_Slice";
      begin
         Set.Include_Slice (String_Slices.To_Slice (Parent_String (22 .. 26)));
         Report.Item (Name, NT.Fail);
         Report.Info ("No exception has been raised.");
      exception
         when Constraint_Error =>
            Report.Item (Name, NT.Success);
         when Error : others =>
            Report.Item (Name, NT.Fail);
            Report.Info ("Wrong exception "
              & Ada.Exceptions.Exception_Name (Error)
              & " has been raised.");
      end;

      declare
         Name : constant String := "Cut_Before before first";
      begin
         Set.Cut_Before (5);
         Report.Item (Name, NT.Fail);
         Report.Info ("No exception has been raised.");
      exception
         when Constraint_Error =>
            Report.Item (Name, NT.Success);
         when Error : others =>
            Report.Item (Name, NT.Fail);
            Report.Info ("Wrong exception "
              & Ada.Exceptions.Exception_Name (Error)
              & " has been raised.");
      end;

      declare
         Name : constant String := "Cut_Before after last";
      begin
         Set.Cut_Before (Parent_String'Last + 5);
         Report.Item (Name, NT.Fail);
         Report.Info ("No exception has been raised.");
      exception
         when Constraint_Error =>
            Report.Item (Name, NT.Success);
         when Error : others =>
            Report.Item (Name, NT.Fail);
            Report.Info ("Wrong exception "
              & Ada.Exceptions.Exception_Name (Error)
              & " has been raised.");
      end;

      declare
         Name : constant String := "Extender callback in Trim_Slices";

         function Extender (Slice : String) return String_Slices.String_Range;

         function Extender (Slice : String)
           return String_Slices.String_Range is
         begin
            return String_Slices.To_Range (Slice'First, Slice'Last + 1);
         end Extender;
      begin
         Set.Trim_Slices (Extender'Access);
         Report.Item (Name, NT.Fail);
         Report.Info ("No exception has been raised.");
      exception
         when Constraint_Error =>
            Report.Item (Name, NT.Success);
         when Error : others =>
            Report.Item (Name, NT.Fail);
            Report.Info ("Wrong exception "
              & Ada.Exceptions.Exception_Name (Error)
              & " has been raised.");
      end;

      declare
         Name : constant String := "Function Index with invalid From";
         N : Natural;
      begin
         N := Set.Index (Ada.Strings.Maps.To_Set (" "), 5);
         Report.Item (Name, NT.Fail);
         Report.Info ("No exception has been raised.");
         Report.Info ("Return value:" & Integer'Image (N));
      exception
         when Ada.Strings.Index_Error =>
            Report.Item (Name, NT.Success);
         when Error : others =>
            Report.Item (Name, NT.Fail);
            Report.Info ("Wrong exception "
              & Ada.Exceptions.Exception_Name (Error)
              & " has been raised.");
      end;

      Set.Clear;

      declare
         Name : constant String := "Empty_Set.Next";
         N : Natural;
      begin
         N := Set.Next (15);
         Report.Item (Name, NT.Fail);
         Report.Info ("No exception has been raised.");
         Report.Info ("Return value:" & Integer'Image (N));
      exception
         when Constraint_Error =>
            Report.Item (Name, NT.Success);
         when Error : others =>
            Report.Item (Name, NT.Fail);
            Report.Info ("Wrong exception "
              & Ada.Exceptions.Exception_Name (Error)
              & " has been raised.");
      end;

      declare
         Name : constant String := "Empty_Set.Previous";
         N : Natural;
      begin
         N := Set.Previous (15);
         Report.Item (Name, NT.Fail);
         Report.Info ("No exception has been raised.");
         Report.Info ("Return value:" & Integer'Image (N));
      exception
         when Constraint_Error =>
            Report.Item (Name, NT.Success);
         when Error : others =>
            Report.Item (Name, NT.Fail);
            Report.Info ("Wrong exception "
              & Ada.Exceptions.Exception_Name (Error)
              & " has been raised.");
      end;

      Set.Include_Slice (Parent_String'First, Parent_String'Last);

      declare
         Name : constant String := "Invalid input Index for Next";
         N : Natural;
      begin
         N := Set.Next (5);
         Report.Item (Name, NT.Fail);
         Report.Info ("No exception has been raised.");
         Report.Info ("Return value:" & Integer'Image (N));
      exception
         when Constraint_Error =>
            Report.Item (Name, NT.Success);
         when Error : others =>
            Report.Item (Name, NT.Fail);
            Report.Info ("Wrong exception "
              & Ada.Exceptions.Exception_Name (Error)
              & " has been raised.");
      end;

      declare
         Name : constant String := "Invalid input Index for Previous";
         N : Natural;
      begin
         N := Set.Previous (5);
         Report.Item (Name, NT.Fail);
         Report.Info ("No exception has been raised.");
         Report.Info ("Return value:" & Integer'Image (N));
      exception
         when Constraint_Error =>
            Report.Item (Name, NT.Success);
         when Error : others =>
            Report.Item (Name, NT.Fail);
            Report.Info ("Wrong exception "
              & Ada.Exceptions.Exception_Name (Error)
              & " has been raised.");
      end;

      Report.End_Section;
   end Test_Exceptions;


   procedure Test_Interval_Arithmetic (Report : in out NT.Reporter'Class) is
      Name : constant String := "Interval arithmetic";
      Reported : Boolean := False;

      procedure Check_Set
        (Parent : in String;
         Slices : in Slice_Sets.Slice_Set;
         Contents : in Ada.Strings.Maps.Character_Set);
         --  Check that slice inclusion is equivalent to character set

      procedure Check_Set
        (Parent : in String;
         Slices : in Slice_Sets.Slice_Set;
         Contents : in Ada.Strings.Maps.Character_Set)
      is
         Failure : Natural := 0;
      begin
         for I in Parent'Range loop
            if Ada.Strings.Maps.Is_In (Parent (I), Contents)
              /= Slices.Is_In (I)
            then
               Failure := I;
               exit;
            end if;
         end loop;

         if Failure > 0 then
            if not Reported then
               Report.Item (Name, NT.Fail);
               Reported := True;
            end if;

            Report.Info ("Expected set: "
              & Ada.Strings.Maps.To_Sequence (Contents));
            Dump (Report, Slices);
         end if;
      end Check_Set;
   begin
      declare
         Parent : constant String (11 .. 36)
           := "ABCDEFabc1234567890zyxWXYZ";
         --    123456789.123456789.123456
         Set : Slice_Sets.Slice_Set := Slice_Sets.To_Slice_Set (Parent);
      begin
         Set.Clear;
         Check_Set (Parent, Set, Ada.Strings.Maps.Null_Set);

         Set.Include_Slice (20, 29);
         Check_Set (Parent, Set, Ada.Strings.Maps.To_Set ("0123456789"));

         Set.Include_Slice (20, 25);
         Check_Set (Parent, Set, Ada.Strings.Maps.To_Set ("0123456789"));

         Set.Include_Slice (11, 16);
         Check_Set (Parent, Set, Ada.Strings.Maps.To_Set ("0123456789ABCDEF"));

         Set.Include_Slice (14, 32);
         Check_Set (Parent, Set,
           Ada.Strings.Maps.To_Set ("0123456789ABCDEFabczyx"));

         Set.Include_Slice (33, 36);
         Check_Set (Parent, Set, Ada.Strings.Maps.To_Set (Parent));

         Set.Exclude_Slice (20, 29);
         Check_Set (Parent, Set, Ada.Strings.Maps.To_Set ("ABCDEFabczyxWXYZ"));

         Set.Include_Slice (27, 34);
         Check_Set (Parent, Set,
           Ada.Strings.Maps.To_Set ("ABCDEFabc890zyxWXYZ"));

         Set.Exclude_Slice (14, 13);
         Check_Set (Parent, Set,
           Ada.Strings.Maps.To_Set ("ABCDEFabc890zyxWXYZ"));

         Set.Exclude_Slice (5, 23);
         Check_Set (Parent, Set, Ada.Strings.Maps.To_Set ("890zyxWXYZ"));

         Set.Include_Slice (13, 12);
         Check_Set (Parent, Set, Ada.Strings.Maps.To_Set ("890zyxWXYZ"));
      end;

      if not Reported then
         Report.Item (Name, NT.Success);
      end if;
   exception
      when Error : others => Report.Report_Exception (Name, Error);
   end Test_Interval_Arithmetic;


   procedure Test_Reductions (Report : in out NT.Reporter'Class) is
      Name : constant String := "Slice set reductions";
      Reported : Boolean := False;
   begin
      declare
         use type Slice_Sets.Slice_Set;

         Set : Slice_Sets.Slice_Set := Slice_Sets.To_Slice_Set (Parent_String);
         Subset : Slice_Sets.Slice_Set;
         First : constant Positive := 21;
         Last : constant Positive := 50;
         Middle_First : constant Positive := 30;
         Middle_Last : constant Positive := 39;
         Expected_String : constant String
           := Parent_String (First .. Middle_First - 1)
            & Parent_String (Middle_Last + 1 .. Last);
      begin
         Set.Exclude_Slice (Middle_First, Middle_Last);
         Set.Restrict (First, Last);

         if Set.To_String /= Expected_String then
            Info_Fail (Report, Name, Reported,
              "Expected """ & Expected_String & '"');
            Dump (Report, Set);
         end if;

         Set.Restrict (First, First - 1);
         if Set.Is_Null or not Set.Is_Empty then
            Info_Fail (Report, Name, Reported, "Expected empty set");
            Dump (Report, Set);
         end if;

         Subset := Set.Subset (Parent_String'First, Parent_String'Last);
         if Subset.Is_Null or not Set.Is_Empty then
            Info_Fail (Report, Name, Reported, "Expected empty subset");
            Dump (Report, Subset);
         end if;

         Set.Add_Slice (First, Last);
         Set.Exclude_Slice (Middle_First, Middle_Last);

         Subset := Set.Subset (Parent_String'First, Last);
         if Subset /= Set then
            Info_Fail (Report, Name, Reported, "Expected equal subset");
            Dump (Report, Set);
            Dump (Report, Subset);
         end if;

         Subset := Set.Subset (First + 1, Middle_First - 2);
         if Subset.To_String
           /= Parent_String (First + 1 .. Middle_First - 2)
         then
            Info_Fail (Report, Name, Reported, "Expected """
              & Parent_String (First + 1 .. Middle_First - 2) & '"');
            Dump (Report, Subset);
         end if;
      end;

      if not Reported then
         Report.Item (Name, NT.Success);
      end if;
   exception
      when Error : others => Report.Report_Exception (Name, Error);
   end Test_Reductions;


   procedure Test_Navigation (Report : in out NT.Reporter'Class) is
      Name : constant String := "External index navigation";
   begin
      declare
         Set : Slice_Sets.Slice_Set := Slice_Sets.To_Slice_Set (Parent_String);
         Middle_First : constant Positive := 20;
         Middle_Last : constant Natural := 29;
         Index, Previous, Expected, Set_Last : Positive;
      begin
         Set.Exclude_Slice (Middle_First, Middle_Last);
         Set.Cut_Before (45);
         Index := Set.First;
         Set_Last := Set.Last;

         if Index /= Parent_String'First then
            Report.Item (Name, NT.Fail);
            Report.Info
              ("First index is" & Integer'Image (Index)
               & ", expected" & Integer'Image (Parent_String'First));
            Dump (Report, Set);
            return;
         end if;

         if Set.Previous (Index) /= 0 then
            Report.Item (Name, NT.Fail);
            Report.Info
              ("Unexpected predecessor of First: "
               & Integer'Image (Set.Previous (Index)));
            Dump (Report, Set);
            return;
         end if;

         if Set_Last /= Parent_String'Last then
            Report.Item (Name, NT.Fail);
            Report.Info
              ("Last index is" & Integer'Image (Set_Last)
               & ", expected" & Integer'Image (Parent_String'Last));
            Dump (Report, Set);
            return;
         end if;

         if Set.Next (Set_Last) /= 0 then
            Report.Item (Name, NT.Fail);
            Report.Info
              ("Unexpected successor of Last: "
               & Integer'Image (Set.Next (Set_Last)));
            Dump (Report, Set);
            return;
         end if;

         loop
            if Set.Element (Index) /= Parent_String (Index) then
               Report.Item (Name, NT.Fail);
               Report.Info
                 ("Content mismatch at" & Integer'Image (Index)
                  & ": " & Character'Image (Set.Element (Index))
                  & " instead of " & Character'Image (Parent_String (Index)));
               Dump (Report, Set);
               return;
            end if;

            exit when Index >= Set_Last;

            Previous := Index;
            Expected := Index + 1;
            if Expected in Middle_First .. Middle_Last then
               Expected := Middle_Last + 1;
            end if;

            Set.Next (Index);

            if Index <= Previous then
               Report.Item (Name, NT.Fail);
               Report.Info
                 ("Next updated index from" & Integer'Image (Previous)
                  & " to" & Integer'Image (Index));
               Dump (Report, Set);
               return;
            end if;

            if Index /= Expected then
               Report.Item (Name, NT.Fail);
               Report.Info
                 ("Index after" & Integer'Image (Previous)
                  & " is" & Integer'Image (Index)
                  & ", expected" & Integer'Image (Expected));
               Dump (Report, Set);
               return;
            end if;

            Set.Previous (Expected);

            if Previous /= Expected then
               Report.Item (Name, NT.Fail);
               Report.Info
                 ("Index before" & Integer'Image (Index)
                  & " is" & Integer'Image (Expected)
                  & ", expected" & Integer'Image (Previous));
               Dump (Report, Set);
               return;
            end if;
         end loop;
      end;

      Report.Item (Name, NT.Success);
   exception
      when Error : others => Report.Report_Exception (Name, Error);
   end Test_Navigation;


   procedure Test_Slices (Report : in out NT.Reporter'Class) is
      Name : constant String := "Slice-based operations";
      Parent : constant String_Slices.Slice
        := String_Slices.To_Slice (Parent_String);
      Unrelated : constant String_Slices.Slice
        := String_Slices.To_Slice (Name);
      Set_1, Set_2, Set_3 : Slice_Sets.Slice_Set;

      use type Slice_Sets.Slice_Set;
   begin
      Set_1.Include_Slice (Unrelated.Subslice (10, 9));
      Set_1.Add_Slice (Parent);
      Set_2.Add_Slice (Parent.Subslice (20, 29));
      Set_2.Add_Slice (Unrelated.Subslice (25, 24));
      Set_2.Include_Slice (Parent);
      Set_3.Include_Slice (Parent.Subslice (20, 29));
      Set_3.Add_Slice (Parent.Subslice (35, 39));
      Set_3.Include_Slice (Parent);

      if Set_1 = Set_2 and Set_1 = Set_3 then
         Report.Item (Name, NT.Success);
      else
         Report.Item (Name, NT.Fail);
         Report.Info ("Add_Slice and Include_Slice not equivalent");
         Dump (Report, Set_1);
         Dump (Report, Set_2);
         Dump (Report, Set_3);
      end if;
   exception
      when Error : others => Report.Report_Exception (Name, Error);
   end Test_Slices;


   procedure Test_Tokenization (Report : in out NT.Reporter'Class) is
      Name : constant String := "Simple tokenization";
      Space : constant Ada.Strings.Maps.Character_Set
        := Ada.Strings.Maps.To_Set (" ");

      function Has_Spaces (S : String) return Boolean;
      function Trim_Spaces (S : String) return String_Slices.String_Range;


      function Has_Spaces (S : String) return Boolean is
      begin
         return Ada.Strings.Fixed.Index (S, Space) > 0;
      end Has_Spaces;

      function Trim_Spaces (S : String) return String_Slices.String_Range is
         Result : String_Slices.String_Range;
         N : Natural;
      begin
         N := Ada.Strings.Fixed.Index (S, Space, Ada.Strings.Outside);
         if N = 0 then
            return (1, 0);
         end if;
         Result.First := N;

         N := Ada.Strings.Fixed.Index
           (S, Space, Ada.Strings.Outside, Ada.Strings.Backward);
         String_Slices.Set_Last (Result, N);

         return Result;
      end Trim_Spaces;
   begin
      declare
         Set : Slice_Sets.Slice_Set := Slice_Sets.To_Slice_Set (Parent_String);
         R : String_Slices.String_Range;
         N : Natural;
      begin
         N := Set.Index (Space);
         while N > 0 loop
            Set.Cut_Before (N);
            N := Set.Index (Space, N + 1);
         end loop;

         Set.Trim_Slices (Trim_Spaces'Access);

         N := Set.Index (Space, Going => Ada.Strings.Backward);
         if N /= 0 then
            Report.Item (Name, NT.Fail);
            Report.Info ("Unexpected space at" & Integer'Image (N));
            Dump (Report, Set);
            return;
         end if;

         R := Set.Find_Slice (Has_Spaces'Access);
         if R.First /= 1 or R.Length /= 0 then
            Report.Item (Name, NT.Fail);
            Report.Info ("Unxpected slice found at "
              & String_Slices.Image (R));
            Dump (Report, Set);
            return;
         end if;
      end;

      Report.Item (Name, NT.Success);
   exception
      when Error : others => Report.Report_Exception (Name, Error);
   end Test_Tokenization;

end Natools.String_Slice_Set_Tests;
