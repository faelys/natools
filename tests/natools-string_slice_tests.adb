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
with Natools.String_Slices;

package body Natools.String_Slice_Tests is

   Parent_String : constant String (11 .. 54)
     := "The quick brown fox jumps over the lazy dog.";

   procedure No_Fail
     (Report : in out NT.Reporter'Class;
      Name : in String;
      Slice : in String_Slices.Slice);
      --  Report lack-of-exception test failure and dump Slice

   ------------------------------
   -- Local helper subprograms --
   ------------------------------

   procedure No_Fail
     (Report : in out NT.Reporter'Class;
      Name : in String;
      Slice : in String_Slices.Slice) is
   begin
      Report.Item (Name, NT.Fail);
      Report.Info ("No exception has been raised.");
      Report.Info ("Final value: "
        & String_Slices.Image (Slice.Get_Range)
        & " """ & Slice.To_String & '"');
   end No_Fail;


   ----------------------
   -- Test collections --
   ----------------------

   procedure All_Tests (Report : in out NT.Reporter'Class) is
   begin
      Report.Section ("String_Range tests");
      Range_Tests (Report);
      Report.End_Section;

      Slice_Tests (Report);
   end All_Tests;


   procedure Range_Tests (Report : in out NT.Reporter'Class) is
   begin
      Test_Is_In (Report);
      Test_Is_Subrange (Report);
      Test_Range_Image (Report);
      Test_Set_Length (Report);
   end Range_Tests;


   procedure Slice_Tests (Report : in out NT.Reporter'Class) is
   begin
      Test_Slice_Relations (Report);
      Test_Invalid_Subslices (Report);

      Test_Conversions (Report);
      Test_Extensions (Report);
      Test_Incoming_Range (Report);
      Test_Invalid_Extensions (Report);
      Test_Null_Slice (Report);
      Test_Outgoing_Range (Report);
      Test_Subslices (Report);
   end Slice_Tests;



   ----------------------
   -- Individual tests --
   ----------------------

   procedure Test_Conversions (Report : in out NT.Reporter'Class) is
      Name : constant String := "Functions To_Slice, To_String and Export";
   begin
      declare
         S : constant String_Slices.Slice := String_Slices.To_Slice (Name);
         Str : constant String := String_Slices.To_String (S);
         Exported : String (101 .. 100 + Name'Length);
      begin
         S.Export (Exported);

         if Str /= Name or Exported /= Name then
            Report.Item (Name, NT.Fail);
            Report.Info ('"' & Str & """ instead of """ & Name & '"');
         else
            Report.Item (Name, NT.Success);
         end if;
      end;
   exception
      when Error : others => Report.Report_Exception (Name, Error);
   end Test_Conversions;


   procedure Test_Extensions (Report : in out NT.Reporter'Class) is
      Name : constant String := "Slice extensions";
   begin
      declare
         use type String_Slices.Slice;

         Parent : constant String_Slices.Slice
           := String_Slices.To_Slice (Parent_String);
         Extended : constant String_Slices.Slice
           := Parent.Subslice (String_Slices.To_Range (33, 50));
         Small : String_Slices.Slice := Parent.Subslice (20, 50);
      begin
         if Small.Parent /= Parent then
            Report.Item (Name, NT.Fail);
            Report.Info ("Small.Parent /= Parent");
            return;
         end if;

         if Small.Extend (Extended.Get_Range) /= Extended then
            Report.Item (Name, NT.Fail);
            Report.Info ("Small.Extend /= Extended");
            return;
         end if;

         Small.Extend (Extended.First, Extended.Last);
         if Small /= Extended then
            Report.Item (Name, NT.Fail);
            Report.Info ("Extended Small /= Extended");
            return;
         end if;

         if String_Slices.Null_Slice.Parent /= String_Slices.Null_Slice then
            Report.Item (Name, NT.Fail);
            Report.Info ("Null_Slice.Parent /= Null_Slice");
            return;
         end if;

         if String_Slices.Null_Slice.Duplicate /= String_Slices.Null_Slice then
            Report.Item (Name, NT.Fail);
            Report.Info ("Null_Slice.Duplicate /= Null_Slice");
            return;
         end if;
      end;

      Report.Item (Name, NT.Success);
   exception
      when Error : others => Report.Report_Exception (Name, Error);
   end Test_Extensions;


   procedure Test_Incoming_Range (Report : in out NT.Reporter'Class) is
      Name : constant String := "Range conservation through To_Slice";
   begin
      declare
         use type String_Slices.String_Range;

         Biased_String : constant String (11 .. 16) := "qwerty";
         S : constant String_Slices.Slice
           := String_Slices.To_Slice (Biased_String);
      begin
         if S.Get_Range /= String_Slices.Get_Range (Biased_String) then
            Report.Item (Name, NT.Fail);
            Report.Info ("String range: "
              & String_Slices.Image (String_Slices.Get_Range (Biased_String)));
            Report.Info ("Slice range: "
              & String_Slices.Image (S.Get_Range));
         else
            Report.Item (Name, NT.Success);
         end if;
      end;
   exception
      when Error : others => Report.Report_Exception (Name, Error);
   end Test_Incoming_Range;


   procedure Test_Invalid_Extensions (Report : in out NT.Reporter'Class) is
      Name : constant String := "Invalid extensions";
   begin
      declare
         Parent : constant String_Slices.Slice
           := String_Slices.To_Slice (Parent_String);
         Small : String_Slices.Slice
           := Parent.Subslice (String_Slices.To_Range (30, 50));
      begin
         declare
            Extended : String_Slices.Slice;
         begin
            Extended := Small.Extend (1, 10);
            No_Fail (Report, Name, Extended);
            return;
         exception
            when Constraint_Error => null;
            when Error : others =>
               Report.Item (Name, NT.Fail);
               Report.Info ("Wrong exception "
                 & Ada.Exceptions.Exception_Name (Error)
                 & "has been raised.");
               return;
         end;

         begin
            Small.Extend (100, 150);
            No_Fail (Report, Name, Small);
            return;
         exception
            when Constraint_Error => null;
            when Error : others =>
               Report.Item (Name, NT.Fail);
               Report.Info ("Wrong exception "
                 & Ada.Exceptions.Exception_Name (Error)
                 & "has been raised.");
               return;
         end;
      end;

      Report.Item (Name, NT.Success);
   exception
      when Error : others => Report.Report_Exception (Name, Error);
   end Test_Invalid_Extensions;


   procedure Test_Invalid_Subslices (Report : in out NT.Reporter'Class) is
      Parent : constant String_Slices.Slice
        := String_Slices.To_Slice (Parent_String);
      Template : constant String_Slices.Slice := Parent.Subslice (22, 40);
      Slice : String_Slices.Slice;
   begin
      Report.Section ("Invalid subslices");

      declare
         Name : constant String := "Subslice too large";
      begin
         Slice := Template.Subslice (String_Slices.To_Range (32, 45));
         No_Fail (Report, Name, Slice);
      exception
         when Constraint_Error => Report.Item (Name, NT.Success);
         when Error : others =>
            Report.Item (Name, NT.Fail);
            Report.Info ("Wrong exception "
              & Ada.Exceptions.Exception_Name (Error)
              & "has been raised.");
      end;

      declare
         Name : constant String := "Subslice of null slice";
      begin
         Slice := String_Slices.Null_Slice.Subslice (32, 45);
         No_Fail (Report, Name, Slice);
      exception
         when Constraint_Error => Report.Item (Name, NT.Success);
         when Error : others =>
            Report.Item (Name, NT.Fail);
            Report.Info ("Wrong exception "
              & Ada.Exceptions.Exception_Name (Error)
              & "has been raised.");
      end;

      declare
         Name : constant String := "Restrict too large";
      begin
         Slice := Template;
         Slice.Restrict (String_Slices.To_Range (15, 30));
         No_Fail (Report, Name, Slice);
      exception
         when Constraint_Error => Report.Item (Name, NT.Success);
         when Error : others =>
            Report.Item (Name, NT.Fail);
            Report.Info ("Wrong exception "
              & Ada.Exceptions.Exception_Name (Error)
              & "has been raised.");
      end;

      declare
         Name : constant String := "Restrict of null slice";
         Default_Slice : String_Slices.Slice;
      begin
         Default_Slice.Restrict (15, 30);
         No_Fail (Report, Name, Default_Slice);
      exception
         when Constraint_Error => Report.Item (Name, NT.Success);
         when Error : others =>
            Report.Item (Name, NT.Fail);
            Report.Info ("Wrong exception "
              & Ada.Exceptions.Exception_Name (Error)
              & "has been raised.");
      end;

      declare
         Name : constant String := "Set_First too small";
      begin
         Slice := Template;
         Slice.Set_First (15);
         No_Fail (Report, Name, Slice);
      exception
         when Constraint_Error => Report.Item (Name, NT.Success);
         when Error : others =>
            Report.Item (Name, NT.Fail);
            Report.Info ("Wrong exception "
              & Ada.Exceptions.Exception_Name (Error)
              & "has been raised.");
      end;

      declare
         Name : constant String := "Set_Last too large";
      begin
         Slice := Template;
         Slice.Set_Last (45);
         No_Fail (Report, Name, Slice);
      exception
         when Constraint_Error => Report.Item (Name, NT.Success);
         when Error : others =>
            Report.Item (Name, NT.Fail);
            Report.Info ("Wrong exception "
              & Ada.Exceptions.Exception_Name (Error)
              & "has been raised.");
      end;

      declare
         Name : constant String := "Set_Length too large";
      begin
         Slice := Template;
         Slice.Set_Length (Template.Length + 5);
         No_Fail (Report, Name, Slice);
      exception
         when Constraint_Error => Report.Item (Name, NT.Success);
         when Error : others =>
            Report.Item (Name, NT.Fail);
            Report.Info ("Wrong exception "
              & Ada.Exceptions.Exception_Name (Error)
              & "has been raised.");
      end;

      declare
         Name : constant String := "Extend beyond parent";
      begin
         Slice := Template;
         Slice.Extend (String_Slices.To_Range (1, 50));
         No_Fail (Report, Name, Slice);
      exception
         when Constraint_Error => Report.Item (Name, NT.Success);
         when Error : others =>
            Report.Item (Name, NT.Fail);
            Report.Info ("Wrong exception "
              & Ada.Exceptions.Exception_Name (Error)
              & "has been raised.");
      end;

      Report.End_Section;
   end Test_Invalid_Subslices;


   procedure Test_Is_In (Report : in out NT.Reporter'Class) is
      Name : constant String := "Function Is_In";
   begin
      declare
         Interval : constant String_Slices.String_Range := (3, 5);
         Before : constant Boolean := String_Slices.Is_In (2, Interval);
         First : constant Boolean := String_Slices.Is_In (3, Interval);
         Inside : constant Boolean := String_Slices.Is_In (5, Interval);
         Last : constant Boolean := String_Slices.Is_In (7, Interval);
         After : constant Boolean := String_Slices.Is_In (8, Interval);
      begin
         Report.Item (Name, NT.To_Result
           ((not Before) and First and Inside and Last and (not After)));

         if Before then
            Report.Info ("2 in [3, 7]");
         end if;

         if not First then
            Report.Info ("3 not in [3, 7]");
         end if;

         if not Inside then
            Report.Info ("5 not in [3, 7]");
         end if;

         if not Last then
            Report.Info ("7 not in [3, 7]");
         end if;

         if After then
            Report.Info ("8 in [3, 7]");
         end if;
      end;
   exception
      when Error : others => Report.Report_Exception (Name, Error);
   end Test_Is_In;


   procedure Test_Is_Subrange (Report : in out NT.Reporter'Class) is
      procedure Check
        (Left, Right : in String_Slices.String_Range;
         Expected : in Boolean);

      Name : constant String := "Function Is_Subrange";
      Result : Boolean := True;

      procedure Check
        (Left, Right : in String_Slices.String_Range;
         Expected : in Boolean) is
      begin
         if String_Slices.Is_Subrange (Left, Right) /= Expected then
            if Result then
               Report.Item (Name, NT.Fail);
            end if;

            Report.Info ("Is_Subrange ("
              & String_Slices.Image (Left)
              & ", "
              & String_Slices.Image (Right)
              & ") should return "
              & Boolean'Image (Expected));

            Result := False;
         end if;
      end Check;
   begin
      Check ((3, 5), (1, 2), False);
      Check ((3, 5), (1, 3), False);
      Check ((3, 5), (1, 5), False);
      Check ((3, 5), (1, 7), True);
      Check ((3, 5), (1, 9), True);
      Check ((3, 5), (3, 1), False);
      Check ((3, 5), (3, 3), False);
      Check ((3, 5), (3, 5), True);
      Check ((3, 5), (3, 7), True);
      Check ((3, 5), (5, 2), False);
      Check ((3, 5), (5, 3), False);
      Check ((3, 5), (5, 5), False);
      Check ((3, 5), (7, 1), False);
      Check ((3, 5), (7, 3), False);
      Check ((3, 5), (9, 3), False);

      Check ((3, 1), (1, 2), False);
      Check ((3, 1), (1, 3), True);
      Check ((3, 1), (1, 5), True);
      Check ((3, 1), (3, 1), True);
      Check ((3, 1), (3, 3), True);
      Check ((3, 1), (5, 3), False);

      if Result then
         Report.Item (Name, NT.Success);
      end if;
   exception
      when Error : others => Report.Report_Exception (Name, Error);
   end Test_Is_Subrange;


   procedure Test_Null_Slice (Report : in out NT.Reporter'Class) is
      procedure Check_Null (S : in String);

      Name : constant String := "Null slice to empty string";
      Result : Boolean := True;

      procedure Check_Null (S : in String) is
      begin
         if S /= "" then
            if Result then
               Report.Item (Name, NT.Fail);
               Result := False;
            end if;

            Report.Info ("Empty string expected, got """ & S & '"');
         end if;
      end Check_Null;
   begin
      declare
         Default_Slice : String_Slices.Slice;
      begin
         String_Slices.Null_Slice.Query (Check_Null'Access);
         Default_Slice.Query (Check_Null'Access);
         Check_Null (String_Slices.Null_Slice.To_String);
         Check_Null (Default_Slice.To_String);
      end;

      if Result then
         Report.Item (Name, NT.Success);
      end if;
   exception
      when Error : others => Report.Report_Exception (Name, Error);
   end Test_Null_Slice;


   procedure Test_Outgoing_Range (Report : in out NT.Reporter'Class) is
      use type String_Slices.String_Range;

      procedure Check_Range (S : in String);

      Name : constant String
        := "Range conservation through To_String and Query";
      Ref_Range : constant String_Slices.String_Range := (10, 21);
      Result : Boolean := True;

      procedure Check_Range (S : in String) is
      begin
         if String_Slices.Get_Range (S) /= Ref_Range then
            Report.Item (Name, NT.Fail);
            Report.Info ("Queried string range: "
              & String_Slices.Image (String_Slices.Get_Range (S))
              & ", expected "
              & String_Slices.Image (Ref_Range));
            Result := False;
         end if;
      end Check_Range;
   begin
      declare
         Slice : String_Slices.Slice := String_Slices.To_Slice (Name);
      begin
         Slice.Restrict (Ref_Range);
         Slice.Query (Check_Range'Access);

         if Result then
            if String_Slices.Get_Range (Slice.To_String) /= Ref_Range then
               Report.Item (Name, NT.Fail);
               Report.Info ("To_String range: "
                 & String_Slices.Image
                     (String_Slices.Get_Range (Slice.To_String))
                 & ", expected "
                 & String_Slices.Image (Ref_Range));
            else
               Report.Item (Name, NT.Success);
            end if;
         end if;
      end;
   exception
      when Error : others => Report.Report_Exception (Name, Error);
   end Test_Outgoing_Range;


   procedure Test_Range_Image (Report : in out NT.Reporter'Class) is
      procedure Check_String (Result, Reference : in String);

      Name : constant String := "Image of String_Range";
      Success : Boolean := True;

      procedure Check_String (Result, Reference : in String) is
      begin
         if Result /= Reference then
            if Success then
               Report.Item (Name, NT.Fail);
               Success := False;
            end if;

            Report.Info
              ("Result """ & Result & """, expected """ & Reference & '"');
         end if;
      end Check_String;
   begin
      declare
         Range1 : constant String_Slices.String_Range := (10, 0);
         Range2 : constant String_Slices.String_Range := (16, 5);
         Range3 : constant String_Slices.String_Range := (5, 1);
      begin
         Check_String (String_Slices.Image (Range1), "empty at 10");
         Check_String (String_Slices.Image (Range2), "[16, 20]");
         Check_String (String_Slices.Image (Range3), "[5]");
      end;

      if Success then
         Report.Item (Name, NT.Success);
      end if;
   exception
      when Error : others => Report.Report_Exception (Name, Error);
   end Test_Range_Image;


   procedure Test_Set_Length (Report : in out NT.Reporter'Class) is
      Name : constant String := "Procedure Set_Length";
   begin
      declare
         R : String_Slices.String_Range := (10, 10);
      begin
         String_Slices.Set_Length (R, 20);

         if R.Length /= 20 then
            Report.Item (Name, NT.Fail);
            Report.Info ("Unexpected length" & Integer'Image (R.Length));
         else
            Report.Item (Name, NT.Success);
         end if;
      end;
   exception
      when Error : others => Report.Report_Exception (Name, Error);
   end Test_Set_Length;


   procedure Test_Slice_Relations (Report : in out NT.Reporter'Class) is
      Section_Name : constant String := "Slice relations";
   begin
      declare
         Parent : constant String_Slices.Slice
           := String_Slices.To_Slice (Parent_String);
         Beginning : constant String_Slices.Slice
           := String_Slices.Subslice (Parent, String_Slices.To_Range (11, 30));
         Empty : constant String_Slices.Slice
           := String_Slices.Subslice (Parent, String_Slices.To_Range (25, 0));
         Ending : constant String_Slices.Slice
           := String_Slices.Subslice (Parent, 30, Parent.Last);
         First_Word : String_Slices.Slice;
         Dupe : constant String_Slices.Slice
           := String_Slices.Duplicate (Beginning);
      begin
         First_Word := Beginning;
         First_Word.Restrict (11, 13);

         Report.Section (Section_Name);

         declare
            Name : constant String := "Is_Subslice (Beginning, Parent)";
         begin
            Report.Item (Name, NT.To_Result
              (String_Slices.Is_Subslice (Beginning, Parent)));
         exception
            when Error : others => Report.Report_Exception (Name, Error);
         end;

         declare
            Name : constant String := "Is_Subslice (First_Word, Beginning)";
         begin
            Report.Item (Name, NT.To_Result
              (String_Slices.Is_Subslice (First_Word, Beginning)));
         exception
            when Error : others => Report.Report_Exception (Name, Error);
         end;

         declare
            Name : constant String := "Is_Subslice (First_Word, Parent)";
         begin
            Report.Item (Name, NT.To_Result
              (String_Slices.Is_Subslice (First_Word, Parent)));
         exception
            when Error : others => Report.Report_Exception (Name, Error);
         end;

         declare
            Name : constant String := "not Is_Subslice (Beginning, Ending)";
         begin
            Report.Item (Name, NT.To_Result
              (not String_Slices.Is_Subslice (Beginning, Ending)));
         exception
            when Error : others => Report.Report_Exception (Name, Error);
         end;

         declare
            Name : constant String := "Is_Related (Beginning, Ending)";
         begin
            Report.Item (Name, NT.To_Result
              (String_Slices.Is_Related (Beginning, Ending)));
         exception
            when Error : others => Report.Report_Exception (Name, Error);
         end;

         declare
            Name : constant String := "not Is_Related (Beginning, Dupe)";
         begin
            Report.Item (Name, NT.To_Result
              (not String_Slices.Is_Related (Beginning, Dupe)));
         exception
            when Error : others => Report.Report_Exception (Name, Error);
         end;

         declare
            Name : constant String := "Is_Empty (Empty)";
         begin
            Report.Item (Name, NT.To_Result (String_Slices.Is_Empty (Empty)));
         exception
            when Error : others => Report.Report_Exception (Name, Error);
         end;

         declare
            Name : constant String := "not Is_Null (Empty)";
         begin
            Report.Item (Name, NT.To_Result
              (not String_Slices.Is_Null (Empty)));
         exception
            when Error : others => Report.Report_Exception (Name, Error);
         end;

         Report.End_Section;
      end;
   exception
      when Error : others => Report.Report_Exception (Section_Name, Error);
   end Test_Slice_Relations;


   procedure Test_Subslices (Report : in out NT.Reporter'Class) is
      procedure Report_Fail;
      procedure Check_Range
        (Slice : in String_Slices.Slice;
         First, Last : in Natural);
      procedure Check_Contents
        (Slice : in String_Slices.Slice;
         Expected : in String);
      procedure Check_Empty
        (Slice : in String_Slices.Slice;
         Name : in String);

      Name : constant String := "Subslices";
      Success : Boolean := True;

      procedure Report_Fail is
      begin
         if Success then
            Report.Item (Name, NT.Fail);
            Success := False;
         end if;
      end Report_Fail;

      procedure Check_Range
        (Slice : in String_Slices.Slice;
         First, Last : in Natural) is
      begin
         if Slice.First /= First or Slice.Last /= Last then
            Report_Fail;
            Report.Info ("Slice range "
              & String_Slices.Image (Slice.Get_Range)
              & ", expected "
              & String_Slices.Image (String_Slices.To_Range (First, Last)));
         end if;
      end Check_Range;

      procedure Check_Contents
        (Slice : in String_Slices.Slice;
         Expected : in String) is
      begin
         if Slice.To_String /= Expected then
            Report_Fail;
            Report.Info ("Slice contains """ & Slice.To_String & '"');
            Report.Info ("Expected """ & Expected & '"');
         end if;
      end Check_Contents;

      procedure Check_Empty
        (Slice : in String_Slices.Slice;
         Name : in String) is
      begin
         if Slice.Is_Null then
            Report_Fail;
            Report.Info ("Unexpected null slice after " & Name);
         elsif not Slice.Is_Empty then
            Report_Fail;
            Report.Info ("Unexpected non-empty slice after " & Name);
            Report.Info ("Contents: """ & Slice.To_String & '"');
         end if;
      end Check_Empty;
   begin
      declare
         procedure Check_Both
           (S : in String_Slices.Slice;
            First, Last : in Natural);

         procedure Check_Both
           (S : in String_Slices.Slice;
            First, Last : in Natural) is
         begin
            Check_Range (S, First, Last);
            Check_Contents (S, Parent_String (First .. Last));
         end Check_Both;

         Parent : constant String_Slices.Slice
           := String_Slices.To_Slice (Parent_String);
         Slice : String_Slices.Slice;
      begin
         Slice := Parent.Subslice (String_Slices.To_Range (22, 33));
         Check_Both (Slice, 22, 33);

         Slice.Set_First (25);
         Check_Both (Slice, 25, 33);

         Slice.Set_Last (30);
         Check_Both (Slice, 25, 30);

         Slice.Set_Length (3);
         Check_Both (Slice, 25, 27);

         Slice := Parent.Subslice (15, 0);
         Check_Empty (Slice, "Subslice");

         Slice := Parent;
         Slice.Restrict (String_Slices.To_Range (40, 0));
         Check_Empty (Slice, "Restrict");

         Slice := Parent;
         Slice.Set_First (60);
         Check_Empty (Slice, "Set_First");

         Slice := Parent;
         Slice.Set_Last (10);
         Check_Empty (Slice, "Set_Last");

         Slice := Parent.Subslice (String_Slices.To_Range (22, 33));
         Slice.Set_Length (0);
         Check_Empty (Slice, "Set_Length");
      end;

      declare
         Sub_Null : constant String_Slices.Slice
           := String_Slices.Null_Slice.Subslice (10, 0);
      begin
         Check_Range (Sub_Null, 10, 9);

         if not Sub_Null.Is_Null then
            Report_Fail;
            Report.Info ("Null_Slice.Subslice is not null: """
              & Sub_Null.To_String & '"');
         end if;
      end;

      if Success then
         Report.Item (Name, NT.Success);
      end if;
   exception
      when Error : others => Report.Report_Exception (Name, Error);
   end Test_Subslices;

end Natools.String_Slice_Tests;
