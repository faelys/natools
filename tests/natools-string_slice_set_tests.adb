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
   pragma Unreferenced (Info_Fail);

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
      Test_Navigation (Report);
      Test_Tokenization (Report);
   end All_Tests;



   ----------------------
   -- Individual tests --
   ----------------------

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

         if Set_Last /= Parent_String'Last then
            Report.Item (Name, NT.Fail);
            Report.Info
              ("Last index is" & Integer'Image (Set_Last)
               & ", expected" & Integer'Image (Parent_String'Last));
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
