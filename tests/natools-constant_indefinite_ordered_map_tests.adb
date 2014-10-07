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

with Ada.Containers;
with Ada.Strings.Unbounded;
with Natools.Constant_Indefinite_Ordered_Maps;

package body Natools.Constant_Indefinite_Ordered_Map_Tests is

   package Test_Maps is new
     Constant_Indefinite_Ordered_Maps
        (Key_Type     => String,
         Element_Type => Integer);


   function Image (Map : Test_Maps.Unsafe_Maps.Map) return String;

   function Sample_Map return Test_Maps.Unsafe_Maps.Map;

   function Sample_Map return Test_Maps.Updatable_Map
     is (Test_Maps.Create (Sample_Map));


   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

   function Image (Map : Test_Maps.Unsafe_Maps.Map) return String is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;
      First : Boolean := True;

      procedure Process (Cursor : Test_Maps.Unsafe_Maps.Cursor);

      procedure Process (Cursor : Test_Maps.Unsafe_Maps.Cursor) is
      begin
         if First then
            First := False;
         else
            Append (Result, ", ");
         end if;

         Append (Result, Test_Maps.Unsafe_Maps.Key (Cursor));
         Append (Result, " ->");
         Append
           (Result, Integer'Image (Test_Maps.Unsafe_Maps.Element (Cursor)));
      end Process;
   begin
      Append (Result, "(");
      Map.Iterate (Process'Access);
      Append (Result, ")");
      return To_String (Result);
   end Image;


   function Sample_Map return Test_Maps.Unsafe_Maps.Map is
      Result : Test_Maps.Unsafe_Maps.Map;
   begin
      for I in 0 .. 9 loop
         Result.Insert
           ((1 => '1',
             2 => Character'Val (Character'Pos ('0') + I)),
            I + 10);
         Result.Insert
           ((1 => '2',
             2 => Character'Val (Character'Pos ('0') + I)),
            I + 20);
      end loop;

      return Result;
   end Sample_Map;



   -------------------------
   -- Complete Test Suite --
   -------------------------

   procedure All_Tests (Report : in out NT.Reporter'Class) is
   begin
      Consistency (Report);
      Cursor_Operations (Report);
      Direct_Access (Report);
      Empty_Map (Report);
      Iterations (Report);
      Map_Updates (Report);
      Unsafe_Map_Roundtrip (Report);
   end All_Tests;



   ----------------------
   -- Individual Tests --
   ----------------------

   procedure Consistency (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Consistency checks");
   begin
      if Test_Maps.Has_Element (Test_Maps.No_Element) then
         Test.Fail ("No_Element has an element");
      end if;

      declare
         use type Ada.Containers.Count_Type;
         use type Test_Maps.Cursor;

         Map : constant Test_Maps.Updatable_Map := Sample_Map;
         Cursor : Test_Maps.Cursor;
      begin
         if Map.Length /= 20 then
            Test.Fail ("Unexpected map length:"
              & Ada.Containers.Count_Type'Image (Map.Length));
         end if;

         Cursor := Map.First;

         if Test_Maps.Key (Cursor) /= Map.First_Key then
            Test.Fail ("Key (First) /= First_Key");
         end if;

         if Test_Maps.Element (Cursor) /= Map.First_Element then
            Test.Fail ("Element (First) /= First_Element");
         end if;

         if Test_Maps.Previous (Cursor) /= Test_Maps.No_Element then
            Test.Fail ("Previous (First) has element");
         end if;

         Test_Maps.Next (Cursor);

         if Cursor < Map.First then
            Test.Fail ("Second < First");
         end if;

         if Cursor < Map.First_Key then
            Test.Fail ("Second < First_Key");
         end if;

         if not (Map.First_Key < Cursor) then
            Test.Fail ("Second <= First_Key");
         end if;

         Cursor := Map.Last;

         if Test_Maps.Key (Cursor) /= Map.Last_Key then
            Test.Fail ("Key (Last) /= Last_Key");
         end if;

         if Test_Maps.Element (Cursor) /= Map.Last_Element then
            Test.Fail ("Element (Last) /= Last_Element");
         end if;

         if Test_Maps.Next (Cursor) /= Test_Maps.No_Element then
            Test.Fail ("Next (Last) has element");
         end if;

         Test_Maps.Previous (Cursor);

         if Cursor > Map.Last then
            Test.Fail ("Before_Last > Last");
         end if;

         if Cursor > Map.Last_Key then
            Test.Fail ("Before_Last > Last_Key");
         end if;

         if not (Map.Last_Key > Cursor) then
            Test.Fail ("Before_Last >= Last_Key");
         end if;
      end;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Consistency;


   procedure Cursor_Operations (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Cursor operations");
   begin
      declare
         procedure Check (Cursor : in Test_Maps.Cursor);
         procedure Check (Key : in String; Element : in Integer);

         Expected : String := "??";

         procedure Check (Cursor : in Test_Maps.Cursor) is
         begin
            Test_Maps.Query_Element (Cursor, Check'Access);
         end Check;

         procedure Check (Key : in String; Element : in Integer) is
         begin
            if Key /= Expected or Element /= Integer'Value (Expected) then
               Test.Fail ("Expected """ & Expected
                 & """, got (""" & Key
                 & " ->" & Integer'Image (Element) & ')');
            end if;
         end Check;

         Map : constant Test_Maps.Updatable_Map := Sample_Map;
         Cursor, Alternate : Test_Maps.Cursor;
      begin
         if Test_Maps.Has_Element (Cursor) then
            Test.Fail ("Default cursor is not empty");
            return;
         end if;

         Expected := "17";
         Cursor := Map.Find (Expected);
         if not Test_Maps.Has_Element (Cursor) then
            Test.Fail ("Map.Find (""17"") has no element");
            return;
         end if;
         Check (Cursor);

         Alternate := Test_Maps.Previous (Cursor);
         Expected := "16";
         Check (Alternate);

         Alternate := Test_Maps.Next (Cursor);
         Expected := "18";
         Check (Alternate);

         Test_Maps.Clear (Alternate);
         if Test_Maps.Has_Element (Alternate) then
            Test.Fail ("Clear cursor has element");
            return;
         end if;

         Test_Maps.Next (Alternate);
         if Test_Maps.Has_Element (Alternate) then
            Test.Fail ("Next (Empty_Cursor) has element");
            return;
         end if;

         Test_Maps.Previous (Alternate);
         if Test_Maps.Has_Element (Alternate) then
            Test.Fail ("Previous (Empty_Cursor) has element");
         end if;
      end;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Cursor_Operations;


   procedure Direct_Access (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Direct node access");
   begin
      declare
         use type Test_Maps.Cursor;

         Map : constant Test_Maps.Updatable_Map := Sample_Map;
         Img : String := "??";
         Cursor : Test_Maps.Cursor;
      begin
         for I in 10 .. 29 loop
            Img (1) := Character'Val (Character'Pos ('0') + I / 10);
            Img (2) := Character'Val (Character'Pos ('0') + I mod 10);

            if not Map.Contains (Img) then
               Test.Fail ("Sample_Map should contain key """ & Img & '"');
            elsif Map.Floor (Img) /= Map.Ceiling (Img) then
               Test.Fail ("Floor /= Ceiling for existing key """ & Img & '"');
            elsif Map.Element (Img) /= I then
               Test.Fail ("Unexpected element"
                 & Integer'Image (Map.Element (Img))
                 & " for key """ & Img & '"');
            end if;

            Cursor := Map.Floor ("1");
            if Test_Maps.Has_Element (Cursor) then
               Test.Fail ("Map.Floor (""1"") is not empty ("""
                 & Test_Maps.Key (Cursor) & '"');
            end if;

            Cursor := Map.Find ("2");
            if Test_Maps.Has_Element (Cursor) then
               Test.Fail ("Map.Find (""2"") is not empty ("""
                 & Test_Maps.Key (Cursor) & '"');
            end if;

            Cursor := Map.Ceiling ("3");
            if Test_Maps.Has_Element (Cursor) then
               Test.Fail ("Map.Ceiling (""3"") is not empty ("""
                 & Test_Maps.Key (Cursor) & '"');
            end if;

            Cursor := Map.Floor ("2");
            if not Test_Maps.Has_Element (Cursor) then
               Test.Fail ("Map.Floor (""2"") is empty");
            elsif Test_Maps.Key (Cursor) /= "19" then
               Test.Fail ("Map.Floor (""2"") returns unexpected node """
                 & Test_Maps.Key (Cursor) & '"');
            end if;

            Cursor := Map.Ceiling ("2");
            if not Test_Maps.Has_Element (Cursor) then
               Test.Fail ("Map.Ceiling (""2"") is empty");
            elsif Test_Maps.Key (Cursor) /= "20" then
               Test.Fail ("Map.Ceiling (""2"") returns unexpected node """
                 & Test_Maps.Key (Cursor) & '"');
            end if;
         end loop;
      end;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Direct_Access;


   procedure Empty_Map (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Operations on empty map");
   begin
      declare
         use type Ada.Containers.Count_Type;
         use type Test_Maps.Updatable_Map;

         procedure Fail_Test (Cursor : in Test_Maps.Cursor);

         procedure Fail_Test (Cursor : in Test_Maps.Cursor) is
            pragma Unreferenced (Cursor);
         begin
            Test.Fail ("Unexpected callback use");
         end Fail_Test;

         Cursor : Test_Maps.Cursor;
         Map : Test_Maps.Updatable_Map;
         pragma Unmodified (Map);
      begin
         Map.Iterate (Fail_Test'Access);
         Map.Reverse_Iterate (Fail_Test'Access);

         if Test_Maps.Has_Element (Map.First) then
            Test.Fail ("Empty_Map.First has an element");
         end if;

         if Test_Maps.Has_Element (Map.Last) then
            Test.Fail ("Empty_Map.Last has an element");
         end if;

         if not Map.To_Unsafe_Map.Is_Empty then
            Test.Fail ("Empty_Map.To_Unsafe_Map is not empty");
         end if;

         if Map.Length /= 0 then
            Test.Fail ("Empty_Map.Length is not zero");
         end if;

         if Map.Contains ("foo") then
            Test.Fail ("Empty_Map.Contains (""foo"")");
         end if;

         Cursor := Map.Find ("2");
         if Test_Maps.Has_Element (Cursor) then
            Test.Fail ("Empty_Map.Find (""2"") has element ("""
              & Test_Maps.Key (Cursor) & """ ->"
              & Integer'Image (Test_Maps.Element (Cursor)) & ')');
         end if;

         Cursor := Map.Floor ("2");
         if Test_Maps.Has_Element (Cursor) then
            Test.Fail ("Empty_Map.Floor (""2"") has element ("""
              & Test_Maps.Key (Cursor) & """ ->"
              & Integer'Image (Test_Maps.Element (Cursor)) & ')');
         end if;

         Cursor := Map.Ceiling ("2");
         if Test_Maps.Has_Element (Cursor) then
            Test.Fail ("Empty_Map.Ceiling (""2"") has element ("""
              & Test_Maps.Key (Cursor) & """ ->"
              & Integer'Image (Test_Maps.Element (Cursor)) & ')');
         end if;

         if Map /= Test_Maps.Create (Test_Maps.Unsafe_Maps.Empty_Map) then
            Test.Fail ("Empty_Map /= Create (Unsafe_Empty_Map)");
         end if;
      end;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Empty_Map;


   procedure Iterations (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Iterative visit of the whole container");
   begin
      declare
         Map : constant Test_Maps.Updatable_Map := Sample_Map;

         procedure Check (Key : in String; Element : in Integer);
         procedure Check_Cursor (Cursor : in Test_Maps.Cursor);
         procedure Init_Backward (Id_Char : in Character);
         procedure Init_Forward (Id_Char : in Character);

         Id : String := "??";
         Index : Integer := 0;
         Direction : Integer := 1;

         procedure Check (Key : in String; Element : in Integer) is
            Space_Image : constant String := Integer'Image (Index);
            Image : constant String
              := Space_Image (Space_Image'First + 1 .. Space_Image'Last);
         begin
            if Key /= Image then
               Test.Fail (Id & '.' & Image
                 & ". unexpected key """ & Key & '"');
            end if;

            if Element /= Index then
               Test.Fail (Id & '.' & Image
                  & ". unexpected element" & Integer'Image (Element));
            end if;

            Index := Index + Direction;
         end Check;

         procedure Check_Cursor (Cursor : in Test_Maps.Cursor) is
         begin
            Check (Test_Maps.Key (Cursor), Test_Maps.Element (Cursor));
         end Check_Cursor;

         procedure Init_Backward (Id_Char : in Character) is
         begin
            Id := Id_Char & 'b';
            Index := 29;
            Direction := -1;
         end Init_Backward;

         procedure Init_Forward (Id_Char : in Character) is
         begin
            Id := Id_Char & 'f';
            Index := 10;
            Direction := 1;
         end Init_Forward;
      begin
         begin
            Init_Forward ('1');
            Map.Iterate (Check_Cursor'Access);
         end;

         begin
            Init_Backward ('1');
            Map.Reverse_Iterate (Check_Cursor'Access);
         end;

         declare
            Cursor : Test_Maps.Cursor := Map.First;
         begin
            Init_Forward ('2');
            while Test_Maps.Has_Element (Cursor) loop
               Check_Cursor (Cursor);
               Test_Maps.Next (Cursor);
            end loop;
         end;

         declare
            Cursor : Test_Maps.Cursor := Map.Last;
         begin
            Init_Backward ('2');
            while Test_Maps.Has_Element (Cursor) loop
               Check_Cursor (Cursor);
               Test_Maps.Previous (Cursor);
            end loop;
         end;
      end;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Iterations;


   procedure Map_Updates (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Map updates");
   begin
      declare
         use type Test_Maps.Updatable_Map;

         procedure Update (Key : in String; Element : in out Integer);

         procedure Update (Key : in String; Element : in out Integer) is
            pragma Unreferenced (Key);
         begin
            Element := 7;
         end Update;

         Map_A : Test_Maps.Updatable_Map := Sample_Map;
         Map_B : Test_Maps.Updatable_Map := Sample_Map;
         Cursor : Test_Maps.Cursor;
      begin
         if Map_A = Map_B then
            Test.Fail ("Unrelated maps are equal");
            return;
         end if;

         Cursor := Map_A.Find ("17");
         pragma Assert (Test_Maps.Has_Element (Cursor));

         if Test_Maps.Is_Related (Map_B, Cursor) then
            Test.Fail ("Map_B and Cursor should be unrelated");
            return;
         end if;

         Map_A.Update_Element (Cursor, Update'Access);
         if Test_Maps.Element (Cursor) /= 7 then
            Test.Fail ("Update failure, element is"
              & Integer'Image (Test_Maps.Element (Cursor))
              & ", should be 7");
         end if;

         Test_Maps.Move (Map_B, Map_A);

         if not Map_A.Is_Empty then
            Test.Fail ("Move source is not empty");
         end if;

         if not Test_Maps.Is_Related (Map_B, Cursor) then
            Test.Fail ("Move target is not related to old source");
         else
            Map_B.Update_Element (Cursor, Update'Access);
         end if;

         Map_A.Replace (Map_B.To_Unsafe_Map);

         if Map_A.Is_Empty then
            Test.Fail ("Replaced map is empty");
         end if;

         if Map_A.Element ("17") /= 7 then
            Test.Fail ("Unexpected value"
              & Integer'Image (Map_A.Element ("17"))
              & "for Map_A.Element (""17"")");
         end if;

         Map_B.Clear;

         if not Map_B.Is_Empty then
            Test.Fail ("Cleared map is not empty");
         end if;

         if Test_Maps.Is_Related (Map_B, Cursor) then
            Test.Fail ("Clear map is still related to cursor");
         end if;

         if (not Test_Maps.Has_Element (Cursor))
           or else Test_Maps.Element (Cursor) /= 7
         then
            Test.Fail ("Orphaned cursor has lost its value");
         end if;

         Test_Maps.Next (Cursor);
         if (not Test_Maps.Has_Element (Cursor))
           or else Test_Maps.Element (Cursor) /= 18
         then
            Test.Fail ("Moved orphaned cursor has lost its value");
         end if;
      end;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Map_Updates;


   procedure Unsafe_Map_Roundtrip (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Constant_Map <-> Unsafe_Map roundtrip");
   begin
      declare
         use type Test_Maps.Unsafe_Maps.Map;

         Unsafe : constant Test_Maps.Unsafe_Maps.Map := Sample_Map;
         Safe : constant Test_Maps.Updatable_Map := Test_Maps.Create (Unsafe);
         Roundtrip : constant Test_Maps.Unsafe_Maps.Map := Safe.To_Unsafe_Map;
      begin
         if Unsafe /= Roundtrip then
            Test.Fail;
            Test.Info ("Original: " & Image (Unsafe));
            Test.Info ("Roundtrip: " & Image (Roundtrip));
         end if;
      end;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Unsafe_Map_Roundtrip;

end Natools.Constant_Indefinite_Ordered_Map_Tests;
