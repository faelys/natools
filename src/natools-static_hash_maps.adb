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

with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;
with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Text_IO;

with GNAT.Perfect_Hash_Generators;

package body Natools.Static_Hash_Maps is

   function File_Name (Package_Name : in String) return String;
      --  Convert a package name into a file name, the GNAT way

   function Image (Pos : Natural) return String;
      --  Trimmed image, for suffix construction

   function Image (Offset : Ada.Calendar.Time_Zones.Time_Offset) return String;

   procedure Write_Map_Body
     (Map : in Map_Description;
      Prefix : in String;
      File : in Ada.Text_IO.File_Type);
   procedure Write_Map_Hash_Package (Map : in Map_Description);
   procedure Write_Map_Private_Spec
     (Map : in Map_Description;
      Prefix : in String;
      File : in Ada.Text_IO.File_Type);
   procedure Write_Map_Public_Spec
     (Map : in Map_Description;
      File : in Ada.Text_IO.File_Type);
   procedure Write_Map_With
     (Map : in Map_Description;
      File : in Ada.Text_IO.File_Type);
      --  Output fragments relevant for the given map

   procedure Write_Package
     (Pkg : in Map_Package;
      Spec_File, Body_File : in Ada.Text_IO.File_Type);
      --  Output a complete map package


   ------------------------
   -- Package Generators --
   ------------------------

   function File_Name (Package_Name : in String) return String is
      Result : String := Ada.Characters.Handling.To_Lower (Package_Name);
   begin
      for I in Result'Range loop
         if Result (I) = '.' then
            Result (I) := '-';
         end if;
      end loop;

      return Result;
   end File_Name;


   function Image (Pos : Natural) return String is
      Result : constant String := Natural'Image (Pos);
   begin
      pragma Assert (Result (Result'First) = ' ');
      return Result (Result'First + 1 .. Result'Last);
   end Image;


   function Image (Offset : Ada.Calendar.Time_Zones.Time_Offset)
     return String
   is
      use type Ada.Calendar.Time_Zones.Time_Offset;
      H : constant Natural := Natural (abs Offset) / 60;
      M : constant Natural := Natural (abs Offset) mod 60;
      Sign : Character := '+';
   begin
      if Offset < 0 then
         Sign := '-';
      end if;

      return String'(1 => Sign,
         2 => Character'Val (48 + H / 10),
         3 => Character'Val (48 + H mod 10),
         4 => Character'Val (48 + M / 10),
         5 => Character'Val (48 + M mod 10));
   end Image;


   procedure Write_Map_Body
     (Map : in Map_Description;
      Prefix : in String;
      File : in Ada.Text_IO.File_Type) is
   begin
      Ada.Text_IO.Put_Line
        (File,
         "   function "
         & To_String (Map.Function_Name)
         & " (Key : String) return "
         & To_String (Map.Element_Type)
         & " is");
      Ada.Text_IO.Put_Line (File, "      N : constant Natural");
      Ada.Text_IO.Put_Line
        (File,
         "        := " & To_String (Map.Hash_Package_Name) & ".Hash (Key);");
      Ada.Text_IO.Put_Line (File, "   begin");
      Ada.Text_IO.Put_Line
        (File, "      if " & Prefix & "_Keys (N).all = Key then");
      Ada.Text_IO.Put_Line
        (File, "         return " & Prefix & "_Elements (N);");
      Ada.Text_IO.Put_Line (File, "      else");
      if To_String (Map.Not_Found) /= "" then
         Ada.Text_IO.Put_Line
           (File, "         return " & To_String (Map.Not_Found) & ';');
      else
         Ada.Text_IO.Put_Line
           (File,
            "         raise Constraint_Error "
            & "with ""Key """""" & Key & """""" not in map"";");
      end if;
      Ada.Text_IO.Put_Line (File, "      end if;");
      Ada.Text_IO.Put_Line
        (File, "   end " & To_String (Map.Function_Name) & ';');
   end Write_Map_Body;


   procedure Write_Map_Hash_Package (Map : in Map_Description) is
      Seed : constant Natural := 2;
      Cursor : Node_Lists.Cursor := Map.Nodes.First;
   begin
      while Node_Lists.Has_Element (Cursor) loop
         GNAT.Perfect_Hash_Generators.Insert
           (To_String (Node_Lists.Element (Cursor).Key));
         Node_Lists.Next (Cursor);
      end loop;

      GNAT.Perfect_Hash_Generators.Initialize (Seed);
      GNAT.Perfect_Hash_Generators.Compute;
      GNAT.Perfect_Hash_Generators.Produce (To_String (Map.Hash_Package_Name));
      GNAT.Perfect_Hash_Generators.Finalize;
   exception
      when others =>
         GNAT.Perfect_Hash_Generators.Finalize;
         raise;
   end Write_Map_Hash_Package;


   procedure Write_Map_Private_Spec
     (Map : in Map_Description;
      Prefix : in String;
      File : in Ada.Text_IO.File_Type)
   is
      Last : constant Natural := Positive (Map.Nodes.Length) - 1;
      Pos : Natural;
      Cursor : Node_Lists.Cursor;
   begin
      Pos := 0;
      Cursor := Map.Nodes.First;
      while Node_Lists.Has_Element (Cursor) loop
         Ada.Text_IO.Put_Line
           (File,
            "   " & Prefix & "_Key_" & Image (Pos)
            & " : aliased constant String := """
            & To_String (Node_Lists.Element (Cursor).Key)
            & """;");
         Pos := Pos + 1;
         Node_Lists.Next (Cursor);
      end loop;

      Ada.Text_IO.Put_Line
        (File,
         "   " & Prefix & "_Keys : constant array (0 .. " & Image (Last)
         & ") of access constant String");
      Pos := 0;
      Cursor := Map.Nodes.First;
      while Node_Lists.Has_Element (Cursor) loop
         if Pos = 0 then
            Ada.Text_IO.Put (File, "     := (");
         else
            Ada.Text_IO.Put (File, "         ");
         end if;

         Ada.Text_IO.Put (File, Prefix & "_Key_" & Image (Pos) & "'Access");

         if Pos = Last then
            Ada.Text_IO.Put_Line (File, ");");
         else
            Ada.Text_IO.Put_Line (File, ",");
         end if;

         Pos := Pos + 1;
         Node_Lists.Next (Cursor);
      end loop;

      Ada.Text_IO.Put_Line
        (File,
         "   " & Prefix & "_Elements : constant array (0 .. " & Image (Last)
         & ") of " & To_String (Map.Element_Type));
      Pos := 0;
      Cursor := Map.Nodes.First;
      while Node_Lists.Has_Element (Cursor) loop
         if Pos = 0 then
            Ada.Text_IO.Put (File, "     := (");
         else
            Ada.Text_IO.Put (File, "         ");
         end if;

         Ada.Text_IO.Put
           (File, To_String (Node_Lists.Element (Cursor).Name));

         if Pos = Last then
            Ada.Text_IO.Put_Line (File, ");");
         else
            Ada.Text_IO.Put_Line (File, ",");
         end if;

         Pos := Pos + 1;
         Node_Lists.Next (Cursor);
      end loop;
   end Write_Map_Private_Spec;


   procedure Write_Map_Public_Spec
     (Map : in Map_Description;
      File : in Ada.Text_IO.File_Type) is
   begin
      Ada.Text_IO.Put_Line
        (File,
         "   function "
         & To_String (Map.Function_Name)
         & " (Key : String) return "
         & To_String (Map.Element_Type)
         & ';');
   end Write_Map_Public_Spec;


   procedure Write_Map_With
     (Map : in Map_Description;
      File : in Ada.Text_IO.File_Type) is
   begin
      Ada.Text_IO.Put_Line
        (File, "with " & To_String (Map.Hash_Package_Name) & ';');
   end Write_Map_With;


   procedure Write_Package
     (Pkg : in Map_Package;
      Spec_File, Body_File : in Ada.Text_IO.File_Type)
   is
      type Stage is
        (Hash_Package, Public_Spec, Private_Spec, Body_With, Body_Contents);

      Current_Stage : Stage;
      Map_Pos : Natural := 0;

      procedure Process (Element : in Map_Description);
      procedure Query (Cursor : in Map_Lists.Cursor);

      procedure Process (Element : in Map_Description) is
         Prefix : constant String := "Map_" & Image (Map_Pos + 1);
      begin
         case Current_Stage is
            when Hash_Package =>
               Write_Map_Hash_Package (Element);
            when Public_Spec =>
               Write_Map_Public_Spec (Element, Spec_File);
            when Private_Spec =>
               Ada.Text_IO.New_Line (Spec_File);
               Write_Map_Private_Spec (Element, Prefix, Spec_File);
            when Body_With =>
               Write_Map_With (Element, Body_File);
            when Body_Contents =>
               Ada.Text_IO.New_Line (Body_File);
               Write_Map_Body (Element, Prefix, Body_File);
               Ada.Text_IO.New_Line (Body_File);
         end case;
         Map_Pos := Map_Pos + 1;
      end Process;

      procedure Query (Cursor : in Map_Lists.Cursor) is
      begin
         Map_Lists.Query_Element (Cursor, Process'Access);
      end Query;
   begin
      Current_Stage := Hash_Package;
      Map_Pos := 0;
      Pkg.Maps.Iterate (Query'Access);

      Write_Headers :
      declare
         Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
         Offset : constant Ada.Calendar.Time_Zones.Time_Offset
           := Ada.Calendar.Time_Zones.UTC_Time_Offset (Now);
         Header : constant String
           := "--  Generated at "
            & Ada.Calendar.Formatting.Image (Now, False, Offset)
            & ' ' & Image (Offset)
            & " by Natools.Static_Hash_Maps";
         Description : constant String := To_String (Pkg.Description);
      begin
         Ada.Text_IO.Put_Line (Spec_File, Header);
         Ada.Text_IO.Put_Line (Body_File, Header);
         if Description'Length > 0 then
            Ada.Text_IO.Put_Line (Spec_File, "--  " & Description);
            Ada.Text_IO.Put_Line (Body_File, "--  " & Description);
         end if;
         Ada.Text_IO.New_Line (Spec_File);
         Ada.Text_IO.New_Line (Body_File);
      end Write_Headers;

      if Pkg.Priv then
         Ada.Text_IO.Put (Spec_File, "private ");
      end if;
      Ada.Text_IO.Put_Line
        (Spec_File, "package " & To_String (Pkg.Name) & " is");
      Ada.Text_IO.New_Line (Spec_File);

      Current_Stage := Public_Spec;
      Map_Pos := 0;
      Pkg.Maps.Iterate (Query'Access);

      Ada.Text_IO.New_Line (Spec_File);
      Ada.Text_IO.Put_Line (Spec_File, "private");

      Current_Stage := Private_Spec;
      Map_Pos := 0;
      Pkg.Maps.Iterate (Query'Access);

      Ada.Text_IO.New_Line (Spec_File);
      Ada.Text_IO.Put_Line (Spec_File, "end " & To_String (Pkg.Name) & ';');

      Current_Stage := Body_With;
      Map_Pos := 0;
      Pkg.Maps.Iterate (Query'Access);

      Ada.Text_IO.New_Line (Body_File);
      Ada.Text_IO.Put_Line
        (Body_File, "package body " & To_String (Pkg.Name) & " is");

      Current_Stage := Body_Contents;
      Map_Pos := 0;
      Pkg.Maps.Iterate (Query'Access);

      Ada.Text_IO.Put_Line (Body_File, "end " & To_String (Pkg.Name) & ';');
   end Write_Package;



   -------------------------------
   -- Key-Name Pair Constructor --
   -------------------------------

   function Node (Key, Name : String) return Map_Node is
   begin
      return (Key => Hold (Key), Name => Hold (Name));
   end Node;



   ---------------------------------
   -- Map Description Subprograms --
   ---------------------------------

   procedure Reset (Self : out Map_Description) is
   begin
      Self := (Element_Type => Hold (""),
               Hash_Package_Name => Hold (""),
               Function_Name => Hold (""),
               Not_Found => Hold (""),
               Nodes => Node_Lists.Empty_List);
   end Reset;


   procedure Insert
     (Self : in out Map_Description;
      Key : in String;
      Element_Name : in String) is
   begin
      Self.Nodes.Append  (Node (Key, Element_Name));
   end Insert;


   procedure Set_Element_Type
     (Self : in out Map_Description;
      Name : in String) is
   begin
      Self.Element_Type := Hold (Name);
   end Set_Element_Type;


   procedure Set_Function_Name
     (Self : in out Map_Description;
      Name : in String) is
   begin
      Self.Function_Name := Hold (Name);
   end Set_Function_Name;


   procedure Set_Hash_Package_Name
     (Self : in out Map_Description;
      Name : in String) is
   begin
      Self.Hash_Package_Name := Hold (Name);
   end Set_Hash_Package_Name;


   procedure Set_Not_Found
     (Self : in out Map_Description;
      Name : in String) is
   begin
      Self.Not_Found := Hold (Name);
   end Set_Not_Found;


   function Map
     (Element_Type : String;
      Nodes : Node_Array;
      Hash_Package_Name : String := "";
      Function_Name : String := "Element";
      Not_Found : String := "")
     return Map_Description
   is
      Result : Map_Description
        := (Element_Type => Hold (Element_Type),
            Hash_Package_Name => Hold (Hash_Package_Name),
            Function_Name => Hold (Function_Name),
            Not_Found => Hold (Not_Found),
            Nodes => Node_Lists.Empty_List);
   begin
      for I in Nodes'Range loop
         Result.Nodes.Append (Nodes (I));
      end loop;

      return Result;
   end Map;


   ----------------------------
   -- Map Package Primitives --
   ----------------------------

   procedure Open
     (Self : in out Map_Package;
      Name : in String;
      Private_Child : in Boolean := False) is
   begin
      Self.Name := Hold (Name);
      Self.Description := Hold ("");
      Self.Priv := Private_Child;
      Self.Maps.Clear;
   end Open;


   procedure Close (Self : in out Map_Package) is
   begin
      Self.Name := Hold ("");
      Self.Maps.Clear;
   end Close;


   procedure Set_Description
     (Self : in out Map_Package;
      Description : in String) is
   begin
      Self.Description := Hold (Description);
   end Set_Description;


   procedure Set_Private_Child
     (Self : in out Map_Package;
      Private_Child : in Boolean := True) is
   begin
      Self.Priv := Private_Child;
   end Set_Private_Child;


   procedure Add_Map (Self : in out Map_Package; Map : in Map_Description) is
   begin
      if To_String (Self.Name) = "" then
         raise Constraint_Error
           with "Add_Map on non-opened static hash map package";
      end if;

      Self.Maps.Append (Map);
   end Add_Map;


   procedure Commit (Self : in out Map_Package) is
   begin
      if To_String (Self.Name) = "" then
         raise Constraint_Error
           with "Commit on static hash map package without a name";
      end if;

      if Self.Maps.Is_Empty then
         raise Constraint_Error
           with "Commit on static hash map package without any map";
      end if;

      declare
         Package_Name : constant String := To_String (Self.Name);
         Base_Name : constant String := File_Name (Package_Name);
         Spec_File, Body_File : Ada.Text_IO.File_Type;
      begin
         Ada.Text_IO.Create
           (File => Spec_File,
            Name => Ada.Directories.Compose ("", Base_Name, "ads"));
         Ada.Text_IO.Create
           (File => Body_File,
            Name => Ada.Directories.Compose ("", Base_Name, "adb"));

         Write_Package (Self, Spec_File, Body_File);

         Ada.Text_IO.Close (Spec_File);
         Ada.Text_IO.Close (Body_File);
      end;
   end Commit;



   -------------------------
   -- Combined Procedures --
   -------------------------

   procedure Generate_Package
     (Name : in String;
      Single_Map : in Map_Description;
      Private_Child : in Boolean := False)
   is
      Object : Map_Package;
   begin
      Open (Object, Name, Private_Child);
      Add_Map (Object, Single_Map);
      Commit (Object);
   end Generate_Package;


   procedure Generate_Package
     (Name : in String;
      Maps : in Map_Array;
      Private_Child : in Boolean := False)
   is
      Object : Map_Package;
   begin
      Open (Object, Name, Private_Child);
      for I in Maps'Range loop
         Add_Map (Object, Maps (I));
      end loop;
      Commit (Object);
   end Generate_Package;

end Natools.Static_Hash_Maps;

