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

with Natools.S_Expressions.Interpreter_Loop;
with Natools.Static_Hash_Maps.S_Expressions.Command_Maps;

package body Natools.Static_Hash_Maps.S_Expressions is

   procedure Add_Map
     (Pkg : in out Map_Package;
      Context : in Meaningless_Type;
      Name : in Sx.Atom;
      Arguments : in out Sx.Lockable.Descriptor'Class);

   procedure Add_Value
     (Map : in out Map_Description;
      Element_Name : in String;
      Key : in Sx.Atom);

   procedure Generate_Package
     (Pkg : in out Map_Package;
      Description : in String;
      Name : in Sx.Atom;
      Arguments : in out Sx.Lockable.Descriptor'Class);

   procedure Update_Map
     (Map : in out Map_Description;
      Context : in Meaningless_Type;
      Name : in Sx.Atom;
      Arguments : in out Sx.Lockable.Descriptor'Class);

   procedure Update_Nodes
     (Map : in out Map_Description;
      Context : in Meaningless_Type;
      Name : in Sx.Atom;
      Arguments : in out Sx.Lockable.Descriptor'Class);

   procedure Update_Package
     (Pkg : in out Map_Package;
      Context : in Meaningless_Type;
      Name : in Sx.Atom;
      Arguments : in out Sx.Lockable.Descriptor'Class);

   procedure Update_Package
     (Pkg : in out Map_Package;
      Context : in Meaningless_Type;
      Name : in Sx.Atom);



   procedure Map_Interpreter is new Sx.Interpreter_Loop
     (Map_Description, Meaningless_Type, Update_Map);

   procedure Node_Interpreter is new Sx.Interpreter_Loop
     (Map_Description, Meaningless_Type, Update_Nodes);

   procedure Package_Generator is new Sx.Interpreter_Loop
     (Map_Package, String, Generate_Package);

   procedure Package_Interpreter is new Sx.Interpreter_Loop
     (Map_Package, Meaningless_Type, Update_Package, Update_Package);

   procedure Value_Interpreter is new Sx.Interpreter_Loop
     (Map_Description, String, Dispatch_Without_Argument => Add_Value);



   -------------------------
   -- Command Dispatchers --
   -------------------------

   procedure Add_Map
     (Pkg : in out Map_Package;
      Context : in Meaningless_Type;
      Name : in Sx.Atom;
      Arguments : in out Sx.Lockable.Descriptor'Class)
   is
      pragma Unreferenced (Context);
      Map : Map_Description;
   begin
      Set_Element_Type (Map, Sx.To_String (Name));
      Map_Interpreter (Arguments, Map, Meaningless_Value);
      Add_Map (Pkg, Map);
   end Add_Map;


   procedure Add_Value
     (Map : in out Map_Description;
      Element_Name : in String;
      Key : in Sx.Atom) is
   begin
      Insert (Map, Sx.To_String (Key), Element_Name);
   end Add_Value;


   procedure Generate_Package
     (Pkg : in out Map_Package;
      Description : in String;
      Name : in Sx.Atom;
      Arguments : in out Sx.Lockable.Descriptor'Class) is
   begin
      Open (Pkg, Sx.To_String (Name));
      Set_Description (Pkg, Description);
      Package_Interpreter (Arguments, Pkg, Meaningless_Value);
      Commit (Pkg);
   end Generate_Package;



   procedure Update_Map
     (Map : in out Map_Description;
      Context : in Meaningless_Type;
      Name : in Sx.Atom;
      Arguments : in out Sx.Lockable.Descriptor'Class)
   is
      pragma Unreferenced (Context);
      use type Sx.Events.Event;
      Event : constant Sx.Events.Event := Arguments.Current_Event;
   begin
      case Command_Maps.To_Map_Command (Sx.To_String (Name)) is
         when Hash_Package =>
            if Event = Sx.Events.Add_Atom then
               Set_Hash_Package_Name
                 (Map, Sx.To_String (Arguments.Current_Atom));
            else
               Set_Hash_Package_Name (Map, "");
            end if;

         when Function_Name =>
            if Event = Sx.Events.Add_Atom then
               Set_Function_Name (Map, Sx.To_String (Arguments.Current_Atom));
            else
               Set_Function_Name (Map, "");
            end if;

         when Not_Found =>
            if Event = Sx.Events.Add_Atom then
               Set_Not_Found (Map, Sx.To_String (Arguments.Current_Atom));
            else
               Set_Not_Found (Map, "");
            end if;

         when Nodes =>
            Node_Interpreter (Arguments, Map, Meaningless_Value);
      end case;
   end Update_Map;


   procedure Update_Nodes
     (Map : in out Map_Description;
      Context : in Meaningless_Type;
      Name : in Sx.Atom;
      Arguments : in out Sx.Lockable.Descriptor'Class)
   is
      pragma Unreferenced (Context);
   begin
      Value_Interpreter (Arguments, Map, Sx.To_String (Name));
   end Update_Nodes;


   procedure Update_Package
     (Pkg : in out Map_Package;
      Context : in Meaningless_Type;
      Name : in Sx.Atom;
      Arguments : in out Sx.Lockable.Descriptor'Class)
   is
      pragma Unreferenced (Context);
      use type Sx.Events.Event;
      use type Sx.Octet;
      Is_Command : Boolean := False;
   begin
      for I in Name'Range loop
         if Name (I) = Character'Pos ('-') then
            Is_Command := True;
            exit;
         end if;
      end loop;

      if not Is_Command then
         Add_Map (Pkg, Meaningless_Value, Name, Arguments);
         return;
      end if;

      case Command_Maps.To_Package_Command (Sx.To_String (Name)) is
         when Private_Child =>
            Set_Private_Child (Pkg, True);
         when Public_Child =>
            Set_Private_Child (Pkg, False);
         when Extra_Declarations =>
            if Arguments.Current_Event = Sx.Events.Add_Atom then
               Set_Extra_Declarations
                 (Pkg, Sx.To_String (Arguments.Current_Atom));
            end if;
         when Test_Function =>
            if Arguments.Current_Event = Sx.Events.Add_Atom then
               declare
                  Child_Name : constant String
                    := Sx.To_String (Arguments.Current_Atom);
                  Parent_Name : constant String := To_String (Pkg.Name);
               begin
                  if Child_Name'Length > Parent_Name'Length
                    and then Child_Name (Child_Name'First
                              .. Child_Name'First + Parent_Name'Length - 1)
                       = Parent_Name
                  then
                     Set_Test_Child (Pkg, Child_Name
                       (Child_Name'First + Parent_Name'Length
                        .. Child_Name'Last));
                  else
                     Set_Test_Child (Pkg, Child_Name);
                  end if;
               end;
            else
               Set_Test_Child (Pkg, "");
            end if;
      end case;
   end Update_Package;


   procedure Update_Package
     (Pkg : in out Map_Package;
      Context : in Meaningless_Type;
      Name : in Sx.Atom)
   is
      pragma Unreferenced (Context);
   begin
      case Command_Maps.To_Package_Command (Sx.To_String (Name)) is
         when Private_Child =>
            Set_Private_Child (Pkg, True);
         when Public_Child =>
            Set_Private_Child (Pkg, False);
         when Extra_Declarations =>
            Set_Extra_Declarations (Pkg, Sx.To_String (Name));
         when Test_Function =>
            null;
      end case;
   end Update_Package;



   -----------------------
   -- Public Generators --
   -----------------------

   procedure Generate_Packages
     (Input : in out Sx.Lockable.Descriptor'Class;
      Description : in String := "")
   is
      Pkg : Map_Package;
   begin
      Package_Generator (Input, Pkg, Description);
   end Generate_Packages;


   procedure Generate_Package
     (Input : in out Sx.Lockable.Descriptor'Class;
      Description : in String := "")
   is
      use type Sx.Events.Event;
      Pkg : Map_Package;
   begin
      if Input.Current_Event /= Sx.Events.Add_Atom then
         return;
      end if;

      declare
         Name : constant Sx.Atom := Input.Current_Atom;
         Event : Sx.Events.Event;
      begin
         Input.Next (Event);
         if Event = Sx.Events.Add_Atom or Event = Sx.Events.Open_List then
            Generate_Package (Pkg, Description, Name, Input);
         end if;
      end;
   end Generate_Package;

end Natools.Static_Hash_Maps.S_Expressions;
