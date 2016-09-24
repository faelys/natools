------------------------------------------------------------------------------
-- Copyright (c) 2016, Natacha Porté                                        --
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
-- Command Line Interface for primitives in Natools.Smaz.Tools.             --
------------------------------------------------------------------------------

with Ada.Characters.Latin_1;
with Ada.Command_Line;
with Ada.Streams;
with Ada.Strings.Unbounded;
with Ada.Text_IO.Text_Streams;
with Natools.Getopt_Long;
with Natools.S_Expressions.Parsers;
with Natools.S_Expressions.Printers;
with Natools.Smaz.Tools;
with Natools.Smaz.Tools.GNAT;

procedure Smaz is
   package Actions is
      type Enum is
        (Nothing,
         Encode);
   end Actions;

   package Options is
      type Id is
        (Output_Ada_Dictionary,
         Encode,
         Output_Hash,
         Help,
         Stat_Output,
         No_Stat_Output,
         Sx_Output,
         No_Sx_Output);
   end Options;

   package Getopt is new Natools.Getopt_Long (Options.Id);

   type Callback is new Getopt.Handlers.Callback with record
      Display_Help : Boolean := False;
      Need_Dictionary : Boolean := False;
      Stat_Output : Boolean := False;
      Sx_Output : Boolean := False;
      Action : Actions.Enum := Actions.Nothing;
      Ada_Dictionary : Ada.Strings.Unbounded.Unbounded_String;
      Hash_Package : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   overriding procedure Option
     (Handler  : in out Callback;
      Id       : in Options.Id;
      Argument : in String);

   overriding procedure Argument
     (Handler  : in out Callback;
      Argument : in String)
     is null;


   function Getopt_Config return Getopt.Configuration;
      --  Build the configuration object

   procedure Print_Dictionary
     (Filename : in String;
      Dictionary : in Natools.Smaz.Dictionary;
      Hash_Package_Name : in String := "");
   procedure Print_Dictionary
     (Output : in Ada.Text_IO.File_Type;
      Dictionary : in Natools.Smaz.Dictionary;
      Hash_Package_Name : in String := "");
      --  print the given dictionary in the given file

   procedure Print_Help
     (Opt : in Getopt.Configuration;
      Output : in Ada.Text_IO.File_Type);
      --  Print the help text to the given file


   overriding procedure Option
     (Handler  : in out Callback;
      Id       : in Options.Id;
      Argument : in String) is
   begin
      case Id is
         when Options.Help =>
            Handler.Display_Help := True;

         when Options.Encode =>
            Handler.Need_Dictionary := True;
            Handler.Action := Actions.Encode;

         when Options.No_Stat_Output =>
            Handler.Stat_Output := False;

         when Options.No_Sx_Output =>
            Handler.Sx_Output := False;

         when Options.Output_Ada_Dictionary =>
            Handler.Need_Dictionary := True;

            if Argument'Length > 0 then
               Handler.Ada_Dictionary
                 := Ada.Strings.Unbounded.To_Unbounded_String (Argument);
            else
               Handler.Ada_Dictionary
                 := Ada.Strings.Unbounded.To_Unbounded_String ("-");
            end if;

         when Options.Output_Hash =>
            Handler.Need_Dictionary := True;
            Handler.Hash_Package
              := Ada.Strings.Unbounded.To_Unbounded_String (Argument);

         when Options.Stat_Output =>
            Handler.Stat_Output := True;

         when Options.Sx_Output =>
            Handler.Sx_Output := True;
      end case;
   end Option;


   function Getopt_Config return Getopt.Configuration is
      use Getopt;
      use Options;
      R : Getopt.Configuration;
   begin
      R.Add_Option ("ada-dict", 'A', Optional_Argument, Output_Ada_Dictionary);
      R.Add_Option ("encode",   'e', No_Argument,       Encode);
      R.Add_Option ("help",     'h', No_Argument,       Help);
      R.Add_Option ("hash-pkg", 'H', Required_Argument, Output_Hash);
      R.Add_Option ("stats",    's', No_Argument,       Stat_Output);
      R.Add_Option ("no-stats", 'S', No_Argument,       No_Stat_Output);
      R.Add_Option ("s-expr",   'x', No_Argument,       Sx_Output);
      R.Add_Option ("no-s-expr", 'X', No_Argument,       No_Sx_Output);

      return R;
   end Getopt_Config;


   procedure Print_Dictionary
     (Filename : in String;
      Dictionary : in Natools.Smaz.Dictionary;
      Hash_Package_Name : in String := "") is
   begin
      if Filename = "-" then
         Print_Dictionary
           (Ada.Text_IO.Current_Output, Dictionary, Hash_Package_Name);
      elsif Filename'Length > 0 then
         declare
            File : Ada.Text_IO.File_Type;
         begin
            Ada.Text_IO.Create (File, Name => Filename);
            Print_Dictionary (File, Dictionary, Hash_Package_Name);
            Ada.Text_IO.Close (File);
         end;
      end if;
   end Print_Dictionary;


   procedure Print_Dictionary
     (Output : in Ada.Text_IO.File_Type;
      Dictionary : in Natools.Smaz.Dictionary;
      Hash_Package_Name : in String := "")
   is
      procedure Put_Line (Line : in String);

      procedure Put_Line (Line : in String) is
      begin
         Ada.Text_IO.Put_Line (Output, Line);
      end Put_Line;

      procedure Print_Dictionary_In_Ada is
        new Natools.Smaz.Tools.Print_Dictionary_In_Ada (Put_Line);
   begin
      if Hash_Package_Name'Length > 0 then
         Print_Dictionary_In_Ada
           (Dictionary,
            Hash_Image => Hash_Package_Name & ".Hash'Access");
      else
         Print_Dictionary_In_Ada (Dictionary);
      end if;
   end Print_Dictionary;


   procedure Print_Help
     (Opt : in Getopt.Configuration;
      Output : in Ada.Text_IO.File_Type)
   is
      use Ada.Text_IO;
      Indent : constant String := "    ";
   begin
      Put_Line (Output, "Usage:");

      for Id in Options.Id loop
         Put (Output, Indent & Opt.Format_Names (Id));

         case Id is
            when Options.Help =>
               New_Line (Output);
               Put_Line (Output, Indent & Indent
                 & "Display this help text");

            when Options.Encode =>
               New_Line (Output);
               Put_Line (Output, Indent & Indent
                 & "Read a list of strings and encode them");

            when Options.No_Stat_Output =>
               New_Line (Output);
               Put_Line (Output, Indent & Indent
                 & "Do not output filter statistics");

            when Options.No_Sx_Output =>
               New_Line (Output);
               Put_Line (Output, Indent & Indent
                 & "Do not output filtered results in a S-expression");

            when Options.Output_Ada_Dictionary =>
               Put_Line (Output, "=[filename]");
               Put_Line (Output, Indent & Indent
                 & "Output the current dictionary as Ada code in the given");
               Put_Line (Output, Indent & Indent
                 & "file, or standard output if filename is ""-""");

            when Options.Output_Hash =>
               Put_Line (Output, " <Hash Package Name>");
               Put_Line (Output, Indent & Indent
                 & "Build a package with a perfect hash function for the");
               Put_Line (Output, Indent & Indent
                 & "current dictionary.");

            when Options.Stat_Output =>
               New_Line (Output);
               Put_Line (Output, Indent & Indent
                 & "Output filter statistics");

            when Options.Sx_Output =>
               New_Line (Output);
               Put_Line (Output, Indent & Indent
                 & "Output filtered results in a S-expression");
         end case;
      end loop;
   end Print_Help;

   Opt_Config : constant Getopt.Configuration := Getopt_Config;
   Handler : Callback;
   Input_List, Input_Data : Natools.Smaz.Tools.String_Lists.List;
begin
   Process_Command_Line :
   begin
      Opt_Config.Process (Handler);
   exception
      when Getopt.Option_Error =>
         Print_Help (Opt_Config, Ada.Text_IO.Current_Error);
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
         return;
   end Process_Command_Line;

   if Handler.Display_Help then
      Print_Help (Opt_Config, Ada.Text_IO.Current_Output);
   end if;

   if not Handler.Need_Dictionary then
      return;
   end if;

   if not (Handler.Stat_Output or Handler.Sx_Output) then
      Handler.Sx_Output := True;
   end if;

   Read_Input_List :
   declare
      use type Actions.Enum;

      Input : constant access Ada.Streams.Root_Stream_Type'Class
        := Ada.Text_IO.Text_Streams.Stream (Ada.Text_IO.Current_Input);
      Parser : Natools.S_Expressions.Parsers.Stream_Parser (Input);
   begin
      Parser.Next;
      Natools.Smaz.Tools.Read_List (Input_List, Parser);

      if Handler.Action /= Actions.Nothing then
         Parser.Next;
         Natools.Smaz.Tools.Read_List (Input_Data, Parser);
      end if;
   end Read_Input_List;


   Build_Dictionary :
   declare
      Dictionary : Natools.Smaz.Dictionary
        := Natools.Smaz.Tools.To_Dictionary (Input_List, True);
      Sx_Output : Natools.S_Expressions.Printers.Canonical
        (Ada.Text_IO.Text_Streams.Stream (Ada.Text_IO.Current_Output));
      Ada_Dictionary : constant String
        := Ada.Strings.Unbounded.To_String (Handler.Ada_Dictionary);
      Hash_Package : constant String
        := Ada.Strings.Unbounded.To_String (Handler.Hash_Package);
   begin
      Dictionary.Hash := Natools.Smaz.Tools.Linear_Search'Access;
      Natools.Smaz.Tools.List_For_Linear_Search := Input_List;

      if Ada_Dictionary'Length > 0 then
         Print_Dictionary (Ada_Dictionary, Dictionary, Hash_Package);
      end if;

      if Hash_Package'Length > 0 then
         Natools.Smaz.Tools.GNAT.Build_Perfect_Hash (Input_List, Hash_Package);
      end if;

      case Handler.Action is
         when Actions.Nothing => null;

         when Actions.Encode =>
            if Handler.Sx_Output then
               Sx_Output.Open_List;
               for S of Input_Data loop
                  Sx_Output.Append_Atom
                    (Natools.Smaz.Compress (Dictionary, S));
               end loop;
               Sx_Output.Close_List;
            end if;

            if Handler.Stat_Output then
               declare
                  procedure Print_Line (Original, Output, Base64 : Natural);

                  procedure Print_Line (Original, Output, Base64 : Natural) is
                  begin
                     Ada.Text_IO.Put_Line
                       (Natural'Image (Original)
                        & Ada.Characters.Latin_1.HT
                        & Natural'Image (Output)
                        & Ada.Characters.Latin_1.HT
                        & Natural'Image (Base64)
                        & Ada.Characters.Latin_1.HT
                        & Float'Image (Float (Output) / Float (Original))
                        & Ada.Characters.Latin_1.HT
                        & Float'Image (Float (Base64) / Float (Original)));
                  end Print_Line;
                  Original_Total : Natural := 0;
                  Output_Total : Natural := 0;
                  Base64_Total : Natural := 0;
               begin
                  for S of Input_Data loop
                     declare
                        Original_Size : constant Natural := S'Length;
                        Output_Size : constant Natural
                          := Natools.Smaz.Compress (Dictionary, S)'Length;
                        Base64_Size : constant Natural
                          := ((Output_Size + 2) / 3) * 4;
                     begin
                        Print_Line (Original_Size, Output_Size, Base64_Size);
                        Original_Total := Original_Total + Original_Size;
                        Output_Total := Output_Total + Output_Size;
                        Base64_Total := Base64_Total + Base64_Size;
                     end;
                  end loop;

                  Print_Line (Original_Total, Output_Total, Base64_Total);
               end;
            end if;
      end case;
   end Build_Dictionary;
end Smaz;
