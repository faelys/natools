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

with Ada.Command_Line;
with Ada.Streams;
with Ada.Strings.Unbounded;
with Ada.Text_IO.Text_Streams;
with Natools.Getopt_Long;
with Natools.S_Expressions.Parsers;
with Natools.Smaz.Tools;

procedure Smaz is
   package Options is
      type Id is
        (Help,
         Output_Ada_Dictionary);
   end Options;

   package Getopt is new Natools.Getopt_Long (Options.Id);

   type Callback is new Getopt.Handlers.Callback with record
      Display_Help : Boolean := False;
      Need_Dictionary : Boolean := False;
      Ada_Dictionary : Ada.Strings.Unbounded.Unbounded_String;
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

         when Options.Output_Ada_Dictionary =>
            Handler.Need_Dictionary := True;

            if Argument'Length > 0 then
               Handler.Ada_Dictionary
                 := Ada.Strings.Unbounded.To_Unbounded_String (Argument);
            else
               Handler.Ada_Dictionary
                 := Ada.Strings.Unbounded.To_Unbounded_String ("-");
            end if;
      end case;
   end Option;


   function Getopt_Config return Getopt.Configuration is
      use Getopt;
      use Options;
      R : Getopt.Configuration;
   begin
      R.Add_Option ("ada-dict", 'A', Optional_Argument, Output_Ada_Dictionary);
      R.Add_Option ("help",     'h', No_Argument,       Help);

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

            when Options.Output_Ada_Dictionary =>
               Put_Line (Output, "=[filename]");
               Put_Line (Output, Indent & Indent
                 & "Output the current dictionary as Ada code in the given");
               Put_Line (Output, Indent & Indent
                 & "file, or standard output if filename is ""-""");
         end case;
      end loop;
   end Print_Help;


   Opt_Config : constant Getopt.Configuration := Getopt_Config;
   Handler : Callback;
   Input_List : Natools.Smaz.Tools.String_Lists.List;
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

   Read_Input_List :
   declare
      Input : constant access Ada.Streams.Root_Stream_Type'Class
        := Ada.Text_IO.Text_Streams.Stream (Ada.Text_IO.Current_Input);
      Parser : Natools.S_Expressions.Parsers.Stream_Parser (Input);
   begin
      Parser.Next;
      Natools.Smaz.Tools.Read_List (Input_List, Parser);
   end Read_Input_List;


   Build_Dictionary :
   declare
      Dictionary : constant Natools.Smaz.Dictionary
        := Natools.Smaz.Tools.To_Dictionary (Input_List, True);
      Ada_Dictionary : constant String
        := Ada.Strings.Unbounded.To_String (Handler.Ada_Dictionary);
   begin
      if Ada_Dictionary'Length > 0 then
         Print_Dictionary (Ada_Dictionary, Dictionary);
      end if;
   end Build_Dictionary;
end Smaz;
