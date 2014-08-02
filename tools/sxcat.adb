------------------------------------------------------------------------------
-- Copyright (c) 2013-2014, Natacha Porté                                   --
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
-- Sxcat concatenates and pretty prints input S-expressions to standard     --
-- output.                                                                  --
-- Pretty printer options can be given through command-line options or      --
-- loaded from a S-expression file.                                         --
------------------------------------------------------------------------------

with Ada.Command_Line;
with Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Text_IO.Text_Streams;

with Natools.Getopt_Long;
with Natools.S_Expressions.Atom_Buffers;
with Natools.S_Expressions.Encodings;
with Natools.S_Expressions.File_Readers;
with Natools.S_Expressions.Lockable;
with Natools.S_Expressions.Parsers;
with Natools.S_Expressions.Printers.Pretty.Config;

procedure Sxcat is
   package SE renames Natools.S_Expressions;

   Param : SE.Printers.Pretty.Parameters :=
     (Char_Encoding => SE.Printers.Pretty.ASCII,
      Fallback      => SE.Printers.Pretty.Hexadecimal,
      Hex_Casing    => SE.Encodings.Upper,
      Indent        => SE.Printers.Pretty.Tabs,
      Indentation   => 1,
      Newline       => SE.Printers.Pretty.LF,
      Newline_At    => (SE.Printers.Pretty.Opening =>
                           (SE.Printers.Pretty.Opening => True,
                            SE.Printers.Pretty.Atom_Data => False,
                            SE.Printers.Pretty.Closing => False),
                        SE.Printers.Pretty.Atom_Data =>
                           (SE.Printers.Pretty.Opening => True,
                            SE.Printers.Pretty.Atom_Data => True,
                            SE.Printers.Pretty.Closing => True),
                        SE.Printers.Pretty.Closing =>
                           (SE.Printers.Pretty.Opening => True,
                            SE.Printers.Pretty.Atom_Data => True,
                            SE.Printers.Pretty.Closing => True)),
      Quoted        => SE.Printers.Pretty.When_Shorter,
      Quoted_Escape => SE.Printers.Pretty.Octal_Escape,
      Space_At      => (others => (others => False)),
      Tab_Stop      => 8,
      Token         => SE.Printers.Pretty.Standard_Token,
      Width         => 79);

   To_Print : SE.Printers.Pretty.Parameters := Param;

   Output_Stream : constant Ada.Text_IO.Text_Streams.Stream_Access
     := Ada.Text_IO.Text_Streams.Stream (Ada.Text_IO.Current_Output);

   function Is_Natural (Argument : in String) return Boolean;

   package Options is
      type Id is
        (ASCII,
         Atom,
         Base64_Expr,
         Base64_Atom,
         Canonical,
         Dos_Newline,
         Dump_Config,
         Eight_Bit,
         Extended_Token,
         Load_Config,
         Hex_Atom,
         Hex_Escape,
         Help,
         Newline_At,
         Newline_Encoding,
         No_Indent,
         No_Quoted,
         No_Token,
         Indent,
         Octal_Escape,
         Quoted_Single_Line,
         Quoted_When_Shorter,
         Space_At,
         Tab_Stop,
         Token,
         Unix_Newline,
         UTF_8,
         Verbatim,
         Width,
         Upper_Hex,
         Lower_Hex);

      type Action is
        (Error,
         Print_Help,
         Run,
         Run_Base64,
         Print_Atom,
         Print_Config);
   end Options;

   package Getopt is new Natools.Getopt_Long (Options.Id);

   function Getopt_Config return Getopt.Configuration;

   type Callback is new Getopt.Handlers.Callback with record
      Action    : Options.Action := Options.Run;
      Arg_Count : Natural := 0;
   end record;

   overriding procedure Option
     (Handler  : in out Callback;
      Id       : in Options.Id;
      Argument : in String);

   overriding procedure Argument
     (Handler  : in out Callback;
      Argument : in String);

   procedure Parse_Separator
     (Separator : in out SE.Printers.Pretty.Entity_Separator;
      Image : in String);

   procedure Process
     (Handler : in Callback'Class;
      Input : access Ada.Streams.Root_Stream_Type'Class);

   procedure Process
     (Printer : in out SE.Printers.Pretty.Stream_Printer;
      Input   : access Ada.Streams.Root_Stream_Type'Class;
      Parse   : in Boolean := True);

   procedure Print_Help
     (Opt : in Getopt.Configuration;
      Output : in Ada.Text_IO.File_Type);

   type Base64_Stream (Backend : access Ada.Streams.Root_Stream_Type'Class) is
      new Ada.Streams.Root_Stream_Type with record
         Buffer : Ada.Streams.Stream_Element_Array (1 .. 3);
         Cursor : Ada.Streams.Stream_Element_Offset := 1;
      end record;

   procedure Read
     (Stream : in out Base64_Stream;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);

   procedure Write
     (Stream : in out Base64_Stream;
      Item   : in Ada.Streams.Stream_Element_Array);

   procedure Open (Stream : in out Base64_Stream'Class);
   procedure Close (Stream : in out Base64_Stream'Class);



   function Is_Natural (Argument : in String) return Boolean is
   begin
      if Argument = "" then
         return False;
      end if;

      for I in Argument'Range loop
         if Argument (I) not in '0' .. '9' then
            return False;
         end if;
      end loop;

      return True;
   end Is_Natural;


   function Getopt_Config return Getopt.Configuration is
      use Getopt;
      Result : Getopt.Configuration;
   begin
      Result.Add_Option
        ("8-bit",        '8', No_Argument,       Options.Eight_Bit);
      Result.Add_Option
        ("ASCII",        'A', No_Argument,       Options.ASCII);
      Result.Add_Option
        ("atom",         'a', No_Argument,       Options.Atom);
      Result.Add_Option
        ("brace",        'B', No_Argument,       Options.Base64_Expr);
      Result.Add_Option
        ("base64",       'b', No_Argument,       Options.Base64_Atom);
      Result.Add_Option
        ("canonical",    'c', No_Argument,       Options.Canonical);
      Result.Add_Option
        ("dos-newline",  'D', No_Argument,       Options.Dos_Newline);
      Result.Add_Option
        ("dump-config",  'd', No_Argument,       Options.Dump_Config);
      Result.Add_Option
        ("escape-hex",   'E', No_Argument,       Options.Hex_Escape);
      Result.Add_Option
        ("ext-token",    'e', No_Argument,       Options.Extended_Token);
      Result.Add_Option
        ("config",       'f', Required_Argument, Options.Load_Config);
      Result.Add_Option
        ("hex-atom",     'H', No_Argument,       Options.Hex_Atom);
      Result.Add_Option
        ("help",         'h', No_Argument,       Options.Help);
      Result.Add_Option
        ("no-indent",    'I', No_Argument,       Options.No_Indent);
      Result.Add_Option
        ("indent",       'i', Required_Argument, Options.Indent);
      Result.Add_Option
        ("single-line",  'l', No_Argument,       Options.Quoted_Single_Line);
      Result.Add_Option
        ("newline-at",   'N', Required_Argument, Options.Newline_At);
      Result.Add_Option
        ("nl-encoding",  'n', Required_Argument, Options.Newline_Encoding);
      Result.Add_Option
        ("escape-octal", 'o', No_Argument,       Options.Octal_Escape);
      Result.Add_Option
        ("no-quoted",    'Q', No_Argument,       Options.No_Quoted);
      Result.Add_Option
        ("quoted",       'q', No_Argument,       Options.Quoted_When_Shorter);
      Result.Add_Option
        ("space-at",     'S', Required_Argument, Options.Space_At);
      Result.Add_Option
        ("tab-stop",     's', Required_Argument, Options.Tab_Stop);
      Result.Add_Option
        ("no-token",     'T', No_Argument,       Options.No_Token);
      Result.Add_Option
        ("token",        't', No_Argument,       Options.Token);
      Result.Add_Option
        ("unix-newline", 'U', No_Argument,       Options.Unix_Newline);
      Result.Add_Option
        ("utf-8",        'u', No_Argument,       Options.UTF_8);
      Result.Add_Option
        ("verbatim",     'v', No_Argument,       Options.Verbatim);
      Result.Add_Option
        ("width",        'w', Required_Argument, Options.Width);
      Result.Add_Option
        ("upper-hex",    'X', No_Argument,       Options.Upper_Hex);
      Result.Add_Option
        ("lower-hex",    'x', No_Argument,       Options.Lower_Hex);
      return Result;
   end Getopt_Config;


   overriding procedure Option
     (Handler  : in out Callback;
      Id       : in Options.Id;
      Argument : in String)
   is
      use type Options.Action;
   begin
      case Id is
         when Options.ASCII =>
            Param.Char_Encoding := SE.Printers.Pretty.ASCII;

         when Options.Atom =>
            if Handler.Action in Options.Run .. Options.Print_Config then
               Handler.Action := Options.Print_Atom;
            end if;

         when Options.Base64_Atom =>
            Param.Quoted := SE.Printers.Pretty.No_Quoted;
            Param.Fallback := SE.Printers.Pretty.Base64;

         when Options.Base64_Expr =>
            if Handler.Action in Options.Run .. Options.Print_Config then
               Handler.Action := Options.Run_Base64;
            end if;

         when Options.Canonical =>
            Param.Width := 0;
            Param.Newline_At := (others => (others => False));
            Param.Space_At := (others => (others => False));
            Param.Indentation := 0;
            Param.Quoted := SE.Printers.Pretty.No_Quoted;
            Param.Token := SE.Printers.Pretty.No_Token;
            Param.Fallback := SE.Printers.Pretty.Verbatim;

         when Options.Dos_Newline =>
            Param.Newline := SE.Printers.Pretty.CR_LF;

         when Options.Dump_Config =>
            if Handler.Action in Options.Run .. Options.Print_Config then
               To_Print := Param;
               Handler.Action := Options.Print_Config;
            end if;

         when Options.Eight_Bit =>
            Param.Char_Encoding := SE.Printers.Pretty.Latin;

         when Options.Extended_Token =>
            Param.Token := SE.Printers.Pretty.Extended_Token;

         when Options.Indent =>
            declare
               package Fixed renames Ada.Strings.Fixed;
               package Maps renames Ada.Strings.Maps;
               Last : constant Natural := Fixed.Index
                 (Source => Argument,
                  Set => Maps.To_Set ("0123456789"),
                  Going => Ada.Strings.Backward);
            begin
               if Last = 0 then
                  Handler.Action := Options.Error;
               elsif Last = Argument'Last then
                  Param.Indentation := SE.Printers.Pretty.Screen_Offset'Value
                    (Argument);
               elsif Argument (Last + 1 .. Argument'Last) = "t"
                 or else Argument (Last + 1 .. Argument'Last) = "T"
               then
                  Param.Indentation := SE.Printers.Pretty.Screen_Offset'Value
                    (Argument (Argument'First .. Last));
                  Param.Indent := SE.Printers.Pretty.Tabs;
               elsif Argument (Last + 1 .. Argument'Last) = "s"
                 or else Argument (Last + 1 .. Argument'Last) = "S"
               then
                  Param.Indentation := SE.Printers.Pretty.Screen_Offset'Value
                    (Argument (Argument'First .. Last));
                  Param.Indent := SE.Printers.Pretty.Spaces;
               elsif Argument (Last + 1 .. Argument'Last) = "ts"
                 or else Argument (Last + 1 .. Argument'Last) = "TS"
                 or else Argument (Last + 1 .. Argument'Last) = "st"
                 or else Argument (Last + 1 .. Argument'Last) = "ST"
               then
                  Param.Indentation := SE.Printers.Pretty.Screen_Offset'Value
                    (Argument (Argument'First .. Last));
                  Param.Indent := SE.Printers.Pretty.Tabs_And_Spaces;
               else
                  Handler.Action := Options.Error;
               end if;
            exception
               when Constraint_Error =>
                  Handler.Action := Options.Error;
            end;

         when Options.Hex_Atom =>
            Param.Fallback := SE.Printers.Pretty.Hexadecimal;

         when Options.Hex_Escape =>
            Param.Quoted_Escape := SE.Printers.Pretty.Hex_Escape;

         when Options.Help =>
            Handler.Action := Options.Print_Help;

         when Options.Load_Config =>
            declare
               Conf : SE.Lockable.Descriptor'Class
                := SE.File_Readers.Reader (Argument);
            begin
               SE.Printers.Pretty.Config.Update (Param, Conf);
            end;

         when Options.Newline_At =>
            Parse_Separator (Param.Newline_At, Argument);

         when Options.Newline_Encoding =>
            begin
               case Argument'Length is
                  when 2 =>
                     Param.Newline := SE.Printers.Pretty.Newline_Encoding'Value
                       (Argument);
                  when 4 | 5 =>
                     Param.Newline := SE.Printers.Pretty.Newline_Encoding'Value
                       (Argument (Argument'First .. Argument'First + 1)
                        & '_'
                        & Argument (Argument'Last - 1 .. Argument'Last));
                  when others =>
                     raise Constraint_Error;
               end case;
            exception
               when Constraint_Error =>
                  Ada.Text_IO.Put_Line
                    (Ada.Text_IO.Current_Error,
                     "Invalid newline encoding """ & Argument & '"');
                  Handler.Action := Options.Error;
            end;

         when Options.No_Quoted =>
            Param.Quoted := SE.Printers.Pretty.No_Quoted;

         when Options.No_Indent =>
            Param.Indentation := 0;

         when Options.No_Token =>
            Param.Token := SE.Printers.Pretty.No_Token;

         when Options.Octal_Escape =>
            Param.Quoted_Escape := SE.Printers.Pretty.Octal_Escape;

         when Options.Quoted_Single_Line =>
            Param.Quoted := SE.Printers.Pretty.Single_Line;

         when Options.Quoted_When_Shorter =>
            Param.Quoted := SE.Printers.Pretty.When_Shorter;

         when Options.Space_At =>
            Parse_Separator (Param.Space_At, Argument);

         when Options.Tab_Stop =>
            if Is_Natural (Argument) then
               Param.Tab_Stop
                 := SE.Printers.Pretty.Screen_Offset'Value (Argument);
            else
               Handler.Action := Options.Error;
            end if;

         when Options.Token =>
            Param.Token := SE.Printers.Pretty.Standard_Token;

         when Options.Unix_Newline =>
            Param.Newline := SE.Printers.Pretty.LF;

         when Options.UTF_8 =>
            Param.Char_Encoding := SE.Printers.Pretty.UTF_8;

         when Options.Verbatim =>
            Param.Fallback := SE.Printers.Pretty.Verbatim;

         when Options.Width =>
            if Is_Natural (Argument) then
               Param.Width
                 := SE.Printers.Pretty.Screen_Offset'Value (Argument);
            else
               Handler.Action := Options.Error;
            end if;

         when Options.Upper_Hex =>
            Param.Hex_Casing := SE.Encodings.Upper;

         when Options.Lower_Hex =>
            Param.Hex_Casing := SE.Encodings.Lower;
      end case;
   end Option;


   overriding procedure Argument
     (Handler  : in out Callback;
      Argument : in String)
   is
      use type Options.Action;
   begin
      Handler.Arg_Count := Handler.Arg_Count + 1;

      if Handler.Action not in Options.Run .. Options.Print_Config then
         return;
      end if;

      if Argument = "-" then
         Process
           (Handler,
            Ada.Text_IO.Text_Streams.Stream (Ada.Text_IO.Current_Input));
      else
         declare
            File : Ada.Streams.Stream_IO.File_Type;
         begin
            Ada.Streams.Stream_IO.Open
              (File, Ada.Streams.Stream_IO.In_File, Argument);
            Process
              (Handler,
               Ada.Streams.Stream_IO.Stream (File));
            Ada.Streams.Stream_IO.Close (File);
         end;
      end if;
   end Argument;


   procedure Parse_Separator
     (Separator : in out SE.Printers.Pretty.Entity_Separator;
      Image : in String)
   is
      procedure Parse
        (C : in Character;
         Entity : out SE.Printers.Pretty.Entity;
         Valid : out Boolean);

      procedure Parse
        (C : in Character;
         Entity : out SE.Printers.Pretty.Entity;
         Valid : out Boolean) is
      begin
         case C is
            when '(' | 'o' | 'O' =>
               Entity := SE.Printers.Pretty.Opening;
               Valid := True;
            when ')' | 'c' | 'C' =>
               Entity := SE.Printers.Pretty.Closing;
               Valid := True;
            when 'a' | 'A' | 'd' | 'D' =>
               Entity := SE.Printers.Pretty.Atom_Data;
               Valid := True;
            when others =>
               Valid := False;
         end case;
      end Parse;

      I : Positive := Image'First;
      Before, After : SE.Printers.Pretty.Entity := SE.Printers.Pretty.Opening;
      Valid : Boolean;
      Result : SE.Printers.Pretty.Entity_Separator
        := (others => (others => False));
   begin
      while I + 1 in Image'Range loop
         Parse (Image (I), Before, Valid);

         if Valid then
            Parse (Image (I), After, Valid);

            if Valid then
               Result (Before, After) := True;
            end if;

            I := I + 1;
         end if;

         I := I + 1;
      end loop;

      Separator := Result;
   end Parse_Separator;

   Printer_Direct : SE.Printers.Pretty.Stream_Printer (Output_Stream);
   Base64_Output : aliased Base64_Stream (Output_Stream);
   Printer_Base64 : SE.Printers.Pretty.Stream_Printer (Base64_Output'Access);

   procedure Process
     (Handler : in Callback'Class;
      Input   : access Ada.Streams.Root_Stream_Type'Class) is
   begin
      case Handler.Action is
         when Options.Error | Options.Print_Help | Options.Print_Config =>
            raise Program_Error;

         when Options.Run =>
            Printer_Direct.Set_Parameters (Param);
            Process (Printer_Direct, Input, True);

         when Options.Run_Base64 =>
            if Handler.Arg_Count = 1 then
               Open (Base64_Output);
            end if;
            Printer_Base64.Set_Parameters (Param);
            Process (Printer_Base64, Input, True);

         when Options.Print_Atom =>
            Printer_Direct.Set_Parameters (Param);
            Process (Printer_Direct, Input, False);
      end case;
   end Process;


   procedure Process
     (Printer : in out SE.Printers.Pretty.Stream_Printer;
      Input   : access Ada.Streams.Root_Stream_Type'Class;
      Parse   : in Boolean := True)
   is
      Parser : SE.Parsers.Stream_Parser (Input);
      Event : SE.Events.Event;
   begin
      if Parse then
         loop
            Parser.Next (Event);

            case Event is
               when SE.Events.Error =>
                  raise Program_Error;
               when SE.Events.Open_List =>
                  Printer.Open_List;
               when SE.Events.Close_List =>
                  Printer.Close_List;
               when SE.Events.Add_Atom =>
                  Printer.Append_Atom (Parser.Current_Atom);
               when SE.Events.End_Of_Input =>
                  exit;
            end case;
         end loop;
      else
         declare
            use type Ada.Streams.Stream_Element_Offset;

            Buffer : SE.Atom_Buffers.Atom_Buffer;
            Chunk  : Ada.Streams.Stream_Element_Array (1 .. 1024);
            Last   : Ada.Streams.Stream_Element_Offset;
         begin
            loop
               Input.Read (Chunk, Last);
               exit when Last + 1 = Chunk'First;
               Buffer.Append (Chunk (1 .. Last));
               exit when Last < Chunk'Last;
            end loop;
            Printer.Append_Atom (Buffer.Data
              (Buffer.Data'First .. Buffer.Data'First + Buffer.Length - 1));
         end;
      end if;
   end Process;


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
            when Options.ASCII =>
               New_Line (Output);
               Put_Line (Output, Indent & Indent
                 & "Assume ASCII encoding of output");

            when Options.Atom =>
               New_Line (Output);
               Put_Line (Output, Indent & Indent
                 & "Treat intputs as individual atoms");

            when Options.Base64_Expr =>
               New_Line (Output);
               Put_Line (Output, Indent & Indent
                 & "Output the expression in base-64 encoding");

            when Options.Base64_Atom =>
               New_Line (Output);
               Put_Line (Output, Indent & Indent
                 & "Output atoms using base-64 encoding");

            when Options.Canonical =>
               New_Line (Output);
               Put_Line (Output, Indent & Indent
                 & "Output canonical representation");

            when Options.Dos_Newline =>
               New_Line (Output);
               Put_Line (Output, Indent & Indent
                 & "Use DOS newlines (CR+LF)");

            when Options.Dump_Config =>
               New_Line (Output);
               Put_Line (Output, Indent & Indent
                 & "Output printer configuration instead of input");

            when Options.Eight_Bit =>
               New_Line (Output);
               Put_Line (Output, Indent & Indent
                 & "Assume 8-bit wide encoding of output (e.g. ISO-8859-*)");

            when Options.Extended_Token =>
               New_Line (Output);
               Put_Line (Output, Indent & Indent
                 & "Output atoms as extended tokens when possible");

            when Options.Hex_Atom =>
               New_Line (Output);
               Put_Line (Output, Indent & Indent
                 & "Use hexadecimal encoding for atoms");

            when Options.Hex_Escape =>
               New_Line (Output);
               Put_Line (Output, Indent & Indent
                 & "Use hexadecimal escape sequences in quoted strings");

            when Options.Help =>
               New_Line (Output);
               Put_Line (Output, Indent & Indent
                 & "Display help");

            when Options.Indent =>
               Put_Line (Output, "  <number>[st]");
               Put_Line (Output, Indent & Indent
                 & "Set indentation to that number of spaces or tabs or both");

            when Options.Load_Config =>
               Put_Line (Output, "  <filename>");
               Put_Line (Output, Indent & Indent
                 & "Load printer configuration from file");

            when Options.Newline_At =>
               Put_Line (Output, "  <position list>");
               Put_Line (Output, Indent & Indent
                 & "Insert newlines at the given positions");

            when Options.Newline_Encoding =>
               Put_Line (Output, "  <newline encoding>");
               Put_Line (Output, Indent & Indent
                 & "Use given newline encoding (CR, LF, CR_LF or LF_CR)");

            when Options.No_Quoted =>
               New_Line (Output);
               Put_Line (Output, Indent & Indent
                 & "Disable quoted-string encoding for atoms");

            when Options.No_Indent =>
               New_Line (Output);
               Put_Line (Output, Indent & Indent
                 & "Disable indentation");

            when Options.No_Token =>
               New_Line (Output);
               Put_Line (Output, Indent & Indent
                 & "Disable token encoding for atoms");

            when Options.Octal_Escape =>
               New_Line (Output);
               Put_Line (Output, Indent & Indent
                 & "Use octal escape sequences in quoted strings");

            when Options.Quoted_Single_Line =>
               New_Line (Output);
               Put_Line (Output, Indent & Indent
                 & "Use quoted-string atom encoding when it fits on the line");

            when Options.Quoted_When_Shorter =>
               New_Line (Output);
               Put_Line (Output, Indent & Indent
                 & "Use quoted-string atom encoding when shorter");

            when Options.Space_At =>
               Put_Line (Output, "  <position list>");
               Put_Line (Output, Indent & Indent
                 & "Insert a space at the given positions");

            when Options.Tab_Stop =>
               Put_Line (Output, "  <columns>");
               Put_Line (Output, Indent & Indent
                 & "Place tab stops at each multiple of given length");

            when Options.Token =>
               New_Line (Output);
               Put_Line (Output, Indent & Indent
                 & "Output atoms as standard tokens when possible");

            when Options.Unix_Newline =>
               New_Line (Output);
               Put_Line (Output, Indent & Indent
                 & "Use UNIX newlines (LF)");

            when Options.UTF_8 =>
               New_Line (Output);
               Put_Line (Output, Indent & Indent
                 & "Assume UTF-8 encoding of output");

            when Options.Verbatim =>
               New_Line (Output);
               Put_Line (Output, Indent & Indent
                 & "Output atoms using verbatim encoding");

            when Options.Width =>
               Put_Line (Output, "  <columns>");
               Put_Line (Output, Indent & Indent
                 & "Maximum width of output (0 to disable width limit)");

            when Options.Upper_Hex =>
               New_Line (Output);
               Put_Line (Output, Indent & Indent
                 & "Output atoms using upper-case hexadecimal encoding");

            when Options.Lower_Hex =>
               New_Line (Output);
               Put_Line (Output, Indent & Indent
                 & "Output atoms using lower-case hexadecimal encoding");
         end case;
      end loop;
   end Print_Help;


   procedure Read
     (Stream : in out Base64_Stream;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset) is
   begin
      Stream.Backend.Read (Item, Last);
   end Read;


   procedure Write
     (Stream : in out Base64_Stream;
      Item   : in Ada.Streams.Stream_Element_Array)
   is
      use type Ada.Streams.Stream_Element_Offset;

      I : Ada.Streams.Stream_Element_Offset := Item'First;
   begin
      while Stream.Cursor in Stream.Buffer'Range loop
         if I not in Item'Range then
            return;
         end if;

         Stream.Buffer (Stream.Cursor) := Item (I);
         Stream.Cursor := Stream.Cursor + 1;
         I := I + 1;
      end loop;

      loop
         Stream.Backend.Write (SE.Encodings.Encode_Base64
           (Stream.Buffer));
         exit when I + Stream.Buffer'Length - 1 not in Item'Range;
         Stream.Buffer := Item (I .. I + Stream.Buffer'Length - 1);
         I := I + Stream.Buffer'Length;
      end loop;

      Stream.Cursor := Stream.Buffer'First;
      while I in Item'Range loop
         Stream.Buffer (Stream.Cursor) := Item (I);
         Stream.Cursor := Stream.Cursor + 1;
         I := I + 1;
      end loop;
   end Write;


   procedure Open (Stream : in out Base64_Stream'Class) is
   begin
      Stream.Backend.Write
        ((1 => SE.Encodings.Base64_Expr_Begin));
   end Open;


   procedure Close (Stream : in out Base64_Stream'Class) is
      use type Ada.Streams.Stream_Element_Offset;
   begin
      if Stream.Cursor > Stream.Buffer'First then
         Stream.Backend.Write (SE.Encodings.Encode_Base64
           (Stream.Buffer (Stream.Buffer'First .. Stream.Cursor - 1)));
      end if;

      Stream.Backend.Write
        ((1 => SE.Encodings.Base64_Expr_End));
   end Close;


   Opt_Config : constant Getopt.Configuration := Getopt_Config;
   Handler : Callback;
begin
   Opt_Config.Process (Handler);

   case Handler.Action is
      when Options.Error =>
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
         Print_Help (Opt_Config, Ada.Text_IO.Current_Error);
      when Options.Print_Help =>
         Print_Help (Opt_Config, Ada.Text_IO.Current_Output);
      when Options.Print_Config =>
         Printer_Direct.Set_Parameters (Param);
         SE.Printers.Pretty.Config.Print (Printer_Direct, To_Print);
      when Options.Run .. Options.Print_Atom =>
         if Handler.Arg_Count = 0 then
            Handler.Argument ("-");
         end if;
         if Options."=" (Handler.Action, Options.Run_Base64) then
            Close (Base64_Output);
         end if;
   end case;
end Sxcat;
