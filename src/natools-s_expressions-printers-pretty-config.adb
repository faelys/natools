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

package body Natools.S_Expressions.Printers.Pretty.Config is

   procedure Read_Screen_Offset
     (Expression : in out Lockable.Descriptor'Class;
      Value : in out Screen_Offset;
      Has_Value : out Boolean);
      --  Decode a screen offset from a S-expression

   procedure Update_Casing
     (Casing : in out Encodings.Hex_Casing;
      Name : in Atom);

   function To_Atom (Value : in Screen_Offset) return Atom;
   function To_Atom (Before, After : in Entity) return Atom;

   function To_String (Value : in Entity) return String;



   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

   procedure Read_Screen_Offset
     (Expression : in out Lockable.Descriptor'Class;
      Value : in out Screen_Offset;
      Has_Value : out Boolean)
   is
      Result : Screen_Offset := 0;
   begin
      Has_Value := False;

      if Expression.Current_Event /= Events.Add_Atom then
         return;
      end if;

      declare
         Data : constant Atom := Expression.Current_Atom;
      begin
         if Data'Length = 0 then
            return;
         end if;

         for I in Data'Range loop
            if Data (I) in Encodings.Digit_0 .. Encodings.Digit_9 then
               Result := Result * 10
                 + Screen_Offset (Data (I) - Encodings.Digit_0);
            else
               return;
            end if;
         end loop;

         Has_Value := True;
         Value := Result;
      end;
   end Read_Screen_Offset;


   function To_Atom (Value : in Screen_Offset) return Atom is
      Length : Count;
   begin
      Compute_Length : declare
         Left : Screen_Offset := Value;
      begin
         Length := 1;
         while Left >= 10 loop
            Length := Length + 1;
            Left := Left / 10;
         end loop;
      end Compute_Length;

      return Result : Atom (0 .. Length - 1) do
         declare
            I : Offset := Result'Last;
            Left : Screen_Offset := Value;
         begin
            loop
               Result (I) := Encodings.Digit_0 + Octet (Left mod 10);
               I := I - 1;
               Left := Left / 10;
               exit when Left = 0;
            end loop;
            pragma Assert (I + 1 = Result'First);
         end;
      end return;
   end To_Atom;


   function To_Atom (Before, After : in Entity) return Atom is
   begin
      return To_Atom (To_String (Before) & "-" & To_String (After));
   end To_Atom;


   function To_String (Value : in Entity) return String is
   begin
      case Value is
         when Opening => return "open";
         when Atom_Data => return "atom";
         when Closing => return "close";
      end case;
   end To_String;


   procedure Update_Casing
     (Casing : in out Encodings.Hex_Casing;
      Name : in Atom) is
   begin
      if Name'Length > 5 then
         declare
            Prefix : constant String
              := To_String (Name (Name'First .. Name'First + 4));
         begin
            if Prefix = "upper" then
               Casing := Encodings.Upper;
            elsif Prefix = "lower" then
               Casing := Encodings.Lower;
            end if;
         end;
      end if;
   end Update_Casing;



   ---------------------------------
   -- Public High Level Interface --
   ---------------------------------

   procedure Print
     (Output : in out Printers.Printer'Class;
      Param : in Parameters) is
   begin
      --  Newline_At and Newline

      Output.Open_List;
      Output.Append_Atom (To_Atom ("newline"));
      case Param.Newline is
         when CR =>    Output.Append_Atom (To_Atom ("cr"));
         when LF =>    Output.Append_Atom (To_Atom ("lf"));
         when CR_LF => Output.Append_Atom (To_Atom ("cr-lf"));
         when LF_CR => Output.Append_Atom (To_Atom ("lf-cr"));
      end case;

      if Param.Newline_At = Entity_Separator'(others => (others => True)) then
         Output.Append_Atom (To_Atom ("all"));
      else
         Output.Append_Atom (To_Atom ("none"));
         for Before in Entity loop
            for After in Entity loop
               if Param.Newline_At (Before, After) then
                  Output.Append_Atom (To_Atom (Before, After));
               end if;
            end loop;
         end loop;
      end if;
      Output.Close_List;

      --  Space_At

      Output.Open_List;
      Output.Append_Atom (To_Atom ("space"));
      if Param.Space_At = Entity_Separator'(others => (others => True)) then
         Output.Append_Atom (To_Atom ("all"));
      else
         Output.Append_Atom (To_Atom ("none"));
         for Before in Entity loop
            for After in Entity loop
               if Param.Space_At (Before, After) then
                  Output.Append_Atom (To_Atom (Before, After));
               end if;
            end loop;
         end loop;
      end if;
      Output.Close_List;

      --  Tab_Stop

      Output.Open_List;
      Output.Append_Atom (To_Atom ("tab-stop"));
      Output.Append_Atom (To_Atom (Param.Tab_Stop));
      Output.Close_List;

      --  Width

      if Param.Width > 0 then
         Output.Open_List;
         Output.Append_Atom (To_Atom ("width"));
         Output.Append_Atom (To_Atom (Param.Width));
         Output.Close_List;
      else
         Output.Append_Atom (To_Atom ("no-width"));
      end if;

      --  Indentation and Indent

      if Param.Indentation = 0 then
         Output.Append_Atom (To_Atom ("no-indentation"));
      else
         Output.Open_List;
         Output.Append_Atom (To_Atom ("indentation"));
         Output.Append_Atom (To_Atom (Param.Indentation));
         if Param.Indentation > 1 then
            case Param.Indent is
               when Spaces =>
                  Output.Append_Atom (To_Atom ("spaces"));
               when Tabs =>
                  Output.Append_Atom (To_Atom ("tabs"));
               when Tabs_And_Spaces =>
                  Output.Append_Atom (To_Atom ("tabbed-spaces"));
            end case;
         else
            case Param.Indent is
               when Spaces =>
                  Output.Append_Atom (To_Atom ("space"));
               when Tabs =>
                  Output.Append_Atom (To_Atom ("tab"));
               when Tabs_And_Spaces =>
                  Output.Append_Atom (To_Atom ("tabbed-space"));
            end case;
         end if;
         Output.Close_List;
      end if;

      --  Quoted

      case Param.Quoted is
         when No_Quoted    =>
            Output.Append_Atom (To_Atom ("no-quoted-string"));
         when Single_Line  =>
            Output.Append_Atom (To_Atom ("single-line-quoted-string"));
         when When_Shorter =>
            Output.Append_Atom (To_Atom ("quoted-string-when-shorter"));
      end case;

      --  Quoted_Escape

      Output.Open_List;
      Output.Append_Atom (To_Atom ("escape"));
      case Param.Quoted_Escape is
         when Octal_Escape => Output.Append_Atom (To_Atom ("octal"));
         when Hex_Escape   => Output.Append_Atom (To_Atom ("hexadecimal"));
      end case;
      Output.Close_List;

      --  Token

      Output.Open_List;
      Output.Append_Atom (To_Atom ("token"));
      case Param.Token is
         when No_Token       => Output.Append_Atom (To_Atom ("never"));
         when Extended_Token => Output.Append_Atom (To_Atom ("extended"));
         when Standard_Token => Output.Append_Atom (To_Atom ("standard"));
      end case;
      Output.Close_List;

      --  Char_Encoding

      case Param.Char_Encoding is
         when UTF_8 => Output.Append_Atom (To_Atom ("utf-8"));
         when ASCII => Output.Append_Atom (To_Atom ("ascii"));
         when Latin => Output.Append_Atom (To_Atom ("latin-1"));
      end case;

      --  Hex_Casing

      case Param.Hex_Casing is
         when Encodings.Upper => Output.Append_Atom (To_Atom ("upper-case"));
         when Encodings.Lower => Output.Append_Atom (To_Atom ("lower-case"));
      end case;

      --  Fallback

      case Param.Fallback is
         when Base64      => Output.Append_Atom (To_Atom ("base-64"));
         when Hexadecimal => Output.Append_Atom (To_Atom ("hexadecimal"));
         when Verbatim    => Output.Append_Atom (To_Atom ("verbatim"));
      end case;
   end Print;


   procedure Update
     (Param : in out Parameters;
      Expression : in out Lockable.Descriptor'Class)
   is
      Interpreter : Interpreters.Interpreter := Config_Interpreter;
   begin
      Update (Interpreter, Param, Expression);
   end Update;


   procedure Update
     (Interpreter : in out Interpreters.Interpreter;
      Param : in out Parameters;
      Expression : in out Lockable.Descriptor'Class) is
   begin
      Interpreter.Execute (Expression, Param, True);
   end Update;



   ------------------------------
   -- Interpreter Constructors --
   ------------------------------

   function Config_Interpreter return Interpreters.Interpreter is
      Result : Interpreters.Interpreter;
   begin
      Result.Add_Command (To_Atom ("escape"),
                          Set_Quoted_String'(others => <>));
      Result.Add_Command (To_Atom ("indent"), Set_Indentation'(null record));
      Result.Add_Command (To_Atom ("indentation"),
                          Set_Indentation'(null record));
      Result.Add_Command (To_Atom ("no-indent"),
                          Set_Indentation'(null record));
      Result.Add_Command (To_Atom ("no-indentation"),
                          Set_Indentation'(null record));
      Result.Add_Command (To_Atom ("newline"), Set_Newline'(others => <>));
      Result.Add_Command (To_Atom ("quoted"),
                          Set_Quoted_String'(others => <>));
      Result.Add_Command (To_Atom ("space"), Set_Space_At'(others => <>));
      Result.Add_Command (To_Atom ("tab-stop"), Set_Tab_Stop'(null record));
      Result.Add_Command (To_Atom ("width"), Set_Width'(null record));
      Result.Add_Command (To_Atom ("no-width"), Set_Width'(null record));

      Result.Add_Command (To_Atom ("extended-token"),
                          Set_Token'(Value => Extended_Token));
      Result.Add_Command (To_Atom ("no-token"),
                          Set_Token'(Value => No_Token));
      Result.Add_Command (To_Atom ("standard-token"),
                          Set_Token'(Value => Standard_Token));
      Result.Add_Command (To_Atom ("token"),
                          Set_Token'(Value => Standard_Token));

      Result.Add_Command (To_Atom ("no-quoted"),
                          Set_Quoted'(Value => No_Quoted));
      Result.Add_Command (To_Atom ("no-quoted-string"),
                          Set_Quoted'(Value => No_Quoted));
      Result.Add_Command (To_Atom ("quoted-when-shorter"),
                          Set_Quoted'(Value => When_Shorter));
      Result.Add_Command (To_Atom ("quoted-string-when-shorter"),
                          Set_Quoted'(Value => When_Shorter));
      Result.Add_Command (To_Atom ("single-line-quoted"),
                          Set_Quoted'(Value => Single_Line));
      Result.Add_Command (To_Atom ("single-line-quoted-string"),
                          Set_Quoted'(Value => Single_Line));

      Result.Add_Command (To_Atom ("base64"),
                          Set_Fallback'(Value => Base64));
      Result.Add_Command (To_Atom ("base-64"),
                          Set_Fallback'(Value => Base64));
      Result.Add_Command (To_Atom ("lower-hex"),
                          Set_Fallback'(Value => Hexadecimal));
      Result.Add_Command (To_Atom ("lower-hexa"),
                          Set_Fallback'(Value => Hexadecimal));
      Result.Add_Command (To_Atom ("hex"),
                          Set_Fallback'(Value => Hexadecimal));
      Result.Add_Command (To_Atom ("hexa"),
                          Set_Fallback'(Value => Hexadecimal));
      Result.Add_Command (To_Atom ("hexadecimal"),
                          Set_Fallback'(Value => Hexadecimal));
      Result.Add_Command (To_Atom ("upper-hex"),
                          Set_Fallback'(Value => Hexadecimal));
      Result.Add_Command (To_Atom ("upper-hexa"),
                          Set_Fallback'(Value => Hexadecimal));
      Result.Add_Command (To_Atom ("verbatim"),
                          Set_Fallback'(Value => Verbatim));

      Add_Char_Encoding_Commands (Result);
      Add_Hex_Casing_Commands (Result);
      Add_Newline_Encoding_Commands (Result);
      return Result;
   end Config_Interpreter;


   procedure Add_Char_Encoding_Commands
     (Interpreter : in out Interpreters.Interpreter) is
   begin
      Interpreter.Add_Command (To_Atom ("utf-8"),
                               Set_Char_Encoding'(Value => UTF_8));
      Interpreter.Add_Command (To_Atom ("UTF-8"),
                               Set_Char_Encoding'(Value => UTF_8));
      Interpreter.Add_Command (To_Atom ("utf8"),
                               Set_Char_Encoding'(Value => UTF_8));
      Interpreter.Add_Command (To_Atom ("UTF8"),
                               Set_Char_Encoding'(Value => UTF_8));
      Interpreter.Add_Command (To_Atom ("ascii"),
                               Set_Char_Encoding'(Value => ASCII));
      Interpreter.Add_Command (To_Atom ("ASCII"),
                               Set_Char_Encoding'(Value => ASCII));
      Interpreter.Add_Command (To_Atom ("latin-1"),
                               Set_Char_Encoding'(Value => Latin));
      Interpreter.Add_Command (To_Atom ("latin"),
                               Set_Char_Encoding'(Value => Latin));
      Interpreter.Add_Command (To_Atom ("iso-8859-1"),
                               Set_Char_Encoding'(Value => Latin));
      Interpreter.Add_Command (To_Atom ("ISO-8859-1"),
                               Set_Char_Encoding'(Value => Latin));
   end Add_Char_Encoding_Commands;


   procedure Add_Hex_Casing_Commands
     (Interpreter : in out Interpreters.Interpreter) is
   begin
      Interpreter.Add_Command (To_Atom ("upper"),
                               Set_Hex_Casing'(Value => Encodings.Upper));
      Interpreter.Add_Command (To_Atom ("upper-case"),
                               Set_Hex_Casing'(Value => Encodings.Upper));
      Interpreter.Add_Command (To_Atom ("lower"),
                               Set_Hex_Casing'(Value => Encodings.Lower));
      Interpreter.Add_Command (To_Atom ("lower-case"),
                               Set_Hex_Casing'(Value => Encodings.Lower));
   end Add_Hex_Casing_Commands;


   procedure Add_Quoted_Commands
     (Interpreter : in out Interpreters.Interpreter) is
   begin
      Interpreter.Add_Command (To_Atom ("never"),
                               Set_Quoted'(Value => No_Quoted));
      Interpreter.Add_Command (To_Atom ("single-line"),
                               Set_Quoted'(Value => Single_Line));
      Interpreter.Add_Command (To_Atom ("when-shorter"),
                               Set_Quoted'(Value => When_Shorter));
   end Add_Quoted_Commands;


   procedure Add_Quoted_Escape_Commands
     (Interpreter : in out Interpreters.Interpreter) is
   begin
      Interpreter.Add_Command (To_Atom ("octal"),
                               Set_Quoted_Escape'(Value => Octal_Escape));
      Interpreter.Add_Command (To_Atom ("hex"),
                               Set_Quoted_Escape'(Value => Hex_Escape));
      Interpreter.Add_Command (To_Atom ("hexa"),
                               Set_Quoted_Escape'(Value => Hex_Escape));
      Interpreter.Add_Command (To_Atom ("hexadecimal"),
                               Set_Quoted_Escape'(Value => Hex_Escape));
      Interpreter.Add_Command (To_Atom ("lower-hex"),
                               Set_Quoted_Escape'(Value => Hex_Escape));
      Interpreter.Add_Command (To_Atom ("lower-hexa"),
                               Set_Quoted_Escape'(Value => Hex_Escape));
      Interpreter.Add_Command (To_Atom ("upper-hex"),
                               Set_Quoted_Escape'(Value => Hex_Escape));
      Interpreter.Add_Command (To_Atom ("upper-hexa"),
                               Set_Quoted_Escape'(Value => Hex_Escape));
      Add_Hex_Casing_Commands (Interpreter);
   end Add_Quoted_Escape_Commands;


   procedure Add_Newline_Encoding_Commands
     (Interpreter : in out Interpreters.Interpreter) is
   begin
      Interpreter.Add_Command (To_Atom ("cr"),
                               Set_Newline_Encoding'(Value => CR));
      Interpreter.Add_Command (To_Atom ("CR"),
                               Set_Newline_Encoding'(Value => CR));
      Interpreter.Add_Command (To_Atom ("lf"),
                               Set_Newline_Encoding'(Value => LF));
      Interpreter.Add_Command (To_Atom ("LF"),
                               Set_Newline_Encoding'(Value => LF));
      Interpreter.Add_Command (To_Atom ("CRLF"),
                               Set_Newline_Encoding'(Value => CR_LF));
      Interpreter.Add_Command (To_Atom ("CR-LF"),
                               Set_Newline_Encoding'(Value => CR_LF));
      Interpreter.Add_Command (To_Atom ("crlf"),
                               Set_Newline_Encoding'(Value => CR_LF));
      Interpreter.Add_Command (To_Atom ("cr-lf"),
                               Set_Newline_Encoding'(Value => CR_LF));
      Interpreter.Add_Command (To_Atom ("lf-cr"),
                               Set_Newline_Encoding'(Value => LF_CR));
      Interpreter.Add_Command (To_Atom ("lfcr"),
                               Set_Newline_Encoding'(Value => LF_CR));
      Interpreter.Add_Command (To_Atom ("LF-CR"),
                               Set_Newline_Encoding'(Value => LF_CR));
      Interpreter.Add_Command (To_Atom ("LFCR"),
                               Set_Newline_Encoding'(Value => LF_CR));
   end Add_Newline_Encoding_Commands;


   procedure Add_Separator_Commands
     (Interpreter : in out Interpreters.Interpreter;
      Value : in Boolean;
      Newline : in Boolean) is
   begin
      for Before in Entity loop
         for After in Entity loop
            Interpreter.Add_Command
              (To_Atom (Before, After),
               Set_Separator'(Before, After, Value, Newline));
         end loop;
      end loop;

      Interpreter.Add_Command
        (To_Atom ("all"),
         Set_All_Separators'(Value, Newline));
      Interpreter.Add_Command
        (To_Atom ("none"),
         Set_All_Separators'(not Value, Newline));
      Interpreter.Add_Command
        (To_Atom ("not"),
         Set_All_Separators'(not Value, Newline));
   end Add_Separator_Commands;



   -------------------------
   -- Invididual Commands --
   -------------------------

   procedure Execute
     (Self : in out Set_Width;
      State : in out Parameters;
      Context : in Boolean;
      Name : in Atom)
   is
      pragma Unreferenced (Self, Context, Name);
   begin
      State.Width := 0;
   end Execute;


   procedure Execute
     (Self : in out Set_Width;
      State : in out Parameters;
      Context : in Boolean;
      Cmd : in out Lockable.Descriptor'Class)
   is
      pragma Unreferenced (Self, Context);
      Has_Value : Boolean;
   begin
      Cmd.Next;
      Read_Screen_Offset (Cmd, State.Width, Has_Value);
   end Execute;


   procedure Execute
     (Self : in out Set_Tab_Stop;
      State : in out Parameters;
      Context : in Boolean;
      Cmd : in out Lockable.Descriptor'Class)
   is
      pragma Unreferenced (Self, Context);
      Value : Screen_Offset := 0;
      Has_Value : Boolean;
   begin
      Cmd.Next;
      Read_Screen_Offset (Cmd, Value, Has_Value);
      if Has_Value and then Value /= 0 then
         State.Tab_Stop := Value;
      end if;
   end Execute;


   procedure Execute
     (Self : in out Set_Indentation;
      State : in out Parameters;
      Context : in Boolean;
      Name : in Atom)
   is
      pragma Unreferenced (Self, Context, Name);
   begin
      State.Indentation := 0;
   end Execute;


   procedure Execute
     (Self : in out Set_Indentation;
      State : in out Parameters;
      Context : in Boolean;
      Cmd : in out Lockable.Descriptor'Class)
   is
      pragma Unreferenced (Self, Context);
      Has_Value : Boolean;
      Event : Events.Event;
   begin
      Cmd.Next (Event);
      Read_Screen_Offset (Cmd, State.Indentation, Has_Value);

      if Has_Value and State.Indentation /= 0 then
         Cmd.Next (Event);
         if Event = Events.Add_Atom then
            declare
               Keyword : constant String := To_String (Cmd.Current_Atom);
            begin
               if Keyword = "tab" or Keyword = "tabs" then
                  State.Indent := Tabs;
               elsif Keyword = "space" or Keyword = "spaces" then
                  State.Indent := Spaces;
               elsif Keyword = "tabbed-space" or Keyword = "tabbed-spaces" then
                  State.Indent := Tabs_And_Spaces;
               end if;
            end;
         end if;
      end if;
   end Execute;


   procedure Execute
     (Self : in out Set_Newline;
      State : in out Parameters;
      Context : in Boolean;
      Cmd : in out Lockable.Descriptor'Class) is
   begin
      if Self.Subinterpreter.Is_Empty then
         Add_Separator_Commands (Self.Subinterpreter, True, True);
         Add_Newline_Encoding_Commands (Self.Subinterpreter);
      end if;
      Cmd.Next;
      Self.Subinterpreter.Execute (Cmd, State, Context);
   end Execute;


   procedure Execute
     (Self : in out Set_Space_At;
      State : in out Parameters;
      Context : in Boolean;
      Cmd : in out Lockable.Descriptor'Class) is
   begin
      if Self.Subinterpreter.Is_Empty then
         Add_Separator_Commands (Self.Subinterpreter, True, False);
      end if;
      Cmd.Next;
      Self.Subinterpreter.Execute (Cmd, State, Context);
   end Execute;


   procedure Execute
     (Self : in out Set_Char_Encoding;
      State : in out Parameters;
      Context : in Boolean;
      Name : in Atom)
   is
      pragma Unreferenced (Context, Name);
   begin
      State.Char_Encoding := Self.Value;
   end Execute;


   procedure Execute
     (Self : in out Set_Newline_Encoding;
      State : in out Parameters;
      Context : in Boolean;
      Name : in Atom)
   is
      pragma Unreferenced (Context, Name);
   begin
      State.Newline := Self.Value;
   end Execute;


   procedure Execute
     (Self : in out Set_Separator;
      State : in out Parameters;
      Context : in Boolean;
      Name : in Atom)
   is
      pragma Unreferenced (Context, Name);
   begin
      if Self.Newline then
         State.Newline_At (Self.Before, Self.After) := Self.Value;
      else
         State.Space_At (Self.Before, Self.After) := Self.Value;
      end if;
   end Execute;


   procedure Execute
     (Self : in out Set_All_Separators;
      State : in out Parameters;
      Context : in Boolean;
      Name : in Atom)
   is
      pragma Unreferenced (Context, Name);
   begin
      if Self.Newline then
         State.Newline_At := (others => (others => Self.Value));
      else
         State.Space_At := (others => (others => Self.Value));
      end if;
   end Execute;


   procedure Execute
     (Self : in out Set_All_Separators;
      State : in out Parameters;
      Context : in Boolean;
      Cmd : in out Lockable.Descriptor'Class)
   is
      Subinterpreter : Interpreters.Interpreter;
   begin
      Add_Separator_Commands (Subinterpreter, Self.Value, Self.Newline);
      Subinterpreter.Execute (Cmd, State, Context);
   end Execute;


   procedure Execute
     (Self : in out Set_Hex_Casing;
      State : in out Parameters;
      Context : in Boolean;
      Name : in Atom)
   is
      pragma Unreferenced (Context, Name);
   begin
      State.Hex_Casing := Self.Value;
   end Execute;


   procedure Execute
     (Self : in out Set_Fallback;
      State : in out Parameters;
      Context : in Boolean;
      Name : in Atom)
   is
      pragma Unreferenced (Context);
   begin
      State.Fallback := Self.Value;
      Update_Casing (State.Hex_Casing, Name);
   end Execute;


   procedure Execute
     (Self : in out Set_Token;
      State : in out Parameters;
      Context : in Boolean;
      Name : in Atom)
   is
      pragma Unreferenced (Context, Name);
   begin
      State.Token := Self.Value;
   end Execute;


   procedure Execute
     (Self : in out Set_Token;
      State : in out Parameters;
      Context : in Boolean;
      Cmd : in out Lockable.Descriptor'Class)
   is
      pragma Unreferenced (Self, Context);
      Event : Events.Event;
   begin
      Cmd.Next (Event);
      if Event /= Events.Add_Atom then
         return;
      end if;

      declare
         Token : constant String := To_String (Cmd.Current_Atom);
      begin
         if Token = "standard" then
            State.Token := Standard_Token;
         elsif Token = "extended" then
            State.Token := Extended_Token;
         elsif Token = "never" or Token = "none" or Token = "no" then
            State.Token := No_Token;
         end if;
      end;
   end Execute;


   procedure Execute
     (Self : in out Set_Quoted_Escape;
      State : in out Parameters;
      Context : in Boolean;
      Name : in Atom)
   is
      pragma Unreferenced (Context);
   begin
      State.Quoted_Escape := Self.Value;
      Update_Casing (State.Hex_Casing, Name);
   end Execute;


   procedure Execute
     (Self : in out Set_Quoted;
      State : in out Parameters;
      Context : in Boolean;
      Name : in Atom)
   is
      pragma Unreferenced (Context, Name);
   begin
      State.Quoted := Self.Value;
   end Execute;


   procedure Execute
     (Self : in out Set_Quoted_String;
      State : in out Parameters;
      Context : in Boolean;
      Cmd : in out Lockable.Descriptor'Class) is
   begin
      if Self.Subinterpreter.Is_Empty then
         Add_Quoted_Commands (Self.Subinterpreter);
         Add_Quoted_Escape_Commands (Self.Subinterpreter);
      end if;

      Cmd.Next;
      Self.Subinterpreter.Execute (Cmd, State, Context);
   end Execute;

end Natools.S_Expressions.Printers.Pretty.Config;
