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

with Natools.S_Expressions.Interpreter_Loop;
with Natools.S_Expressions.Printers.Pretty.Config.Commands;

package body Natools.S_Expressions.Printers.Pretty.Config is

   procedure Read_Screen_Offset
     (Expression : in Lockable.Descriptor'Class;
      Value : in out Screen_Offset;
      Has_Value : out Boolean);
      --  Decode a screen offset from a S-expression

   procedure Update_Casing
     (Casing : in out Encodings.Hex_Casing;
      Name : in Atom);

   function To_Atom (Value : in Screen_Offset) return Atom;
   function To_Atom (Before, After : in Entity) return Atom;

   function To_String (Value : in Entity) return String;


   procedure Main_Execute
     (Param : in out Parameters;
      Context : in Meaningless_Type;
      Name : in Atom);

   procedure Main_Execute
     (Param : in out Parameters;
      Context : in Meaningless_Type;
      Name : in Atom;
      Arguments : in out Lockable.Descriptor'Class);

   procedure Newline_Execute
     (Param : in out Parameters;
      Context : in Meaningless_Type;
      Name : in Atom);

   procedure Newline_Execute
     (Param : in out Parameters;
      Context : in Meaningless_Type;
      Name : in Atom;
      Arguments : in out Lockable.Descriptor'Class);

   procedure Newline_Interpreter
     (Expression : in out Lockable.Descriptor'Class;
      Param : in out Parameters);

   procedure Quoted_Execute
     (Param : in out Parameters;
      Context : in Meaningless_Type;
      Name : in Atom);

   procedure Quoted_Interpreter
     (Expression : in out Lockable.Descriptor'Class;
      Param : in out Parameters);

   procedure Separator_Execute
     (State : in out Entity_Separator;
      Value : in Boolean;
      Name : in Atom);

   procedure Separator_Execute
     (State : in out Entity_Separator;
      Value : in Boolean;
      Name : in Atom;
      Arguments : in out Lockable.Descriptor'Class);

   procedure Separator_Interpreter
     (Expression : in out Lockable.Descriptor'Class;
      State : in out Entity_Separator;
      Context : in Boolean);


   ------------------
   -- Interpreters --
   ------------------

   procedure Main_Execute
     (Param : in out Parameters;
      Context : in Meaningless_Type;
      Name : in Atom)
   is
      pragma Unreferenced (Context);
      Command : constant String := To_String (Name);
   begin
      case Commands.Main (Command) is
         when Commands.Set_Char_Encoding =>
            Param.Char_Encoding := Commands.To_Character_Encoding (Command);

         when Commands.Set_Fallback =>
            Param.Fallback := Commands.To_Atom_Encoding (Command);
            Update_Casing (Param.Hex_Casing, Name);

         when Commands.Set_Hex_Casing =>
            Param.Hex_Casing := Commands.To_Hex_Casing (Command);

         when Commands.Set_Indentation =>
            Param.Indentation := 0;

         when Commands.Set_Newline_Encoding =>
            Param.Newline := Commands.To_Newline_Encoding (Command);

         when Commands.Set_Quoted =>
            Param.Quoted := Commands.To_Quoted_Option (Command);

         when Commands.Set_Token =>
            Param.Token := Commands.To_Token_Option (Command);

         when Commands.Set_Width =>
            Param.Width := 0;

         when Commands.Set_Newline
           | Commands.Set_Quoted_String
           | Commands.Set_Space_At
           | Commands.Set_Tab_Stop
         =>
            --  Those commands are meaningless without argument
            null;
      end case;
   end Main_Execute;


   procedure Main_Execute
     (Param : in out Parameters;
      Context : in Meaningless_Type;
      Name : in Atom;
      Arguments : in out Lockable.Descriptor'Class)
   is
      pragma Unreferenced (Context);
      Command : constant String := To_String (Name);
   begin
      case Commands.Main (Command) is
         when Commands.Set_Indentation =>
            declare
               Has_Value : Boolean;
               Event : Events.Event;
            begin
               Read_Screen_Offset (Arguments, Param.Indentation, Has_Value);

               if Has_Value and then Param.Indentation /= 0 then
                  Arguments.Next (Event);

                  if Event = Events.Add_Atom then
                     declare
                        Unit : constant String
                          := To_String (Arguments.Current_Atom);
                        Last : Natural := Unit'Last;
                     begin
                        if Last > 0 and then Unit (Last) = 's' then
                           Last := Last - 1;
                        end if;

                        if Unit (Unit'First .. Last) = "tab" then
                           Param.Indent := Tabs;
                        elsif Unit (Unit'First .. Last) = "space" then
                           Param.Indent := Spaces;
                        elsif Unit (Unit'First .. Last) = "tabbed-space" then
                           Param.Indent := Tabs_And_Spaces;
                        end if;
                     end;
                  end if;
               end if;
            end;

         when Commands.Set_Newline =>
            Newline_Interpreter (Arguments, Param);

         when Commands.Set_Quoted_String =>
            Quoted_Interpreter (Arguments, Param);

         when Commands.Set_Space_At =>
            Separator_Interpreter (Arguments, Param.Space_At, True);

         when Commands.Set_Tab_Stop =>
            declare
               Value : Screen_Offset := 0;
               Has_Value : Boolean;
            begin
               Read_Screen_Offset (Arguments, Value, Has_Value);

               if Has_Value and then Value /= 0 then
                  Param.Tab_Stop := Value;
               end if;
            end;

         when Commands.Set_Token =>
            begin
               if Arguments.Current_Event = Events.Add_Atom then
                  Param.Token := Commands.To_Token_Option
                    (To_String (Arguments.Current_Atom));
               end if;
            exception
               when Constraint_Error => null;
            end;

         when Commands.Set_Width =>
            declare
               Has_Value : Boolean;
            begin
               Read_Screen_Offset (Arguments, Param.Width, Has_Value);
            end;

         when Commands.Set_Char_Encoding
           | Commands.Set_Fallback
           | Commands.Set_Hex_Casing
           | Commands.Set_Newline_Encoding
           | Commands.Set_Quoted
         =>
            --  These commands don't do anything with arguments
            null;
      end case;
   end Main_Execute;


   procedure Newline_Execute
     (Param : in out Parameters;
      Context : in Meaningless_Type;
      Name : in Atom)
   is
      pragma Unreferenced (Context);
      Command : constant String := To_String (Name);
   begin
      case Commands.Newline (Command) is
         when Commands.Set_Newline_Command_Encoding =>
            Param.Newline := Commands.To_Newline_Encoding (Command);
         when Commands.Set_Newline_Separator =>
            Separator_Execute (Param.Newline_At, True, Name);
      end case;
   end Newline_Execute;


   procedure Newline_Execute
     (Param : in out Parameters;
      Context : in Meaningless_Type;
      Name : in Atom;
      Arguments : in out Lockable.Descriptor'Class)
   is
      pragma Unreferenced (Context);
      Command : constant String := To_String (Name);
   begin
      case Commands.Newline (Command) is
         when Commands.Set_Newline_Command_Encoding =>
            Param.Newline := Commands.To_Newline_Encoding (Command);
         when Commands.Set_Newline_Separator =>
            Separator_Execute (Param.Newline_At, True, Name, Arguments);
      end case;
   end Newline_Execute;


   procedure Quoted_Execute
     (Param : in out Parameters;
      Context : in Meaningless_Type;
      Name : in Atom)
   is
      pragma Unreferenced (Context);
      Command : constant String := To_String (Name);
   begin
      case Commands.Quoted_String (Command) is
         when Commands.Set_Quoted_Option =>
            Param.Quoted := Commands.To_Quoted_Option (Command);
         when Commands.Set_Quoted_Escape =>
            Param.Quoted_Escape := Commands.To_Quoted_Escape (Command);
            Update_Casing (Param.Hex_Casing, Name);
      end case;
   end Quoted_Execute;


   procedure Separator_Execute
     (State : in out Entity_Separator;
      Value : in Boolean;
      Name : in Atom) is
   begin
      case Commands.Separator (To_String (Name)) is
         when Commands.All_Separators =>
            State := (others => (others => Value));
         when Commands.No_Separators =>
            State := (others => (others => not Value));

         when Commands.Invert_Separators =>
            null;  --  Error, actually

         when Commands.Open_Open =>
            State (Opening, Opening) := Value;
         when Commands.Open_Atom =>
            State (Opening, Atom_Data) := Value;
         when Commands.Open_Close =>
            State (Opening, Closing) := Value;

         when Commands.Atom_Open =>
            State (Atom_Data, Opening) := Value;
         when Commands.Atom_Atom =>
            State (Atom_Data, Atom_Data) := Value;
         when Commands.Atom_Close =>
            State (Atom_Data, Closing) := Value;

         when Commands.Close_Open =>
            State (Closing, Opening) := Value;
         when Commands.Close_Atom =>
            State (Closing, Atom_Data) := Value;
         when Commands.Close_Close =>
            State (Closing, Closing) := Value;
      end case;
   end Separator_Execute;


   procedure Separator_Execute
     (State : in out Entity_Separator;
      Value : in Boolean;
      Name : in Atom;
      Arguments : in out Lockable.Descriptor'Class) is
   begin
      case Commands.Separator (To_String (Name)) is
         when Commands.Invert_Separators =>
            Separator_Interpreter (Arguments, State, not Value);

         when Commands.All_Separators
           | Commands.No_Separators
           | Commands.Open_Open
           | Commands.Open_Atom
           | Commands.Open_Close
           | Commands.Atom_Open
           | Commands.Atom_Atom
           | Commands.Atom_Close
           | Commands.Close_Open
           | Commands.Close_Atom
           | Commands.Close_Close
         =>
            Separator_Execute (State, Value, Name);
      end case;
   end Separator_Execute;



   procedure Main_Interpreter is new Interpreter_Loop
     (Parameters, Meaningless_Type, Main_Execute, Main_Execute);

   procedure Newline_Interpreter is new Interpreter_Loop
     (Parameters, Meaningless_Type, Newline_Execute, Newline_Execute);

   procedure Quoted_Interpreter is new Interpreter_Loop
     (Parameters, Meaningless_Type,
      Dispatch_Without_Argument => Quoted_Execute);

   procedure Interpreter is new Interpreter_Loop
     (Entity_Separator, Boolean, Separator_Execute, Separator_Execute);



   procedure Newline_Interpreter
     (Expression : in out Lockable.Descriptor'Class;
      Param : in out Parameters) is
   begin
      Newline_Interpreter (Expression, Param, Meaningless_Value);
   end Newline_Interpreter;

   procedure Quoted_Interpreter
     (Expression : in out Lockable.Descriptor'Class;
      Param : in out Parameters) is
   begin
      Quoted_Interpreter (Expression, Param, Meaningless_Value);
   end Quoted_Interpreter;

   procedure Separator_Interpreter
     (Expression : in out Lockable.Descriptor'Class;
      State : in out Entity_Separator;
      Context : in Boolean)
     renames Interpreter;



   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

   procedure Read_Screen_Offset
     (Expression : in Lockable.Descriptor'Class;
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
      Expression : in out Lockable.Descriptor'Class) is
   begin
      Main_Interpreter (Expression, Param, Meaningless_Value);
   end Update;

end Natools.S_Expressions.Printers.Pretty.Config;
