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

with Natools.S_Expressions.Atom_Ref_Constructors;
with Natools.S_Expressions.Interpreter_Loop;
with Natools.Static_Maps.S_Expressions.Templates.Integers;

package body Natools.S_Expressions.Templates.Generic_Integers is

   package Commands
     renames Natools.Static_Maps.S_Expressions.Templates.Integers;

   function Create (Data : Atom) return Atom_Refs.Immutable_Reference
     renames Atom_Ref_Constructors.Create;


   procedure Update_Format
     (State : in out Format;
      Context : in Meaningless_Type;
      Name : in Atom;
      Arguments : in out Lockable.Descriptor'Class);


   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

   procedure Update_Format
     (State : in out Format;
      Context : in Meaningless_Type;
      Name : in Atom;
      Arguments : in out Lockable.Descriptor'Class)
   is
      pragma Unreferenced (Context);
      Command : constant String := To_String (Name);
      Event : Events.Event;
   begin
      case Commands.Main (Command) is
         when Commands.Error =>
            null;

         when Commands.Align =>
            case Arguments.Current_Event is
               when Events.Add_Atom =>
                  case Commands.To_Align_Command
                    (To_String (Arguments.Current_Atom))
                  is
                     when Commands.Unknown_Align =>
                        null;
                     when Commands.Set_Left =>
                        State.Set_Align (Left_Aligned);
                     when Commands.Set_Center =>
                        State.Set_Align (Centered);
                     when Commands.Set_Right =>
                        State.Set_Align (Right_Aligned);
                  end case;
               when others =>
                  null;
            end case;

         when Commands.Align_Center =>
            State.Set_Align (Centered);

         when Commands.Align_Left =>
            State.Set_Align (Left_Aligned);

         when Commands.Align_Right =>
            State.Set_Align (Right_Aligned);

         when Commands.Base =>
            State.Set_Symbols (Arguments);

         when Commands.Padding =>
            case Arguments.Current_Event is
               when Events.Add_Atom =>
                  State.Left_Padding := Create (Arguments.Current_Atom);
                  State.Right_Padding := State.Left_Padding;
               when others =>
                  return;
            end case;

            Arguments.Next (Event);
            case Event is
               when Events.Add_Atom =>
                  State.Set_Right_Padding (Arguments.Current_Atom);
               when others =>
                  null;
            end case;

         when Commands.Padding_Left =>
            case Arguments.Current_Event is
               when Events.Add_Atom =>
                  State.Set_Left_Padding (Arguments.Current_Atom);
               when others =>
                  null;
            end case;

         when Commands.Padding_Right =>
            case Arguments.Current_Event is
               when Events.Add_Atom =>
                  State.Set_Right_Padding (Arguments.Current_Atom);
               when others =>
                  null;
            end case;

         when Commands.Sign =>
            case Arguments.Current_Event is
               when Events.Add_Atom =>
                  State.Set_Positive_Sign (Arguments.Current_Atom);
               when others =>
                  return;
            end case;

            Arguments.Next (Event);
            case Event is
               when Events.Add_Atom =>
                  State.Set_Negative_Sign (Arguments.Current_Atom);
               when others =>
                  null;
            end case;

         when Commands.Width =>
            case Arguments.Current_Event is
               when Events.Add_Atom =>
                  declare
                     New_Width : constant Width
                       := Width'Value (To_String (Arguments.Current_Atom));
                  begin
                     State.Set_Maximum_Width (New_Width);
                     State.Set_Minimum_Width (New_Width);
                  end;
               when others =>
                  return;
            end case;

            Arguments.Next (Event);
            case Event is
               when Events.Add_Atom =>
                  State.Set_Maximum_Width
                    (Width'Value (To_String (Arguments.Current_Atom)));
               when others =>
                  return;
            end case;

            Arguments.Next (Event);
            case Event is
               when Events.Add_Atom =>
                  State.Set_Overflow_Message (Arguments.Current_Atom);
               when others =>
                  return;
            end case;

         when Commands.Width_Max =>
            case Arguments.Current_Event is
               when Events.Add_Atom =>
                  State.Set_Maximum_Width
                    (Width'Value (To_String (Arguments.Current_Atom)));
               when others =>
                  return;
            end case;

            Arguments.Next (Event);
            case Event is
               when Events.Add_Atom =>
                  State.Set_Overflow_Message (Arguments.Current_Atom);
               when others =>
                  return;
            end case;

         when Commands.Width_Min =>
            case Arguments.Current_Event is
               when Events.Add_Atom =>
                  State.Set_Minimum_Width
                    (Width'Value (To_String (Arguments.Current_Atom)));
               when others =>
                  null;
            end case;
      end case;
   end Update_Format;


   procedure Interpreter is new Interpreter_Loop
     (Format, Meaningless_Type, Update_Format);



   -------------------------
   -- Dynamic Atom Arrays --
   -------------------------

   function Create (Atom_List : in out S_Expressions.Descriptor'Class)
     return Atom_Array
   is
      function Current_Atom return Atom is (Atom_List.Current_Atom);
      New_Ref : Atom_Refs.Immutable_Reference;
   begin
      case Atom_List.Current_Event is
         when Events.Add_Atom =>
            New_Ref := Atom_Refs.Create (Current_Atom'Access);
            Atom_List.Next;
            return (0 => New_Ref) & Create (Atom_List);

         when others =>
            return Atom_Array'(1 .. 0 => <>);
      end case;
   end Create;


   function Create (Atom_List : in out S_Expressions.Descriptor'Class)
     return Atom_Arrays.Immutable_Reference
   is
      function Create_Array return Atom_Array is (Create (Atom_List));
   begin
      return Atom_Arrays.Create (Create_Array'Access);
   end Create;


   function Decimal return Atom_Arrays.Immutable_Reference is
      function Create return Atom_Array
        is ((0 => Create ((1 => Character'Pos ('0'))),
             1 => Create ((1 => Character'Pos ('1'))),
             2 => Create ((1 => Character'Pos ('2'))),
             3 => Create ((1 => Character'Pos ('3'))),
             4 => Create ((1 => Character'Pos ('4'))),
             5 => Create ((1 => Character'Pos ('5'))),
             6 => Create ((1 => Character'Pos ('6'))),
             7 => Create ((1 => Character'Pos ('7'))),
             8 => Create ((1 => Character'Pos ('8'))),
             9 => Create ((1 => Character'Pos ('9')))));
   begin
      if Base_10.Is_Empty then
         Base_10 := Atom_Arrays.Create (Create'Access);
      end if;

      return Base_10;
   end Decimal;


   procedure Reverse_Render
     (Value : in Natural_T;
      Symbols : in Atom_Array;
      Output : in out Atom_Buffers.Atom_Buffer;
      Length : out Width)
   is
      Digit : Natural_T;
      Remainder : Natural_T := Value;
   begin
      Length := 0;
      loop
         Digit := Remainder mod Symbols'Length;
         Remainder := Remainder / Symbols'Length;
         Length := Length + 1;
         Output.Append (Symbols (Digit).Query.Data.all);
         exit when Remainder = 0;
      end loop;
   end Reverse_Render;



   ----------------------
   -- Public Interface --
   ----------------------

   function Render (Value : T; Template : Format) return Atom is
      function "*" (Count : Width; Symbol : Atom) return Atom;

      function Safe_Atom
        (Ref : Atom_Refs.Immutable_Reference;
         Fallback : String)
        return Atom;
--      The expression below seems to trigger an infinite loop in
--      GNAT-AUX 4.9.0 20140422, but the if-statement form doesn't.
--      is (if Ref.Is_Empty then To_Atom (Fallback) else Ref.Query.Data.all);

      function Safe_Atom
        (Ref : Atom_Refs.Immutable_Reference;
         Fallback : String)
        return Atom is
      begin
         if Ref.Is_Empty then
            return To_Atom (Fallback);
         else
            return Ref.Query.Data.all;
         end if;
      end Safe_Atom;

      function "*" (Count : Width; Symbol : Atom) return Atom is
         Result : Atom (1 .. Offset (Count) * Symbol'Length);
      begin
         for I in 0 .. Offset (Count) - 1 loop
            Result (I * Symbol'Length + 1 .. (I + 1) * Symbol'Length)
              := Symbol;
         end loop;

         return Result;
      end "*";

      Output : Atom_Buffers.Atom_Buffer;
      Has_Sign : Boolean := True;
      Length : Width;
      Symbols : constant Atom_Arrays.Immutable_Reference
        := (if Template.Symbols.Is_Empty then Decimal else Template.Symbols);
   begin
      if Value < 0 then
         Reverse_Render (-Value, Symbols.Query.Data.all, Output, Length);
         Output.Append (Safe_Atom (Template.Negative_Sign, "-"));
      else
         Reverse_Render (Value, Symbols.Query.Data.all, Output, Length);

         if not Template.Positive_Sign.Is_Empty then
            Output.Append (Template.Positive_Sign.Query.Data.all);
         else
            Has_Sign := False;
         end if;
      end if;

      Output.Invert;

      if Has_Sign then
         Length := Length + 1;
      end if;

      if Length > Template.Maximum_Width then
         return Safe_Atom (Template.Overflow_Message, "");
      end if;

      if Length < Template.Minimum_Width then
         declare
            Needed : constant Width := Template.Minimum_Width - Length;
            Left_Count, Right_Count : Width := 0;
         begin
            case Template.Align is
               when Left_Aligned =>
                  Right_Count := Needed;
               when Centered =>
                  Left_Count := Needed / 2;
                  Right_Count := Needed - Left_Count;
               when Right_Aligned =>
                  Left_Count := Needed;
            end case;

            return Left_Count * Safe_Atom (Template.Left_Padding, " ")
              & Output.Data
              & Right_Count * Safe_Atom (Template.Right_Padding, " ");
         end;
      end if;

      return Output.Data;
   end Render;


   procedure Parse
     (Template : in out Format;
      Expression : in out Lockable.Descriptor'Class) is
   begin
      Interpreter (Expression, Template, Meaningless_Value);
   end Parse;


   procedure Render
     (Output : in out Ada.Streams.Root_Stream_Type'Class;
      Template : in out Lockable.Descriptor'Class;
      Value : in T)
   is
      Parsed_Template : Format;
   begin
      Parse (Parsed_Template, Template);
      Output.Write (Render (Value, Parsed_Template));
   end Render;


   procedure Render
     (Output : in out Ada.Streams.Root_Stream_Type'Class;
      Default_Format : in Format;
      Template : in out Lockable.Descriptor'Class;
      Value : in T)
   is
      Parsed_Template : Format := Default_Format;
   begin
      Parse (Parsed_Template, Template);
      Output.Write (Render (Value, Parsed_Template));
   end Render;



   ---------------------
   -- Format Mutators --
   ---------------------

   procedure Set_Align (Object : in out Format; Value : in Alignment) is
   begin
      Object.Align := Value;
   end Set_Align;


   procedure Set_Left_Padding
     (Object : in out Format;
      Symbol : in Atom_Refs.Immutable_Reference) is
   begin
      Object.Left_Padding := Symbol;
   end Set_Left_Padding;


   procedure Set_Left_Padding
     (Object : in out Format;
      Symbol : in Atom) is
   begin
      Set_Left_Padding (Object, Create (Symbol));
   end Set_Left_Padding;


   procedure Set_Maximum_Width (Object : in out Format; Value : in Width) is
   begin
      Object.Maximum_Width := Value;

      if Object.Minimum_Width > Object.Maximum_Width then
         Object.Minimum_Width := Value;
      end if;
   end Set_Maximum_Width;


   procedure Set_Minimum_Width (Object : in out Format; Value : in Width) is
   begin
      Object.Minimum_Width := Value;

      if Object.Minimum_Width > Object.Maximum_Width then
         Object.Maximum_Width := Value;
      end if;
   end Set_Minimum_Width;


   procedure Set_Negative_Sign
     (Object : in out Format;
      Sign : in Atom_Refs.Immutable_Reference) is
   begin
      Object.Negative_Sign := Sign;
   end Set_Negative_Sign;


   procedure Set_Negative_Sign
     (Object : in out Format;
      Sign : in Atom) is
   begin
      Set_Negative_Sign (Object, Create (Sign));
   end Set_Negative_Sign;


   procedure Set_Overflow_Message
     (Object : in out Format;
      Message : in Atom_Refs.Immutable_Reference) is
   begin
      Object.Overflow_Message := Message;
   end Set_Overflow_Message;


   procedure Set_Overflow_Message
     (Object : in out Format;
      Message : in Atom) is
   begin
      Set_Overflow_Message (Object, Create (Message));
   end Set_Overflow_Message;


   procedure Set_Positive_Sign
     (Object : in out Format;
      Sign : in Atom_Refs.Immutable_Reference) is
   begin
      Object.Positive_Sign := Sign;
   end Set_Positive_Sign;


   procedure Set_Positive_Sign
     (Object : in out Format;
      Sign : in Atom) is
   begin
      Set_Positive_Sign (Object, Create (Sign));
   end Set_Positive_Sign;


   procedure Set_Right_Padding
     (Object : in out Format;
      Symbol : in Atom_Refs.Immutable_Reference) is
   begin
      Object.Right_Padding := Symbol;
   end Set_Right_Padding;


   procedure Set_Right_Padding
     (Object : in out Format;
      Symbol : in Atom) is
   begin
      Set_Right_Padding (Object, Create (Symbol));
   end Set_Right_Padding;


   procedure Set_Symbols
     (Object : in out Format;
      Symbols : in Atom_Arrays.Immutable_Reference) is
   begin
      if not Symbols.Is_Empty and then Symbols.Query.Data.all'Length >= 2 then
         Object.Symbols := Symbols;
      end if;
   end Set_Symbols;


   procedure Set_Symbols
     (Object : in out Format;
      Expression : in out S_Expressions.Descriptor'Class) is
   begin
      Set_Symbols (Object, Create (Expression));
   end Set_Symbols;

end Natools.S_Expressions.Templates.Generic_Integers;
