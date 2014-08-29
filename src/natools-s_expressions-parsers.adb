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

with Natools.S_Expressions.Encodings;

package body Natools.S_Expressions.Parsers is

   ----------------------
   -- Parser Interface --
   ----------------------

   procedure Reset (Self : in out Parser; Hard : in Boolean := False) is
      Null_Stack : Lockable.Lock_Stack;
   begin
      Self.Internal := (State => Waiting);
      Self.Next_Event := Events.End_Of_Input;
      Self.Latest := Events.Error;
      Self.Level := 0;
      Self.Lock_Stack := Null_Stack;
      Self.Locked := False;

      if Hard then
         Self.Pending.Hard_Reset;
         Self.Buffer.Hard_Reset;
      else
         Self.Pending.Soft_Reset;
         Self.Buffer.Soft_Reset;
      end if;
   end Reset;


   overriding function Current_Event (Self : in Parser) return Events.Event is
   begin
      if Self.Locked then
         return Events.End_Of_Input;
      else
         return Self.Latest;
      end if;
   end Current_Event;


   overriding function Current_Atom (Self : in Parser) return Atom is
   begin
      if Self.Locked or Self.Latest /= Events.Add_Atom then
         raise Program_Error;
      end if;

      return Self.Buffer.Data;
   end Current_Atom;


   overriding function Current_Level (Self : in Parser) return Natural is
   begin
      if Self.Locked then
         return 0;
      else
         return Self.Level - Lockable.Current_Level (Self.Lock_Stack);
      end if;
   end Current_Level;


   overriding procedure Query_Atom
     (Self : in Parser;
      Process : not null access procedure (Data : in Atom)) is
   begin
      if Self.Locked or Self.Latest /= Events.Add_Atom then
         raise Program_Error;
      end if;

      Self.Buffer.Query (Process);
   end Query_Atom;


   overriding procedure Read_Atom
     (Self : in Parser;
      Data : out Atom;
      Length : out Count) is
   begin
      if Self.Locked or Self.Latest /= Events.Add_Atom then
         raise Program_Error;
      end if;

      Self.Buffer.Peek (Data, Length);
   end Read_Atom;


   overriding procedure Next
     (Self : in out Parser;
      Event : out Events.Event)
   is
      O : Octet;
   begin
      if Self.Locked then
         Event := Events.End_Of_Input;
         return;
      end if;

      Self.Latest := Events.Error;
      loop
         --  Process pending events

         if Self.Next_Event /= Events.End_Of_Input then
            Self.Latest := Self.Next_Event;
            Self.Next_Event := Events.End_Of_Input;
            case Self.Latest is
               when Events.Open_List  =>
                  Self.Level := Self.Level + 1;
               when Events.Close_List =>
                  if Self.Level > 0 then
                     Self.Level := Self.Level - 1;
                  end if;
               when others => null;
            end case;
            exit;
         end if;

         --  Read a single octet from source

         if Self.Pending.Length = 0 then
            Read_More (Parser'Class (Self), Self.Pending);

            if Self.Pending.Length = 0 then
               Self.Latest := Events.End_Of_Input;
               exit;
            end if;

            Self.Pending.Invert;
         end if;
         Self.Pending.Pop (O);

         --  Process octet

         case Self.Internal.State is
            when Waiting =>
               Self.Buffer.Soft_Reset;
               case O is
                  when 0 | Encodings.Space | Encodings.HT
                    | Encodings.CR | Encodings.LF
                    | Encodings.VT | Encodings.FF =>
                     null;
                  when Encodings.List_Begin =>
                     Self.Latest := Events.Open_List;
                     Self.Level := Self.Level + 1;
                  when Encodings.List_End =>
                     Self.Latest := Events.Close_List;
                     if Self.Level > 0 then
                        Self.Level := Self.Level - 1;
                     end if;
                  when Encodings.Base64_Atom_Begin =>
                     Self.Internal
                       := (State => Base64_Atom,
                           Chunk => (Data => <>, Length => 0));
                  when Encodings.Base64_Expr_Begin =>
                     Self.Internal
                       := (State => Base64_Expr,
                           Chunk => (Data => <>, Length => 0));
                  when Encodings.Hex_Atom_Begin =>
                     Self.Internal := (State => Hex_Atom, Nibble_Buffer => 0);
                  when Encodings.Quoted_Atom_Begin =>
                     Self.Internal :=
                       (State => Quoted_Atom,
                        Escape => (Data => <>, Length => 0));
                  when Encodings.Digit_0 .. Encodings.Digit_9 =>
                     Self.Internal := (State => Number);
                     Atom_Buffers.Append (Self.Buffer, O);
                  when others =>
                     Self.Internal := (State => Token);
                     Atom_Buffers.Append (Self.Buffer, O);
               end case;

            when Base64_Atom | Base64_Expr =>
               if Encodings.Is_Base64_Digit (O) then
                  Self.Internal.Chunk.Data (Self.Internal.Chunk.Length) := O;
                  Self.Internal.Chunk.Length := Self.Internal.Chunk.Length + 1;
                  if Self.Internal.Chunk.Length = 4 then
                     Self.Buffer.Append
                       (Encodings.Decode_Base64 (Self.Internal.Chunk.Data));
                     Self.Internal.Chunk.Length := 0;
                  end if;
               elsif (O = Encodings.Base64_Atom_End
                      and Self.Internal.State = Base64_Atom)
                 or (O = Encodings.Base64_Expr_End
                     and Self.Internal.State = Base64_Expr)
               then
                  Self.Buffer.Append (Encodings.Decode_Base64
                    (Self.Internal.Chunk.Data
                       (0 .. Self.Internal.Chunk.Length - 1)));
                  if Self.Internal.State = Base64_Atom then
                     Self.Latest := Events.Add_Atom;
                  else
                     Self.Pending.Append_Reverse (Self.Buffer.Data);
                     Self.Buffer.Soft_Reset;
                  end if;
                  Self.Internal := (State => Waiting);
               end if;

            when Hex_Atom =>
               if Encodings.Is_Hex_Digit (O) then
                  if Encodings.Is_Hex_Digit (Self.Internal.Nibble_Buffer) then
                     Self.Buffer.Append
                       (Encodings.Decode_Hex (Self.Internal.Nibble_Buffer, O));
                     Self.Internal.Nibble_Buffer := 0;
                  else
                     Self.Internal.Nibble_Buffer := O;
                  end if;
               elsif O = Encodings.Hex_Atom_End then
                  Self.Latest := Events.Add_Atom;
                  Self.Internal := (State => Waiting);
               end if;

            when Number =>
               case O is
                  when Encodings.Digit_0 .. Encodings.Digit_9 =>
                     Self.Buffer.Append (O);
                  when Encodings.Verbatim_Begin =>
                     Self.Internal := (State => Verbatim_Atom, Size => 0);
                     for I in 1 .. Self.Buffer.Length loop
                        Self.Internal.Size := Self.Internal.Size * 10
                          + Count (Self.Buffer.Element (I)
                          - Encodings.Digit_0);
                     end loop;
                     Self.Buffer.Soft_Reset;
                     if Self.Internal.Size = 0 then
                        Self.Latest := Events.Add_Atom;
                        Self.Internal := (State => Waiting);
                     else
                        Self.Buffer.Preallocate (Self.Internal.Size);
                     end if;
                  when 0 | Encodings.Space | Encodings.HT
                    | Encodings.CR | Encodings.LF
                    | Encodings.VT | Encodings.FF =>
                     Self.Latest := Events.Add_Atom;
                     Self.Internal := (State => Waiting);
                  when Encodings.List_Begin =>
                     Self.Internal := (State => Waiting);
                     Self.Next_Event := Events.Open_List;
                     Self.Latest := Events.Add_Atom;
                  when Encodings.List_End =>
                     Self.Internal := (State => Waiting);
                     Self.Next_Event := Events.Close_List;
                     Self.Latest := Events.Add_Atom;
                  when Encodings.Base64_Atom_Begin =>
                     Self.Internal
                       := (State => Base64_Atom,
                           Chunk => (Data => <>, Length => 0));
                     Self.Buffer.Soft_Reset;
                  when Encodings.Base64_Expr_Begin =>
                     Self.Internal
                       := (State => Base64_Expr,
                           Chunk => (Data => <>, Length => 0));
                     Self.Buffer.Soft_Reset;
                  when Encodings.Hex_Atom_Begin =>
                     Self.Internal := (State => Hex_Atom, Nibble_Buffer => 0);
                     Self.Buffer.Soft_Reset;
                  when Encodings.Quoted_Atom_Begin =>
                     Self.Internal
                       := (State => Quoted_Atom,
                           Escape => (Data => <>, Length => 0));
                     Self.Buffer.Soft_Reset;
                  when others =>
                     Self.Buffer.Append (O);
                     Self.Internal := (State => Token);
               end case;

            when Quoted_Atom =>
               case Self.Internal.Escape.Length is
                  when 0 =>
                     case O is
                        when Encodings.Escape =>
                           Self.Internal.Escape.Data (0) := O;
                           Self.Internal.Escape.Length := 1;
                        when Encodings.Quoted_Atom_End =>
                           Self.Internal := (State => Waiting);
                           Self.Latest := Events.Add_Atom;
                        when others =>
                           Self.Buffer.Append (O);
                     end case;

                  when 1 =>
                     case O is
                        when Character'Pos ('b') =>
                           Self.Buffer.Append (8);
                           Self.Internal.Escape.Length := 0;
                        when Character'Pos ('t') =>
                           Self.Buffer.Append (9);
                           Self.Internal.Escape.Length := 0;
                        when Character'Pos ('n') =>
                           Self.Buffer.Append (10);
                           Self.Internal.Escape.Length := 0;
                        when Character'Pos ('v') =>
                           Self.Buffer.Append (11);
                           Self.Internal.Escape.Length := 0;
                        when Character'Pos ('f') =>
                           Self.Buffer.Append (12);
                           Self.Internal.Escape.Length := 0;
                        when Character'Pos ('r') =>
                           Self.Buffer.Append (13);
                           Self.Internal.Escape.Length := 0;

                        when Character'Pos (''') | Encodings.Escape
                          | Encodings.Quoted_Atom_End =>
                           Self.Buffer.Append (O);
                           Self.Internal.Escape.Length := 0;

                        when Encodings.Digit_0 .. Encodings.Digit_0 + 3
                          | Character'Pos ('x')
                          | Encodings.CR | Encodings.LF =>
                           Self.Internal.Escape.Data (1) := O;
                           Self.Internal.Escape.Length := 2;

                        when others =>
                           Self.Buffer.Append (Self.Internal.Escape.Data (0));
                           Self.Pending.Append (O);
                           Self.Internal.Escape.Length := 0;
                     end case;

                  when 2 =>
                     if (Self.Internal.Escape.Data (1)
                           in Encodings.Digit_0 .. Encodings.Digit_0 + 3
                         and O in Encodings.Digit_0 .. Encodings.Digit_0 + 7)
                       or (Self.Internal.Escape.Data (1) = Character'Pos ('x')
                           and then Encodings.Is_Hex_Digit (O))
                     then
                        Self.Internal.Escape.Data (2) := O;
                        Self.Internal.Escape.Length := 3;

                     elsif Self.Internal.Escape.Data (1) = Encodings.CR
                       or Self.Internal.Escape.Data (1) = Encodings.LF
                     then
                        Self.Internal.Escape.Length := 0;
                        if not ((O = Encodings.CR or O = Encodings.LF)
                                and O /= Self.Internal.Escape.Data (1))
                        then
                           Self.Pending.Append (O);
                        end if;

                     else
                        Self.Buffer.Append
                          ((Self.Internal.Escape.Data (0),
                            Self.Internal.Escape.Data (1)));
                        Self.Pending.Append (O);
                        Self.Internal.Escape.Length := 0;
                     end if;

                  when 3 =>
                     if Self.Internal.Escape.Data (1)
                       = Character'Pos ('x')
                     then
                        if Encodings.Is_Hex_Digit (O) then
                           Self.Buffer.Append
                             (Encodings.Decode_Hex
                                (Self.Internal.Escape.Data (2), O));
                        else
                           Self.Buffer.Append
                             ((Self.Internal.Escape.Data (0),
                               Self.Internal.Escape.Data (1),
                               Self.Internal.Escape.Data (2)));
                           Self.Pending.Append (O);
                        end if;
                     else
                        pragma Assert (Self.Internal.Escape.Data (1)
                          in Encodings.Digit_0 .. Encodings.Digit_0 + 3);
                        if O in Encodings.Digit_0 .. Encodings.Digit_0 + 7 then
                           Atom_Buffers.Append
                             (Self.Buffer,
                              (Self.Internal.Escape.Data (1)
                                 - Encodings.Digit_0)
                               * 2**6 +
                              (Self.Internal.Escape.Data (2)
                                 - Encodings.Digit_0)
                               * 2**3 +
                              (O - Encodings.Digit_0));
                        else
                           Self.Buffer.Append
                             ((Self.Internal.Escape.Data (0),
                               Self.Internal.Escape.Data (1),
                               Self.Internal.Escape.Data (2)));
                           Self.Pending.Append (O);
                        end if;
                     end if;
                     Self.Internal.Escape.Length := 0;

                  when 4 =>
                     raise Program_Error;
               end case;

            when Token =>
               case O is
                  when 0 | Encodings.Space | Encodings.HT
                    | Encodings.CR | Encodings.LF
                    | Encodings.VT | Encodings.FF =>
                     Self.Internal := (State => Waiting);
                     Self.Latest := Events.Add_Atom;
                  when Encodings.List_Begin =>
                     Self.Internal := (State => Waiting);
                     Self.Next_Event := Events.Open_List;
                     Self.Latest := Events.Add_Atom;
                  when Encodings.List_End =>
                     Self.Internal := (State => Waiting);
                     Self.Next_Event := Events.Close_List;
                     Self.Latest := Events.Add_Atom;
                  when others =>
                     Self.Buffer.Append (O);
               end case;

            when Verbatim_Atom =>
               Self.Buffer.Append (O);
               pragma Assert (Self.Buffer.Length <= Self.Internal.Size);
               if Self.Buffer.Length = Self.Internal.Size then
                  Self.Internal := (State => Waiting);
                  Self.Latest := Events.Add_Atom;
               end if;
         end case;

         exit when Self.Latest /= Events.Error;
      end loop;

      if Self.Latest = Events.Close_List
        and then Self.Level < Lockable.Current_Level (Self.Lock_Stack)
      then
         Self.Locked := True;
         Event := Events.End_Of_Input;
      else
         Event := Self.Latest;
      end if;
   end Next;


   overriding procedure Lock
     (Self : in out Parser;
      State : out Lockable.Lock_State) is
   begin
      Lockable.Push_Level (Self.Lock_Stack, Self.Level, State);
   end Lock;


   overriding procedure Unlock
     (Self : in out Parser;
      State : in out Lockable.Lock_State;
      Finish : in Boolean := True)
   is
      Previous_Level : constant Natural
        := Lockable.Current_Level (Self.Lock_Stack);
      Event : Events.Event;
   begin
      Lockable.Pop_Level (Self.Lock_Stack, State);
      State := Lockable.Null_State;

      if Finish then
         Event := Self.Current_Event;
         loop
            case Event is
               when Events.Open_List | Events.Add_Atom =>
                  null;
               when Events.Close_List =>
                  exit when Self.Level < Previous_Level;
               when Events.Error | Events.End_Of_Input =>
                  exit;
            end case;
            Self.Next (Event);
         end loop;
      end if;

      Self.Locked := Self.Level < Lockable.Current_Level (Self.Lock_Stack);
   end Unlock;



   -------------------
   -- Stream Parser --
   -------------------

   overriding procedure Read_More
     (Self : in out Stream_Parser;
      Buffer : out Atom_Buffers.Atom_Buffer)
   is
      Item : Ada.Streams.Stream_Element_Array (1 .. 128);
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      Self.Input.Read (Item, Last);

      if Last in Item'Range then
         Buffer.Append (Item (Item'First .. Last));
      end if;
   end Read_More;

end Natools.S_Expressions.Parsers;
