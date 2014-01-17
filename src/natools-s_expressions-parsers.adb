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

   function Current_Event (P : in Parser) return Events.Event is
   begin
      return P.Latest;
   end Current_Event;


   function Current_Atom (P : in Parser) return Atom is
   begin
      if P.Latest /= Events.Add_Atom then
         raise Program_Error;
      end if;

      return P.Buffer.Data;
   end Current_Atom;


   function Current_Level (P : in Parser) return Natural is
   begin
      return P.Level;
   end Current_Level;


   procedure Query_Atom
     (P : in Parser;
      Process : not null access procedure (Data : in Atom)) is
   begin
      if P.Latest /= Events.Add_Atom then
         raise Program_Error;
      end if;

      P.Buffer.Query (Process);
   end Query_Atom;


   procedure Read_Atom
     (P      : in Parser;
      Data   : out Atom;
      Length : out Count) is
   begin
      if P.Latest /= Events.Add_Atom then
         raise Program_Error;
      end if;

      P.Buffer.Read (Data, Length);
   end Read_Atom;


   procedure Next_Event
     (P     : in out Parser;
      Input : not null access Ada.Streams.Root_Stream_Type'Class)
   is
      O : Octet;
      Item : Ada.Streams.Stream_Element_Array (1 .. 1);
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      P.Latest := Events.Error;
      loop
         --  Process pending events

         if P.Pending /= Events.End_Of_Input then
            P.Latest := P.Pending;
            P.Pending := Events.End_Of_Input;
            case P.Latest is
               when Events.Open_List  =>
                  P.Level := P.Level + 1;
               when Events.Close_List =>
                  if P.Level > 0 then
                     P.Level := P.Level - 1;
                  end if;
               when others => null;
            end case;
            exit;
         end if;

         --  Read a single octet from source

         if P.Override_Pos < P.Override.Length then
            P.Override_Pos := P.Override_Pos + 1;
            O := P.Override.Element (P.Override_Pos);

            if P.Override_Pos >= P.Override.Length then
               P.Override.Hard_Reset;
               P.Override_Pos := 0;
            end if;
         else
            Input.Read (Item, Last);
            if Last not in Item'Range then
               P.Latest := Events.End_Of_Input;
               exit;
            end if;
            O := Item (Last);
         end if;

         --  Process octet

         case P.Internal.State is
            when Waiting =>
               P.Buffer.Soft_Reset;
               case O is
                  when 0 | Encodings.Space | Encodings.HT
                    | Encodings.CR | Encodings.LF
                    | Encodings.VT | Encodings.FF =>
                     null;
                  when Encodings.List_Begin =>
                     P.Latest := Events.Open_List;
                     P.Level := P.Level + 1;
                  when Encodings.List_End =>
                     P.Latest := Events.Close_List;
                     if P.Level > 0 then
                        P.Level := P.Level - 1;
                     end if;
                  when Encodings.Base64_Atom_Begin =>
                     P.Internal := (State => Base64_Atom,
                                    Chunk => (Data => <>, Length => 0));
                  when Encodings.Base64_Expr_Begin =>
                     P.Internal := (State => Base64_Expr,
                                    Chunk => (Data => <>, Length => 0));
                  when Encodings.Hex_Atom_Begin =>
                     P.Internal := (State => Hex_Atom,
                                    Nibble_Buffer => 0);
                  when Encodings.Quoted_Atom_Begin =>
                     P.Internal := (State => Quoted_Atom,
                                    Escape => (Data => <>, Length => 0));
                  when Encodings.Digit_0 .. Encodings.Digit_9 =>
                     P.Internal := (State => Number);
                     Atom_Buffers.Append (P.Buffer, O);
                  when others =>
                     P.Internal := (State => Token);
                     Atom_Buffers.Append (P.Buffer, O);
               end case;

            when Base64_Atom | Base64_Expr =>
               if Encodings.Is_Base64_Digit (O) then
                  P.Internal.Chunk.Data (P.Internal.Chunk.Length) := O;
                  P.Internal.Chunk.Length := P.Internal.Chunk.Length + 1;
                  if P.Internal.Chunk.Length = 4 then
                     P.Buffer.Append
                       (Encodings.Decode_Base64 (P.Internal.Chunk.Data));
                     P.Internal.Chunk.Length := 0;
                  end if;
               elsif (O = Encodings.Base64_Atom_End
                      and P.Internal.State = Base64_Atom)
                 or (O = Encodings.Base64_Expr_End
                     and P.Internal.State = Base64_Expr)
               then
                  P.Buffer.Append
                    (Encodings.Decode_Base64 (P.Internal.Chunk.Data
                                          (0 .. P.Internal.Chunk.Length - 1)));
                  if P.Internal.State = Base64_Atom then
                     P.Latest := Events.Add_Atom;
                  else
                     P.Override.Append (P.Buffer.Data);
                     P.Buffer.Soft_Reset;
                  end if;
                  P.Internal := (State => Waiting);
               end if;

            when Hex_Atom =>
               if Encodings.Is_Hex_Digit (O) then
                  if Encodings.Is_Hex_Digit (P.Internal.Nibble_Buffer) then
                     P.Buffer.Append
                       (Encodings.Decode_Hex (P.Internal.Nibble_Buffer, O));
                     P.Internal.Nibble_Buffer := 0;
                  else
                     P.Internal.Nibble_Buffer := O;
                  end if;
               elsif O = Encodings.Hex_Atom_End then
                  P.Latest := Events.Add_Atom;
                  P.Internal := (State => Waiting);
               end if;

            when Number =>
               case O is
                  when Encodings.Digit_0 .. Encodings.Digit_9 =>
                     P.Buffer.Append (O);
                  when Encodings.Verbatim_Begin =>
                     P.Internal := (State => Verbatim_Atom, Size => 0);
                     for I in 1 .. P.Buffer.Length loop
                        P.Internal.Size := P.Internal.Size * 10
                          + Count (P.Buffer.Element (I) - Encodings.Digit_0);
                     end loop;
                     P.Buffer.Soft_Reset;
                     if P.Internal.Size = 0 then
                        P.Latest := Events.Add_Atom;
                        P.Internal := (State => Waiting);
                     else
                        P.Buffer.Preallocate (P.Internal.Size);
                     end if;
                  when 0 | Encodings.Space | Encodings.HT
                    | Encodings.CR | Encodings.LF
                    | Encodings.VT | Encodings.FF =>
                     P.Latest := Events.Add_Atom;
                     P.Internal := (State => Waiting);
                  when Encodings.List_Begin =>
                     P.Internal := (State => Waiting);
                     P.Pending := Events.Open_List;
                     P.Latest := Events.Add_Atom;
                  when Encodings.List_End =>
                     P.Internal := (State => Waiting);
                     P.Pending := Events.Close_List;
                     P.Latest := Events.Add_Atom;
                  when Encodings.Base64_Atom_Begin =>
                     P.Internal := (State => Base64_Atom,
                                    Chunk => (Data => <>, Length => 0));
                     P.Buffer.Soft_Reset;
                  when Encodings.Base64_Expr_Begin =>
                     P.Internal := (State => Base64_Expr,
                                    Chunk => (Data => <>, Length => 0));
                     P.Buffer.Soft_Reset;
                  when Encodings.Hex_Atom_Begin =>
                     P.Internal := (State => Hex_Atom,
                                    Nibble_Buffer => 0);
                     P.Buffer.Soft_Reset;
                  when Encodings.Quoted_Atom_Begin =>
                     P.Internal := (State => Quoted_Atom,
                                    Escape => (Data => <>, Length => 0));
                     P.Buffer.Soft_Reset;
                  when others =>
                     P.Buffer.Append (O);
                     P.Internal := (State => Token);
               end case;

            when Quoted_Atom =>
               case P.Internal.Escape.Length is
                  when 0 =>
                     case O is
                        when Encodings.Escape =>
                           P.Internal.Escape.Data (0) := O;
                           P.Internal.Escape.Length := 1;
                        when Encodings.Quoted_Atom_End =>
                           P.Internal := (State => Waiting);
                           P.Latest := Events.Add_Atom;
                        when others =>
                           P.Buffer.Append (O);
                     end case;

                  when 1 =>
                     case O is
                        when Character'Pos ('b') =>
                           P.Buffer.Append (8);
                           P.Internal.Escape.Length := 0;
                        when Character'Pos ('t') =>
                           P.Buffer.Append (9);
                           P.Internal.Escape.Length := 0;
                        when Character'Pos ('n') =>
                           P.Buffer.Append (10);
                           P.Internal.Escape.Length := 0;
                        when Character'Pos ('v') =>
                           P.Buffer.Append (11);
                           P.Internal.Escape.Length := 0;
                        when Character'Pos ('f') =>
                           P.Buffer.Append (12);
                           P.Internal.Escape.Length := 0;
                        when Character'Pos ('r') =>
                           P.Buffer.Append (13);
                           P.Internal.Escape.Length := 0;

                        when Character'Pos (''') | Encodings.Escape
                          | Encodings.Quoted_Atom_End =>
                           P.Buffer.Append (O);
                           P.Internal.Escape.Length := 0;

                        when Encodings.Digit_0 .. Encodings.Digit_0 + 3
                          | Character'Pos ('x')
                          | Encodings.CR | Encodings.LF =>
                           P.Internal.Escape.Data (1) := O;
                           P.Internal.Escape.Length := 2;

                        when others =>
                           P.Buffer.Append (P.Internal.Escape.Data (0));
                           P.Override.Append (O);
                           P.Internal.Escape.Length := 0;
                     end case;

                  when 2 =>
                     if (P.Internal.Escape.Data (1) in Encodings.Digit_0
                                                    .. Encodings.Digit_0 + 3
                         and O in Encodings.Digit_0 .. Encodings.Digit_0 + 7)
                       or (P.Internal.Escape.Data (1) = Character'Pos ('x')
                           and then Encodings.Is_Hex_Digit (O))
                     then
                        P.Internal.Escape.Data (2) := O;
                        P.Internal.Escape.Length := 3;

                     elsif P.Internal.Escape.Data (1) = Encodings.CR
                       or P.Internal.Escape.Data (1) = Encodings.LF
                     then
                        P.Internal.Escape.Length := 0;
                        if not ((O = Encodings.CR or O = Encodings.LF)
                                and O /= P.Internal.Escape.Data (1))
                        then
                           P.Override.Append (O);
                        end if;

                     else
                        P.Buffer.Append
                          ((P.Internal.Escape.Data (0),
                            P.Internal.Escape.Data (1)));
                        P.Override.Append (O);
                        P.Internal.Escape.Length := 0;
                     end if;

                  when 3 =>
                     if P.Internal.Escape.Data (1) = Character'Pos ('x') then
                        if Encodings.Is_Hex_Digit (O) then
                           P.Buffer.Append
                             (Encodings.Decode_Hex (P.Internal.Escape.Data (2),
                                                    O));
                        else
                           P.Buffer.Append
                             ((P.Internal.Escape.Data (0),
                               P.Internal.Escape.Data (1),
                               P.Internal.Escape.Data (2)));
                           P.Override.Append (O);
                        end if;
                     else
                        pragma Assert (P.Internal.Escape.Data (1)
                          in Encodings.Digit_0 .. Encodings.Digit_0 + 3);
                        if O in Encodings.Digit_0 .. Encodings.Digit_0 + 7 then
                           Atom_Buffers.Append
                             (P.Buffer,
                              (P.Internal.Escape.Data (1) - Encodings.Digit_0)
                               * 2**6 +
                              (P.Internal.Escape.Data (2) - Encodings.Digit_0)
                               * 2**3 +
                              (O - Encodings.Digit_0));
                        else
                           P.Buffer.Append
                             ((P.Internal.Escape.Data (0),
                               P.Internal.Escape.Data (1),
                               P.Internal.Escape.Data (2)));
                           P.Override.Append (O);
                        end if;
                     end if;
                     P.Internal.Escape.Length := 0;

                  when 4 =>
                     raise Program_Error;
               end case;

            when Token =>
               case O is
                  when 0 | Encodings.Space | Encodings.HT
                    | Encodings.CR | Encodings.LF
                    | Encodings.VT | Encodings.FF =>
                     P.Internal := (State => Waiting);
                     P.Latest := Events.Add_Atom;
                  when Encodings.List_Begin =>
                     P.Internal := (State => Waiting);
                     P.Pending := Events.Open_List;
                     P.Latest := Events.Add_Atom;
                  when Encodings.List_End =>
                     P.Internal := (State => Waiting);
                     P.Pending := Events.Close_List;
                     P.Latest := Events.Add_Atom;
                  when others =>
                     P.Buffer.Append (O);
               end case;

            when Verbatim_Atom =>
               P.Buffer.Append (O);
               pragma Assert (P.Buffer.Length <= P.Internal.Size);
               if P.Buffer.Length = P.Internal.Size then
                  P.Internal := (State => Waiting);
                  P.Latest := Events.Add_Atom;
               end if;
         end case;

         exit when P.Latest /= Events.Error;
      end loop;
   end Next_Event;



   -------------------------
   -- Subparser functions --
   -------------------------

   function Current_Event (P : in Subparser) return Events.Event is
   begin
      if P.Terminated then
         return Events.End_Of_Input;
      else
         return Current_Event (P.Backend.all);
      end if;
   end Current_Event;


   function Current_Atom (P : in Subparser) return Atom is
   begin
      if P.Terminated then
         raise Constraint_Error;
      else
         return Current_Atom (P.Backend.all);
      end if;
   end Current_Atom;


   function Current_Level (P : in Subparser) return Natural is
   begin
      if P.Terminated then
         return P.Base_Level;
      else
         return Current_Level (P.Backend.all);
      end if;
   end Current_Level;


   procedure Query_Atom
     (P : in Subparser;
      Process : not null access procedure (Data : in Atom)) is
   begin
      if P.Terminated then
         raise Constraint_Error;
      else
         Query_Atom (P.Backend.all, Process);
      end if;
   end Query_Atom;


   procedure Read_Atom
     (P      : in Subparser;
      Data   : out Atom;
      Length : out Count) is
   begin
      if P.Terminated then
         raise Constraint_Error;
      else
         Read_Atom (P.Backend.all, Data, Length);
      end if;
   end Read_Atom;


   procedure Next (P : in out Subparser; Event : out Events.Event) is
   begin
      if P.Terminated then
         raise Constraint_Error;
      end if;

      if not P.Initialized then
         P.Base_Level := Current_Level (P.Backend.all);
         P.Initialized := True;
      end if;

      Next_Event (P.Backend.all, P.Input);

      Event := Current_Event (P.Backend.all);

      if Event = Events.Close_List
        and then Current_Level (P.Backend.all) < P.Base_Level
      then
         P.Terminated := True;
         Event := Events.End_Of_Input;
      end if;
   end Next;


   procedure Finish (P : in out Subparser) is
      Event : Events.Event := Current_Event (P);
   begin
      while Event /= Events.Error and Event /= Events.End_Of_Input loop
         Next (P, Event);
      end loop;
   end Finish;

end Natools.S_Expressions.Parsers;
