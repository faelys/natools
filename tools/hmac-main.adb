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

with Ada.Command_Line;
with Ada.Streams;
with Ada.Text_IO;
with Ada.Text_IO.Text_Streams;

with Natools.S_Expressions;
with Natools.S_Expressions.Encodings;

procedure HMAC.Main is
begin
   case Ada.Command_Line.Argument_Count is
      when 0 =>
         Ada.Text_IO.Put_Line ("Usage: "
           & Ada.Command_Line.Command_Name
           & " key [message]");

      when 1 =>
         declare
            Context : HMAC_Implementation.Context
              := HMAC_Implementation.Create (Ada.Command_Line.Argument (1));
            Block : Ada.Streams.Stream_Element_Array (1 .. 64);
            Last : Ada.Streams.Stream_Element_Offset;
            Input : constant Ada.Text_IO.Text_Streams.Stream_Access
              := Ada.Text_IO.Text_Streams.Stream (Ada.Text_IO.Current_Input);
         begin
            loop
               Input.Read (Block, Last);
               exit when Last not in Block'Range;
               HMAC_Implementation.Update
                 (Context, Block (Block'First .. Last));
            end loop;

            Ada.Text_IO.Put_Line
              (Natools.S_Expressions.To_String
                (Natools.S_Expressions.Encodings.Encode_Hex
                  (HMAC_Implementation.Digest (Context),
                   Natools.S_Expressions.Encodings.Lower)));
         end;

      when others =>
         for I in 2 .. Ada.Command_Line.Argument_Count loop
            Ada.Text_IO.Put_Line
              (Natools.S_Expressions.To_String
                (Natools.S_Expressions.Encodings.Encode_Hex
                  (HMAC_Implementation.Digest
                     (Ada.Command_Line.Argument (1),
                      Natools.S_Expressions.To_Atom
                        (Ada.Command_Line.Argument (I))),
                   Natools.S_Expressions.Encodings.Lower)));
         end loop;
   end case;
end HMAC.Main;
