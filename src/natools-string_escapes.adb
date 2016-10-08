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

package body Natools.String_Escapes is

   subtype Hex_Digit is Natural range 0 .. 15;

   function C_Escape_Hex (C : Character) return String;
      --  Return the string representing C in C-style escaped strings

   function Image (N : Hex_Digit) return Character;
      --  Return upper-case hexadecimal image of a digit



   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

   function C_Escape_Hex (C : Character) return String is
   begin
      case C is
         when Character'Val (0)  => return "\0";
         when Character'Val (8)  => return "\b";
         when Character'Val (9)  => return "\t";
         when Character'Val (10) => return "\n";
         when Character'Val (11) => return "\f";
         when Character'Val (12) => return "\v";
         when Character'Val (13) => return "\r";
         when Character'Val (34) => return "\""";

         when Character'Val (32) | Character'Val (33)
           | Character'Val (35) .. Character'Val (126) =>
            return String'(1 => C);

         when others =>
            declare
               Code : constant Natural := Character'Pos (C);
            begin
               return "\x" & Image (Code / 16) & Image (Code mod 16);
            end;
      end case;
   end C_Escape_Hex;


   function Image (N : Hex_Digit) return Character is
   begin
      case N is
         when 0 .. 9 =>
            return Character'Val (Character'Pos ('0') + N);
         when 10 .. 15 =>
            return Character'Val (Character'Pos ('A') + N - 10);
      end case;
   end Image;



   ----------------------
   -- Public Interface --
   ----------------------

   function C_Escape_Hex
     (S : String;
      Add_Quotes : Boolean := False)
     return String
   is
      Length : Natural := 0;
      O : Positive := 1;
      Sublength : Natural := 0;
   begin
      for I in S'Range loop
         case S (I) is
            when Character'Val (0) | '"'
              | Character'Val (8) .. Character'Val (13) =>
               Length := Length + 2;
            when Character'Val (32) | Character'Val (33)
              | Character'Val (35) .. Character'Val (126) =>
               Length := Length + 1;
            when others =>
               Length := Length + 4;
         end case;
      end loop;

      if Add_Quotes then
         Length := Length + 2;
      end if;

      return Result : String (1 .. Length) do
         if Add_Quotes then
            O := O + 1;
            Result (Result'First) := '"';
            Result (Result'Last) := '"';
         end if;

         for I in S'Range loop
            O := O + Sublength;

            declare
               Img : constant String := C_Escape_Hex (S (I));
            begin
               Sublength := Img'Length;
               Result (O .. O + Sublength - 1) := Img;
            end;
         end loop;
      end return;
   end C_Escape_Hex;

end Natools.String_Escapes;
