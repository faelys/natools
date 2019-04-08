------------------------------------------------------------------------------
-- Copyright (c) 2015-2019, Natacha Porté                                   --
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

with Ada.Characters.Handling;
with Ada.Strings.Fixed;

package body Natools.S_Expressions.Enumeration_IO is

   function To_Atom (Enumeration_Image : in String) return Atom is
      Result : Atom := S_Expressions.To_Atom (Ada.Strings.Fixed.Translate
        (Enumeration_Image,
         Ada.Characters.Handling.To_Lower'Access));
   begin
      for I in Result'Range loop
         if Result (I) = Character'Pos ('_') then
            Result (I) := Character'Pos ('-');
         end if;
      end loop;

      return Result;
   end To_Atom;


   function To_Image (Data : in Atom) return String is
      Result : String := Ada.Strings.Fixed.Translate
        (To_String (Data),
         Ada.Characters.Handling.To_Upper'Access);
   begin
      for I in Result'Range loop
         if Result (I) = '-' then
            Result (I) := '_';
         end if;
      end loop;

      return Result;
   end To_Image;


   package body Typed_IO is

      function Image (T : Enum) return Atom is
      begin
         return To_Atom (Enum'Image (T));
      end Image;

      function Value (Data : Atom) return Enum is
      begin
         return Enum'Value (To_Image (Data));
      end Value;

      function Value (Data : Atom; Default : Enum) return Enum is
         Img : constant String := To_Image (Data);
      begin
         return Enum'Value (Img);
      exception
         when Constraint_Error =>
            return Default;
      end Value;

   end Typed_IO;

end Natools.S_Expressions.Enumeration_IO;
