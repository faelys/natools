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

package body Natools.S_Expressions is

   function To_String (Data : in Atom) return String is
   begin
      return Result : String (1 .. Data'Length) do
         for I in Result'Range loop
            Result (I) := Character'Val (Data (Data'First + Offset (I - 1)));
         end loop;
      end return;
   end To_String;


   function To_Atom (Data : in String) return Atom is
   begin
      return Result : Atom (0 .. Data'Length - 1) do
         for I in Result'Range loop
            Result (I) := Character'Pos (Data (Data'First + Integer (I)));
         end loop;
      end return;
   end To_Atom;


   function Less_Than (Left, Right : Atom) return Boolean is
   begin
      return Left'Length < Right'Length
        or else (Left'Length = Right'Length and then Left < Right);
   end Less_Than;


   procedure Next (Object : in out Descriptor'Class) is
      Discarded : Events.Event;
      pragma Unreferenced (Discarded);
   begin
      Next (Object, Discarded);
   end Next;

end Natools.S_Expressions;
