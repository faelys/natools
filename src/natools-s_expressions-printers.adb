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

package body Natools.S_Expressions.Printers is

   overriding procedure Open_List (Output : in out Canonical) is
   begin
      Output.Stream.Write ((0 => Encodings.List_Begin));
   end Open_List;


   overriding procedure Append_Atom (Output : in out Canonical;
                                     Data : in Atom)
   is
      Length_Image : constant String := Count'Image (Data'Length);
      Length_Data  : Atom (0 .. Length_Image'Length);
   begin
      Length_Data (0 .. Length_Image'Length - 1) := To_Atom (Length_Image);
      Length_Data (Length_Data'Last) := Encodings.Verbatim_Begin;

      Output.Stream.Write (Length_Data (1 .. Length_Data'Last));
      Output.Stream.Write (Data);
   end Append_Atom;


   overriding procedure Close_List (Output : in out Canonical) is
   begin
      Output.Stream.Write ((0 => Encodings.List_End));
   end Close_List;

end Natools.S_Expressions.Printers;
