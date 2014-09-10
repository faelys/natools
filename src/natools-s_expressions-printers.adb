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


   procedure Transfer
     (Source : in out Descriptor'Class;
      Target : in out Printer'Class;
      Check_Level : in Boolean := False)
   is
      procedure Print_Atom (Data : in Atom);

      procedure Print_Atom (Data : in Atom) is
      begin
         Target.Append_Atom (Data);
      end Print_Atom;

      Event : Events.Event := Source.Current_Event;
      Starting_Level : constant Natural := Source.Current_Level;
   begin
      loop
         case Event is
            when Events.Error | Events.End_Of_Input =>
               exit;
            when Events.Open_List =>
               Target.Open_List;
            when Events.Close_List =>
               exit when Check_Level
                 and then Source.Current_Level < Starting_Level;
               Target.Close_List;
            when Events.Add_Atom =>
               Source.Query_Atom (Print_Atom'Access);
         end case;

         Source.Next (Event);
      end loop;
   end Transfer;

end Natools.S_Expressions.Printers;
