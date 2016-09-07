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

------------------------------------------------------------------------------
-- Command Line Interface for primitives in Natools.Smaz.Tools.             --
------------------------------------------------------------------------------

with Ada.Streams;
with Ada.Text_IO.Text_Streams;
with Natools.S_Expressions.Parsers;
with Natools.Smaz.Tools;

procedure Smaz is
   Input_List : Natools.Smaz.Tools.String_Lists.List;
begin
   Read_Input_List :
   declare
      Input : constant access Ada.Streams.Root_Stream_Type'Class
        := Ada.Text_IO.Text_Streams.Stream (Ada.Text_IO.Current_Input);
      Parser : Natools.S_Expressions.Parsers.Stream_Parser (Input);
   begin
      Parser.Next;
      Natools.Smaz.Tools.Read_List (Input_List, Parser);
   end Read_Input_List;

   Build_Dictionary :
   declare
      procedure Print_Dictionary_In_Ada is
        new Natools.Smaz.Tools.Print_Dictionary_In_Ada (Ada.Text_IO.Put_Line);

      Dictionary : constant Natools.Smaz.Dictionary
        := Natools.Smaz.Tools.To_Dictionary (Input_List, True);
   begin
      Print_Dictionary_In_Ada (Dictionary);
   end Build_Dictionary;
end Smaz;
