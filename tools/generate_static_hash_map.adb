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

------------------------------------------------------------------------------
-- Generate_Static_Hash_Map is a command-line interface to the static hash  --
-- map package generator, using a description from S-expression files.      --
-- It also provides bootstrap for the S-expression description interpreter. --
------------------------------------------------------------------------------

with Ada.Command_Line;
with Ada.Text_IO;
with Natools.Static_Hash_Maps.S_Expressions;
with Natools.S_Expressions.File_Readers;
with Natools.S_Expressions.Lockable;

procedure Generate_Static_Hash_Map is
begin
   if Ada.Command_Line.Argument_Count = 0 then
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Current_Error,
         "Usage: " & Ada.Command_Line.Command_Name
         & " input.sx [other-input.sx ...]");
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Current_Error,
         "   special argument ""--"" instead of input file bootstraps the");
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Current_Error,
         "   descriptor interpreter hash map.");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   for I in 1 .. Ada.Command_Line.Argument_Count loop
      if Ada.Command_Line.Argument (I) = "--" then
         declare
            use Natools.Static_Hash_Maps;
         begin
            Generate_Package
              ("Natools.Static_Hash_Maps.S_Expressions.Command_Maps",
               (Map
                 (Element_Type => "Package_Command",
                  Hash_Package_Name =>
                    "Natools.Static_Hash_Maps.S_Expressions.Command_Pkg",
                  Function_Name => "To_Package_Command",
                  Nodes =>
                    (Node ("private", "Private_Child"),
                     Node ("public", "Public_Child"))),
                Map
                 (Element_Type => "Map_Command",
                  Hash_Package_Name =>
                    "Natools.Static_Hash_Maps.S_Expressions.Command_Map",
                  Function_Name => "To_Map_Command",
                  Nodes =>
                    (Node ("hash-package", "Hash_Package"),
                     Node ("nodes", "Nodes"),
                     Node ("function", "Function_Name"),
                     Node ("not-found", "Not_Found")))),
               Private_Child => True);
         end;
      else
         declare
            Input : Natools.S_Expressions.Lockable.Descriptor'Class
              := Natools.S_Expressions.File_Readers.Reader
                  (Ada.Command_Line.Argument (I));
         begin
            Natools.Static_Hash_Maps.S_Expressions.Generate_Packages
              (Input,
               "from " & Ada.Command_Line.Argument (I));
         end;
      end if;
   end loop;
end Generate_Static_Hash_Map;
