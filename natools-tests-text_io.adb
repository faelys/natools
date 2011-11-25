------------------------------------------------------------------------------
-- Copyright (c) 2011, Natacha Porté                                        --
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

with Ada.Strings.Fixed;
with Ada.Text_IO;

package body Natools.Tests.Text_IO is

   ------------------------
   -- Helper subprograms --
   ------------------------

   function Indentation (Level : Natural) return String;
      --  Return the indentation string for the given level.

   function Indentation (Report : Text_Reporter) return String;
      --  Return the indentation string for the current level of Report.


   function Indentation (Level : Natural) return String is
      use Ada.Strings.Fixed;
   begin
      return Level * "  ";
   end Indentation;


   function Indentation (Report : Text_Reporter) return String is
   begin
      return Indentation (Natural (Report.Results.Length));
   end Indentation;


   ------------------------
   -- Public subprograms --
   ------------------------

   procedure Section (Report : in out Text_Reporter; Name : String) is
   begin
      Ada.Text_IO.Put_Line (Indentation (Report) & "Section: " & Name);
      Result_Lists.Append (Report.Results, (others => 0));
   end Section;


   procedure End_Section (Report : in out Text_Reporter) is
      Last_Item : Result_Lists.Cursor := Report.Results.Last;
   begin
      Result_Lists.Delete (Report.Results, Last_Item);
   end End_Section;


   procedure Item
     (Report  : in out Text_Reporter;
      Name    : in     String;
      Outcome : in     Result)
   is
      use Ada.Strings.Fixed;

      procedure Process (Position : Result_Lists.Cursor);
      procedure Update (R : in out Result_Summary);

      Indent : constant String := Indentation (Report);
      Text_Size : constant Positive
        := Indent'Length + Name'Length + Max_Result_String_Size + 1;
      Line_Length : constant Natural
        := Natural (Ada.Text_IO.Line_Length);

      procedure Process (Position : Result_Lists.Cursor) is
      begin
         Result_Lists.Update_Element (Report.Results, Position, Update'Access);
      end Process;

      procedure Update (R : in out Result_Summary) is
      begin
         R (Outcome) := R (Outcome) + 1;
      end Update;
   begin
      if Text_Size < Line_Length then
         Ada.Text_IO.Put_Line (Indent & Name
                               & (Line_Length - Text_Size) * " "
                               & Result'Image (Outcome));
      else
         Ada.Text_IO.Put_Line (Indent & Name);
         Ada.Text_IO.Put_Line (Indent & " -> " & Result'Image (Outcome));
      end if;
      Result_Lists.Iterate (Report.Results, Process'Access);
      Report.Total (Outcome) := Report.Total (Outcome) + 1;
   end Item;


   procedure Info (Report : in out Text_Reporter; Text : String) is
      pragma Unreferenced (Report);
   begin
      Ada.Text_IO.Put_Line (Text);
   end Info;


   function Current_Results (Report : Text_Reporter) return Result_Summary is
   begin
      return Result_Lists.Element (Report.Results.Last);
   end Current_Results;

   function Total_Results (Report : Text_Reporter) return Result_Summary is
   begin
      return Report.Total;
   end Total_Results;


   procedure Print_Results (R : Result_Summary) is
      use Ada.Strings.Fixed;
   begin
      for I in R'Range loop
         declare
            Image : constant String := Result'Image (I);
         begin
            Ada.Text_IO.Put_Line
              (Image
              & (Max_Result_String_Size + 1 - Image'Length) * " "
              & Natural'Image (R (I)));
         end;
      end loop;
   end Print_Results;
end Natools.Tests.Text_IO;
