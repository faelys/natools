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

with Natools.S_Expressions.Test_Tools;

package body Natools.S_Expressions.Printers.Tests is

   procedure All_Tests (Report : in out NT.Reporter'Class) is
   begin
      Canonical_Test (Report);
   end All_Tests;


   procedure Canonical_Test (Report : in out NT.Reporter'Class) is
      Name : constant String := "Canonical encoding";
   begin
      declare
         Stream : aliased Test_Tools.Memory_Stream;
         Printer : Canonical (Stream'Access);
      begin
         Stream.Set_Expected (To_Atom
           ("3:The(5:quick((5:brown3:fox)()))"
            & "(5:jumps)9:over3:the()4:lazy0:3:dog"));

         Printer.Append_Atom (To_Atom ("The"));
         Printer.Open_List;
         Printer.Append_Atom (To_Atom ("quick"));
         Printer.Open_List;
         Printer.Open_List;
         Printer.Append_Atom (To_Atom ("brown"));
         Printer.Append_Atom (To_Atom ("fox"));
         Printer.Close_List;
         Printer.Open_List;
         Printer.Close_List;
         Printer.Close_List;
         Printer.Close_List;
         Printer.Open_List;
         Printer.Append_Atom (To_Atom ("jumps"));
         Printer.Close_List;
         Printer.Append_Atom (To_Atom ("over3:the"));
         Printer.Open_List;
         Printer.Close_List;
         Printer.Append_Atom (To_Atom ("lazy"));
         Printer.Append_Atom (Null_Atom);
         Printer.Append_Atom (To_Atom ("dog"));

         if Stream.Has_Mismatch
           or else Stream.Unread_Expected /= Null_Atom
         then
            Report.Item (Name, NT.Fail);
            Report.Info ("Mismatch at position"
              & Count'Image (Stream.Mismatch_Index));
            Report.Info ("Left to expect: """
              & To_String (Stream.Unread_Expected) & '"');
            Report.Info ("Written data: """
              & To_String (Stream.Get_Data) & '"');
         else
            Report.Item (Name, NT.Success);
         end if;
      end;
   exception
      when Error : others => Report.Report_Exception (Name, Error);
   end Canonical_Test;

end Natools.S_Expressions.Printers.Tests;
