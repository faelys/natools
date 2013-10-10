------------------------------------------------------------------------------
-- Copyright (c) 2013, Natacha Porté                                        --
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

with Ada.Strings.Maps;

procedure Natools.Chunked_Strings.Tests.Bugfixes
  (Report : in out Natools.Tests.Reporter'Class)
is
   package NT renames Natools.Tests;
begin
   Report.Section ("Extra tests for complete coverage");

   declare
      Name : constant String := "Overreach of Index";
      CS : Chunked_String := To_Chunked_String ("abcd0123");
      N : Natural;
   begin
      CS.Head (4);
      N := CS.Index (Ada.Strings.Maps.To_Set ("0123456789"));

      if N /= 0 then
         Report.Item (Name, NT.Fail);
         Report.Info ("Index of digit" & Natural'Image (N) & ", expected 0.");
      else
         Report.Item (Name, NT.Success);
      end if;
   exception
      when Error : others => NT.Report_Exception (Report, Name, Error);
   end;

   Report.End_Section;
end Natools.Chunked_Strings.Tests.Bugfixes;
