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


package body Natools.Tests is

   function To_Result (Succeeded : Boolean) return Result is
   begin
      if Succeeded then
         return Success;
      else
         return Fail;
      end if;
   end To_Result;


   procedure Report_Exception
     (Report    : in out Reporter'Class;
      Test_Name : String;
      Ex        : Ada.Exceptions.Exception_Occurrence;
      Code      : Result := Error) is
   begin
      Item (Report, Test_Name, Code);
      Info (Report,
            "Exception " & Ada.Exceptions.Exception_Name (Ex) & " raised:");
      Info (Report, Ada.Exceptions.Exception_Message (Ex));
   end Report_Exception;

end Natools.Tests;
