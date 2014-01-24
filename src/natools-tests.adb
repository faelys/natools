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

   ------------------------
   -- Helper Subprograms --
   ------------------------

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



   -----------------
   -- Test Object --
   -----------------

   function Item
     (Report : access Reporter'Class;
      Name : String;
      Default_Outcome : Result := Success)
     return Test is
   begin
      return Test'(Ada.Finalization.Limited_Controlled with
         Report => Report,
         Name => Ada.Strings.Unbounded.To_Unbounded_String (Name),
         Info => Info_Lists.Empty_List,
         Outcome => Default_Outcome,
         Finalized => False);
   end Item;


   procedure Set_Result (Object : in out Test; Outcome : in Result) is
   begin
      Object.Outcome := Outcome;
   end Set_Result;


   procedure Info (Object : in out Test; Text : in String) is
   begin
      Object.Info.Append (Text);
   end Info;


   procedure Report_Exception
     (Object : in out Test;
      Ex     : in Ada.Exceptions.Exception_Occurrence;
      Code   : in Result := Error) is
   begin
      Set_Result (Object, Code);
      Info
        (Object,
         "Exception " & Ada.Exceptions.Exception_Name (Ex) & " raised:");
      Info (Object, Ada.Exceptions.Exception_Message (Ex));
   end Report_Exception;


   procedure Fail (Object : in out Test; Text : in String := "") is
   begin
      Set_Result (Object, Fail);
      if Text /= "" then
         Info (Object, Text);
      end if;
   end Fail;


   procedure Error (Object : in out Test; Text : in String := "") is
   begin
      Set_Result (Object, Error);
      if Text /= "" then
         Info (Object, Text);
      end if;
   end Error;


   procedure Skip (Object : in out Test; Text : in String := "") is
   begin
      Set_Result (Object, Skipped);
      if Text /= "" then
         Info (Object, Text);
      end if;
   end Skip;


   overriding procedure Finalize (Object : in out Test) is
      Cursor : Info_Lists.Cursor;
   begin
      if not Object.Finalized then
         Object.Finalized := True;
         Object.Report.Item
           (Ada.Strings.Unbounded.To_String (Object.Name),
            Object.Outcome);
         Cursor := Object.Info.First;
         while Info_Lists.Has_Element (Cursor) loop
            Object.Report.Info (Info_Lists.Element (Cursor));
            Info_Lists.Next (Cursor);
         end loop;
      end if;
   end Finalize;

end Natools.Tests;
