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

with Ada.Exceptions;
with Ada.Strings.Fixed;

procedure Natools.Chunked_Strings.Tests.Coverage
  (Report : in out Natools.Tests.Reporter'Class)
is
   package NT renames Natools.Tests;
begin
   NT.Section (Report, "Extra tests for complete coverage");

   declare
      Name : constant String := "Index_Error raised in Element";
      C : Character;
   begin
      C := Element (To_Chunked_String (Name), Name'Length + 1);
      NT.Item (Report, Name, NT.Fail);
      NT.Info (Report, "No exception has been raised.");
      NT.Info (Report, "Return value: " & Character'Image (C));
   exception
      when Ada.Strings.Index_Error =>
         NT.Item (Report, Name, NT.Success);
      when Error : others =>
         NT.Item (Report, Name, NT.Fail);
         NT.Info (Report, "Wrong exception "
                          & Ada.Exceptions.Exception_Name (Error)
                          & "has been raised.");
   end;

   declare
      Name : constant String := "Index_Error raised in Replace_Element";
      CS   : Chunked_String := To_Chunked_String (Name);
   begin
      Replace_Element (CS, Name'Length + 1, '*');
      NT.Item (Report, Name, NT.Fail);
      NT.Info (Report, "No exception has been raised.");
      NT.Info (Report, "Final value: """ & To_String (CS) & '"');
   exception
      when Ada.Strings.Index_Error =>
         NT.Item (Report, Name, NT.Success);
      when Error : others =>
         NT.Item (Report, Name, NT.Fail);
         NT.Info (Report, "Wrong exception "
                          & Ada.Exceptions.Exception_Name (Error)
                          & "has been raised.");
   end;

   declare
      Name : constant String := "Function Duplicate";
      S, T : Chunked_String;
   begin
      S := To_Chunked_String (Name);
      T := Duplicate (S);
      S.Unappend ("cate");

      if To_String (T) /= Name then
         NT.Item (Report, Name, NT.Fail);
         NT.Info (Report,
           "Duplicate """ & To_String (T) & " does not match original");
      else
         NT.Item (Report, Name, NT.Success);
      end if;
   exception
      when Error : others => NT.Report_Exception (Report, Name, Error);
   end;

   declare
      Name : constant String
        := "Procedure Set_Chunked_String (Chunked_String, String)";
      CS : Chunked_String;
   begin
      Set_Chunked_String (CS, Name);

      if To_String (CS) /= Name then
         NT.Item (Report, Name, NT.Fail);
         NT.Info (Report, "Final value: """ & To_String (CS) & '"');
         NT.Info (Report, "Expected: """ & Name & '"');
      else
         NT.Item (Report, Name, NT.Success);
      end if;
   exception
      when Error : others => NT.Report_Exception (Report, Name, Error);
   end;

   declare
      Name : constant String := "Function Delete with empty range";
      CS : Chunked_String;
   begin
      CS := Delete (To_Chunked_String (Name), 1, 0);

      if To_String (CS) /= Name then
         NT.Item (Report, Name, NT.Fail);
         NT.Info (Report, "Final value: """ & To_String (CS) & '"');
         NT.Info (Report, "Expected: """ & Name & '"');
      else
         NT.Item (Report, Name, NT.Success);
      end if;
   exception
      when Error : others => NT.Report_Exception (Report, Name, Error);
   end;

   declare
      Name : constant String
        := "Function Head with oversized Count and parameter override";
      Pad : constant Character := ' ';
      Shadow : constant String (1 .. Name'Length) := (others => Pad);
      CS : Chunked_String;
   begin
      CS := Head (To_Chunked_String (Name), 2 * Name'Length, Pad, 10, 5);

      if To_String (CS) /= Name & Shadow then
         NT.Item (Report, Name, NT.Fail);
         NT.Info (Report, "Final value: """ & To_String (CS) & '"');
         NT.Info (Report, "Expected: """ & Name & Shadow & '"');
      else
         NT.Item (Report, Name, NT.Success);
      end if;
   exception
      when Error : others => NT.Report_Exception (Report, Name, Error);
   end;

   declare
      Name : constant String
        := "Function Tail with oversized Count and parameter override";
      Pad : constant Character := ' ';
      Shadow : constant String (1 .. Name'Length) := (others => Pad);
      CS : Chunked_String;
   begin
      CS := Tail (To_Chunked_String (Name), 2 * Name'Length, Pad, 10, 5);

      if To_String (CS) /= Shadow & Name then
         NT.Item (Report, Name, NT.Fail);
         NT.Info (Report, "Final value: """ & To_String (CS) & '"');
         NT.Info (Report, "Expected: """ & Shadow & Name & '"');
      else
         NT.Item (Report, Name, NT.Success);
      end if;
   exception
      when Error : others => NT.Report_Exception (Report, Name, Error);
   end;

   declare
      Name : constant String
        := "Function ""*"" (Character) over multiple chunks";
      CS : Chunked_String;
      Count : constant Positive := 3 * Default_Chunk_Size + 2;
      Template : constant Character := '$';
      Ref : constant String := Ada.Strings.Fixed."*" (Count, Template);
   begin
      CS := Count * Template;

      if To_String (CS) /= Ref then
         NT.Item (Report, Name, NT.Fail);
         NT.Info (Report, "Final value: """ & To_String (CS) & '"');
         NT.Info (Report, "Expected: """ & Ref & '"');
      else
         NT.Item (Report, Name, NT.Success);
      end if;
   exception
      when Error : others => NT.Report_Exception (Report, Name, Error);
   end;

   declare
      Name : constant String := "Function ""*"" (String) over multiple chunks";
      CS : Chunked_String;
      Count : constant Positive := Default_Chunk_Size + 2;
      Template : constant String := "<>";
      Ref : constant String := Ada.Strings.Fixed."*" (Count, Template);
   begin
      CS := Count * Template;

      if To_String (CS) /= Ref then
         NT.Item (Report, Name, NT.Fail);
         NT.Info (Report, "Final value: """ & To_String (CS) & '"');
         NT.Info (Report, "Expected: """ & Ref & '"');
      else
         NT.Item (Report, Name, NT.Success);
      end if;
   exception
      when Error : others => NT.Report_Exception (Report, Name, Error);
   end;

   declare
      Name : constant String := "Procedure Chunked_Slice";
      CS : Chunked_String;
      Low : constant Positive := 11;
      High : constant Positive := 17;
   begin
      Chunked_Slice (To_Chunked_String (Name), CS, Low, High);

      if To_String (CS) /= Name (Low .. High) then
         NT.Item (Report, Name, NT.Fail);
         NT.Info (Report, "Final value: """ & To_String (CS) & '"');
         NT.Info (Report, "Expected: """ & Name (Low .. High) & '"');
      else
         NT.Item (Report, Name, NT.Success);
      end if;
   exception
      when Error : others => NT.Report_Exception (Report, Name, Error);
   end;

   declare
      Name : constant String := "Procedure Chunked_Slice (empty)";
      CS : Chunked_String;
   begin
      Chunked_Slice (To_Chunked_String (Name), CS, 1, 0);

      if To_String (CS) /= "" then
         NT.Item (Report, Name, NT.Fail);
         NT.Info (Report, "Final value: """ & To_String (CS) & '"');
      else
         NT.Item (Report, Name, NT.Success);
      end if;
   exception
      when Error : others => NT.Report_Exception (Report, Name, Error);
   end;

   declare
      Name : constant String := "Function Index_Non_Blank with From";
      CS : constant Chunked_String := To_Chunked_String (Name);
      M, N : Natural;
   begin
      M := Index (CS, " ");
      N := Index_Non_Blank (CS, M);

      if N /= M + 1 then
         NT.Item (Report, Name, NT.Fail);
         NT.Info (Report, "Final value:" & Natural'Image (N));
         NT.Info (Report, "Expected:" & Natural'Image (M + 1));
      else
         NT.Item (Report, Name, NT.Success);
      end if;
   exception
      when Error : others => NT.Report_Exception (Report, Name, Error);
   end;

   declare
      Name : constant String := "Function Find_Token at string end";
      CS : constant Chunked_String := To_Chunked_String ("--end");
      First : Positive;
      Last : Natural;
   begin
      Find_Token
        (Source => CS,
         Set => Maps.To_Set ("abcdefghijklmnopqrst"),
         Test => Ada.Strings.Inside,
         First => First,
         Last => Last);

      if First /= 3 or Last /= 5 then
         NT.Item (Report, Name, NT.Fail);
         NT.Info (Report,
            "Final interval:" & Natural'Image (First)
            & " .." & Natural'Image (Last));
         NT.Info (Report, "Expected: 3 .. 5");
      else
         NT.Item (Report, Name, NT.Success);
      end if;
   exception
      when Error : others => NT.Report_Exception (Report, Name, Error);
   end;

   Natools.Tests.End_Section (Report);
end Natools.Chunked_Strings.Tests.Coverage;
