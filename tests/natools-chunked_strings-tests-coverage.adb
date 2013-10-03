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
with Ada.Strings.Maps;

procedure Natools.Chunked_Strings.Tests.Coverage
  (Report : in out Natools.Tests.Reporter'Class)
is
   package NT renames Natools.Tests;

   procedure Report_Result
     (Name : in String;
      Reported : in out Boolean;
      Result : in NT.Result := NT.Fail);
      --  Report Result unless already reported

   procedure Report_Result
     (Name : in String;
      Reported : in out Boolean;
      Result : in NT.Result := NT.Fail) is
   begin
      if not Reported then
         NT.Item (Report, Name, Result);
         Reported := True;
      end if;
   end Report_Result;
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
      Name : constant String := "Index_Error raised in function Slice";
      CS   : constant Chunked_String := To_Chunked_String (Name);
      Str  : String (1 .. 10);
   begin
      Str := Slice (CS, Name'Length, Name'Length + Str'Length - 1);
      NT.Item (Report, Name, NT.Fail);
      NT.Info (Report, "No exception has been raised.");
      NT.Info (Report, "Final value: """ & Str & '"');
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
      Name : constant String := "Index_Error raised in function Chunked_Slice";
      CS   : constant Chunked_String := To_Chunked_String (Name);
      Dest : Chunked_String;
   begin
      Dest := Chunked_Slice (CS, Name'Length, Name'Length + 10);
      NT.Item (Report, Name, NT.Fail);
      NT.Info (Report, "No exception has been raised.");
      NT.Info (Report, "Final value: """ & To_String (Dest) & '"');
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
      Name : constant String
        := "Index_Error raised in procedure Chunked_Slice";
      CS   : constant Chunked_String := To_Chunked_String (Name);
      Dest : Chunked_String;
   begin
      Chunked_Slice (CS, Dest, Name'Length + 10, Name'Length + 20);
      NT.Item (Report, Name, NT.Fail);
      NT.Info (Report, "No exception has been raised.");
      NT.Info (Report, "Final value: """ & To_String (Dest) & '"');
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
      Name : constant String
        := "Index_Error raised in function Index (pattern)";
      CS   : constant Chunked_String := To_Chunked_String (Name);
      N : Natural;
   begin
      N := Index (CS, ".", Name'Length + 1);
      NT.Item (Report, Name, NT.Fail);
      NT.Info (Report, "No exception has been raised.");
      NT.Info (Report, "Final value:" & Integer'Image (N));
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
      Name : constant String := "Index_Error raised in function Index (set)";
      CS   : constant Chunked_String := To_Chunked_String (Name);
      N : Natural;
   begin
      N := Index (CS, Maps.To_Set ("."), Name'Length + 1);
      NT.Item (Report, Name, NT.Fail);
      NT.Info (Report, "No exception has been raised.");
      NT.Info (Report, "Final value:" & Integer'Image (N));
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
      Name : constant String := "Index_Error raised in function Replace_Slice";
      CS   : constant Chunked_String := To_Chunked_String (Name);
      Dest : Chunked_String;
   begin
      Dest := Replace_Slice (CS, Name'Length + 10, 0, "Hello");
      NT.Item (Report, Name, NT.Fail);
      NT.Info (Report, "No exception has been raised.");
      NT.Info (Report, "Final value: """ & To_String (Dest) & '"');
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

   declare
      Name : constant String := "Comparisons of Chunked_Strings";
      CS_Name : constant Chunked_String := To_Chunked_String (Name);
      Prefix : constant Chunked_String := To_Chunked_String ("Comparisons");
      Smaller : constant Chunked_String := To_Chunked_String ("Ca");
      Reported : Boolean := False;
   begin
      if CS_Name <= Null_Chunked_String then
         Report_Result (Name, Reported);
         NT.Info (Report, "CS_Name <= Null_Chunked_String");
      end if;

      if Null_Chunked_String >= CS_Name then
         Report_Result (Name, Reported);
         NT.Info (Report, "Null_Chunked_String >= CS_Name");
      end if;

      if Prefix >= CS_Name then
         Report_Result (Name, Reported);
         NT.Info (Report, "Prefix >= CS_Name");
      end if;

      if CS_Name <= Prefix then
         Report_Result (Name, Reported);
         NT.Info (Report, "CS_Name <= Prefix");
      end if;

      if Smaller >= CS_Name then
         Report_Result (Name, Reported);
         NT.Info (Report, "Smaller >= CS_Name");
      end if;

      if CS_Name <= Smaller then
         Report_Result (Name, Reported);
         NT.Info (Report, "CS_Name <= Smaller");
      end if;

      Report_Result (Name, Reported, NT.Success);
   exception
      when Error : others => NT.Report_Exception (Report, Name, Error);
   end;

   declare
      Name : constant String := "Comparisons of Chunked_Strings with Strings";
      CS_Name : constant Chunked_String := To_Chunked_String (Name);
      Double_Name : constant String := Name & Name;
      Reported : Boolean := False;
   begin
      if CS_Name >= Double_Name then
         Report_Result (Name, Reported);
         NT.Info (Report, "CS_Name >= Double_Name");
      end if;

      if Null_Chunked_String >= Name then
         Report_Result (Name, Reported);
         NT.Info (Report, "Null_Chunked_String >= Name");
      end if;

      if CS_Name <= "" then
         Report_Result (Name, Reported);
         NT.Info (Report, "CS_Name <= """"");
      end if;

      Report_Result (Name, Reported, NT.Success);
   exception
      when Error : others => NT.Report_Exception (Report, Name, Error);
   end;

   declare
      Name : constant String := "Procedure Index, backwards without match";
      CS : constant Chunked_String := To_Chunked_String (Name);
      N : Natural;
   begin
      N := Index
        (Source => CS,
         Set => Ada.Strings.Maps.To_Set ("."),
         Test => Ada.Strings.Inside,
         Going => Ada.Strings.Backward);

      if N /= 0 then
         NT.Item (Report, Name, NT.Fail);
         NT.Info (Report, "Unexpected match at" & Natural'Image (N));
      else
         NT.Item (Report, Name, NT.Success);
      end if;
   exception
      when Error : others => NT.Report_Exception (Report, Name, Error);
   end;

   declare
      Name : constant String := "Function Trim, one-sided";
      Str : constant String := "   word  ";
      Reported : Boolean := False;
   begin
      declare
         CS : constant Chunked_String := To_Chunked_String (Str);
         Left : constant String
           := Ada.Strings.Fixed.Trim (Str, Ada.Strings.Left);
         Right : constant String
           := Ada.Strings.Fixed.Trim (Str, Ada.Strings.Right);
         CS_Left : constant Chunked_String := Trim (CS, Ada.Strings.Left);
         CS_Right : constant Chunked_String := Trim (CS, Ada.Strings.Right);
      begin
         if To_String (CS_Left) /= Left then
            Report_Result (Name, Reported);
            NT.Info (Report, "Found """ & To_String (CS_Left) & '"');
            NT.Info (Report, "Expected """ & Left & '"');
         end if;

         if To_String (CS_Right) /= Right then
            Report_Result (Name, Reported);
            NT.Info (Report, "Found """ & To_String (CS_Right) & '"');
            NT.Info (Report, "Expected """ & Right & '"');
         end if;
      end;

      Report_Result (Name, Reported, NT.Success);
   exception
      when Error : others => NT.Report_Exception (Report, Name, Error);
   end;

   Natools.Tests.End_Section (Report);
end Natools.Chunked_Strings.Tests.Coverage;
