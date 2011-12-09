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

with Ada.Exceptions;
with Ada.Strings.Maps.Constants;

procedure Natools.Chunked_Strings.Tests.CXA4032
  (Report : in out Natools.Tests.Reporter'Class)
is
   package NT renames Natools.Tests;
begin
   NT.Section (Report, "Port of ACATS CXA4032");

   declare
      TC_Null_String    : constant String := "";
      TC_String_5       : constant String (1 .. 5) := "ABCDE";
      TC_Chunked_String : Chunked_String := To_Chunked_String ("Test String");
   begin
      NT.Section (Report, "Procedure Replace_Slice");
      declare
         Name : constant String
           := "Index_Error raised when Low > Source'Last+1";
      begin
         Replace_Slice (Source => TC_Chunked_String,
                        Low    => Length (TC_Chunked_String) + 2,
                        High   => Length (TC_Chunked_String),
                        By     => TC_String_5);
         NT.Item (Report, Name, NT.Fail);
         NT.Info (Report, "No exception has been raised.");
         NT.Info (Report,
                  "Final value: """ & To_String (TC_Chunked_String) & '"');
      exception
         when Ada.Strings.Index_Error =>
            NT.Item (Report, Name, NT.Success);
         when Error : others =>
            NT.Item (Report, Name, NT.Fail);
            NT.Info (Report, "Wrong exception "
                             & Ada.Exceptions.Exception_Name (Error)
                             & " raised instead");
      end;


      declare
         Name : constant String := "1-character slice replacement";
      begin
         TC_Chunked_String := To_Chunked_String ("Test String");
         Replace_Slice (TC_Chunked_String, 5, 5, TC_String_5);
         Test (Report, Name, TC_Chunked_String, "TestABCDEString");
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;


      declare
         Name : constant String := "Prefix replacement";
      begin
         Replace_Slice (TC_Chunked_String, 1, 4, TC_String_5);
         Test (Report, Name, TC_Chunked_String, "ABCDEABCDEString");
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;


      declare
         Name : constant String := "Suffix replacement by empty";
      begin
         Replace_Slice (TC_Chunked_String,
                        11,
                        Length (TC_Chunked_String),
                        TC_Null_String);
         Test (Report, Name, TC_Chunked_String, "ABCDEABCDE");
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;


      declare
         Name : constant String := "Slice insertion in the middle";
      begin
         Replace_Slice (TC_Chunked_String, Low => 4, High => 1, By => "xxx");
         Test (Report, Name, TC_Chunked_String, "ABCxxxDEABCDE");
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;


      declare
         Name : constant String := "Slice insertion at the beginning";
      begin
         Replace_Slice (TC_Chunked_String, Low => 1, High => 0, By => "yyy");
         Test (Report, Name, TC_Chunked_String, "yyyABCxxxDEABCDE");
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;


      declare
         Name : constant String := "Slice insertion at the end";
      begin
         Replace_Slice (TC_Chunked_String,
                        Length (TC_Chunked_String) + 1,
                        Length (TC_Chunked_String),
                        By => "zzz");
         Test (Report, Name, TC_Chunked_String, "yyyABCxxxDEABCDEzzz");
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      NT.End_Section (Report);

      NT.Section (Report, "Procedure Insert");
      TC_Chunked_String := To_Chunked_String ("Test String");

      declare
         Name : constant String := "Index_Error raised on incorrect Before";
      begin
         Insert (Source   => TC_Chunked_String,
                 Before   => Length (TC_Chunked_String) + 2,
                 New_Item => TC_String_5);
         NT.Item (Report, Name, NT.Fail);
         NT.Info (Report, "No exception has been raised.");
         NT.Info (Report,
                  "Final value: """ & To_String (TC_Chunked_String) & '"');
      exception
         when Ada.Strings.Index_Error =>
            NT.Item (Report, Name, NT.Success);
         when Error : others =>
            NT.Item (Report, Name, NT.Fail);
            NT.Info (Report, "Wrong exception "
                             & Ada.Exceptions.Exception_Name (Error)
                             & " raised instead");
      end;


      declare
         Name : constant String := "Prefix insertion";
      begin
         Insert (TC_Chunked_String, 1, "**");
         Test (Report, Name, TC_Chunked_String, "**Test String");
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;


      declare
         Name : constant String := "Suffix insertion";
      begin
         Insert (TC_Chunked_String, Length (TC_Chunked_String) + 1, "**");
         Test (Report, Name, TC_Chunked_String, "**Test String**");
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;


      declare
         Name : constant String := "Insertion in the middle";
      begin
         Insert (TC_Chunked_String, 8, "---");
         Test (Report, Name, TC_Chunked_String, "**Test ---String**");
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;


      declare
         Name : constant String := "Empty insertion";
      begin
         Insert (TC_Chunked_String, 3, TC_Null_String);
         Test (Report, Name, TC_Chunked_String, "**Test ---String**");
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      NT.End_Section (Report);

      NT.Section (Report, "Procedure Overwrite");

      declare
         Name : constant String := "Index_Error raised on incorrect Position";
      begin
         Overwrite (Source   => TC_Chunked_String,
                    Position => Length (TC_Chunked_String) + 2,
                    New_Item => TC_String_5);
         NT.Item (Report, Name, NT.Fail);
         NT.Info (Report, "No exception has been raised.");
         NT.Info (Report,
                  "Final value: """ & To_String (TC_Chunked_String) & '"');
      exception
         when Ada.Strings.Index_Error =>
            NT.Item (Report, Name, NT.Success);
         when Error : others =>
            NT.Item (Report, Name, NT.Fail);
            NT.Info (Report, "Wrong exception "
                             & Ada.Exceptions.Exception_Name (Error)
                             & " raised instead");
      end;


      declare
         Name : constant String := "Normal overwrite";
      begin
         TC_Chunked_String := To_Chunked_String ("Test String");
         Overwrite (Source   => TC_Chunked_String,
                    Position => 1,
                    New_Item => "XXXX");
         Test (Report, Name, TC_Chunked_String, "XXXX String");
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;


      declare
         Name : constant String := "Overwrite after the end";
      begin
         Overwrite (TC_Chunked_String, Length (TC_Chunked_String) + 1, "**");
         Test (Report, Name, TC_Chunked_String, "XXXX String**");
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;


      declare
         Name : constant String := "Empty overwrite";
      begin
         Overwrite (TC_Chunked_String, 3, TC_Null_String);
         Test (Report, Name, TC_Chunked_String, "XXXX String**");
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;


      declare
         Name : constant String := "Complete overwrite";
      begin
         Overwrite (TC_Chunked_String, 1, "abcdefghijklmn");
         Test (Report, Name, TC_Chunked_String, "abcdefghijklmn");
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      NT.End_Section (Report);

      NT.Section (Report, "Procedure Delete");


      declare
         Name : constant String := "Empty deletion at the end";
      begin
         TC_Chunked_String := To_Chunked_String ("Test String");
         Delete (Source  => TC_Chunked_String,
                 From    => Length (TC_Chunked_String),
                 Through => Length (TC_Chunked_String) - 1);
         Test (Report, Name, TC_Chunked_String, "Test String");
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;


      declare
         Name : constant String := "Empty deletion at the beginning";
      begin
         Delete (TC_Chunked_String, 1, 0);
         Test (Report, Name, TC_Chunked_String, "Test String");
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;


      declare
         Name : constant String := "Prefix deletion";
      begin
         Delete (TC_Chunked_String, 1, 5);
         Test (Report, Name, TC_Chunked_String, "String");
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;


      declare
         Name : constant String := "1-character range deletion";
      begin
         Delete (TC_Chunked_String, 3, 3);
         Test (Report, Name, TC_Chunked_String, "Sting");
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      NT.End_Section (Report);

      NT.Section (Report, "Procedure Trim");


      declare
         Name : constant String := "Nothing to trim";
      begin
         TC_Chunked_String := To_Chunked_String ("No Spaces");
         Trim (Source => TC_Chunked_String, Side => Ada.Strings.Both);
         Test (Report, Name, TC_Chunked_String, "No Spaces");
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;


      declare
         Name : constant String := "Trim left but not right";
      begin
         TC_Chunked_String := To_Chunked_String ("   Leading Spaces   ");
         Trim (TC_Chunked_String, Ada.Strings.Left);
         Test (Report, Name, TC_Chunked_String, "Leading Spaces   ");
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;


      declare
         Name : constant String := "Trim right but not left";
      begin
         TC_Chunked_String := To_Chunked_String ("   Ending Spaces   ");
         Trim (TC_Chunked_String, Ada.Strings.Right);
         Test (Report, Name, TC_Chunked_String, "   Ending Spaces");
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;


      declare
         Name : constant String := "Trim on both sides";
      begin
         TC_Chunked_String
           := To_Chunked_String ("    Spaces   on  both  ends     ");
         Trim (TC_Chunked_String, Ada.Strings.Both);
         Test (Report, Name, TC_Chunked_String, "Spaces   on  both  ends");
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      NT.End_Section (Report);

      NT.Section (Report, "Procedure Trim (with Character Set parameter)");

      declare
         Name : constant String := "Normal trim";
      begin
         TC_Chunked_String := To_Chunked_String ("lowerCASEletters");
         Trim (Source => TC_Chunked_String,
               Left   => Ada.Strings.Maps.Constants.Lower_Set,
               Right  => Ada.Strings.Maps.Constants.Lower_Set);
         Test (Report, Name, TC_Chunked_String, "CASE");
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;


      declare
         Name : constant String := "Nothing to trim";
      begin
         TC_Chunked_String := To_Chunked_String ("lowerCASEletters");
         Trim (TC_Chunked_String,
               Ada.Strings.Maps.Constants.Upper_Set,
               Ada.Strings.Maps.Constants.Upper_Set);
         Test (Report, Name, TC_Chunked_String, "lowerCASEletters");
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;


      declare
         Name : constant String := "Normal trim";
      begin
         TC_Chunked_String := To_Chunked_String ("012abcdefghGFEDCBA789ab");
         Trim (TC_Chunked_String,
               Ada.Strings.Maps.Constants.Hexadecimal_Digit_Set,
               Ada.Strings.Maps.Constants.Hexadecimal_Digit_Set);
         Test (Report, Name, TC_Chunked_String, "ghG");
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      NT.End_Section (Report);

      NT.Section (Report, "Procedure Head");

      declare
         Name : constant String := "Empty head";
      begin
         TC_Chunked_String := To_Chunked_String ("Test String");
         Head (Source => TC_Chunked_String,
               Count  => 0,
               Pad    => '*');
         Test (Report, Name, TC_Chunked_String, Null_Chunked_String);
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;


      declare
         Name : constant String := "Normal Head";
      begin
         TC_Chunked_String := To_Chunked_String ("Test String");
         Head (Source => TC_Chunked_String,
               Count  => 4,
               Pad    => '*');
         Test (Report, Name, TC_Chunked_String, "Test");
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;


      declare
         Name : constant String := "No-op Head";
      begin
         TC_Chunked_String := To_Chunked_String ("Test String");
         Head (Source => TC_Chunked_String,
               Count  => Length (TC_Chunked_String),
               Pad    => '*');
         Test (Report, Name, TC_Chunked_String, "Test String");
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;


      declare
         Name : constant String := "Head with padding";
      begin
         TC_Chunked_String := To_Chunked_String ("Test String");
         Head (Source => TC_Chunked_String,
               Count  => Length (TC_Chunked_String) + 4,
               Pad    => '*');
         Test (Report, Name, TC_Chunked_String, "Test String****");
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;


      declare
         Name : constant String := "Empty string with padding";
      begin
         TC_Chunked_String := Null_Chunked_String;
         Head (Source => TC_Chunked_String,
               Count  => Length (TC_Chunked_String) + 3,
               Pad    => '*');
         Test (Report, Name, TC_Chunked_String, "***");
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      NT.End_Section (Report);

      NT.Section (Report, "Procedure Tail");

      declare
         Name : constant String := "Empty tail";
      begin
         TC_Chunked_String := To_Chunked_String ("Test String");
         Tail (Source => TC_Chunked_String,
               Count  => 0,
               Pad    => '*');
         Test (Report, Name, TC_Chunked_String, Null_Chunked_String);
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;


      declare
         Name : constant String := "Normal tail";
      begin
         TC_Chunked_String := To_Chunked_String ("Test String");
         Tail (Source => TC_Chunked_String,
               Count  => 6,
               Pad    => '*');
         Test (Report, Name, TC_Chunked_String, "String");
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;


      declare
         Name : constant String := "No-op tail";
      begin
         TC_Chunked_String := To_Chunked_String ("Test String");
         Tail (Source => TC_Chunked_String,
               Count  => Length (TC_Chunked_String),
               Pad    => '*');
         Test (Report, Name, TC_Chunked_String, "Test String");
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;


      declare
         Name : constant String := "Tail with padding";
      begin
         TC_Chunked_String := To_Chunked_String ("Test String");
         Tail (Source => TC_Chunked_String,
               Count  => Length (TC_Chunked_String) + 5,
               Pad    => 'x');
         Test (Report, Name, TC_Chunked_String, "xxxxxTest String");
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;


      declare
         Name : constant String := "Empty string with padding";
      begin
         TC_Chunked_String := Null_Chunked_String;
         Tail (Source => TC_Chunked_String,
               Count  => Length (TC_Chunked_String) + 3,
               Pad    => 'X');
         Test (Report, Name, TC_Chunked_String, "XXX");
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      NT.End_Section (Report);
   exception
      when Error : others =>
         NT.Report_Exception (Report, "Preparation", Error);
   end;

   NT.End_Section (Report);

end Natools.Chunked_Strings.Tests.CXA4032;
