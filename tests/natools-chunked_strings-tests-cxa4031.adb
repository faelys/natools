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

procedure Natools.Chunked_Strings.Tests.CXA4031
  (Report : in out Natools.Tests.Reporter'Class)
is
   package NT renames Natools.Tests;
begin
   NT.Section (Report, "Port of ACATS CXA4031");

   declare
      subtype LC_Characters is Character range 'a' .. 'z';

      Null_String           : constant String := "";
      TC_String             : constant String := "A Standard String";

      TC_Chunked_String,
      TC_New_Chunked_String : Chunked_String := Null_Chunked_String;
   begin
      NT.Section (Report, "Function To_Chunked_String with Length parameter");

      declare
         Name : constant String := "Length = 10";
         Result : Natural;
      begin
         Result := Length (To_Chunked_String (Length => 10));
         if Result = 10 then
            NT.Item (Report, Name, NT.Success);
         else
            NT.Item (Report, Name, NT.Fail);
            NT.Info (Report, "Found length" & Natural'Image (Result));
         end if;
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;


      declare
         Name : constant String := "Singleton";
         Result : Natural;
      begin
         Result := Length (To_Chunked_String (1));
         if Result = 1 then
            NT.Item (Report, Name, NT.Success);
         else
            NT.Item (Report, Name, NT.Fail);
            NT.Info (Report, "Found length" & Natural'Image (Result));
         end if;
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;


      declare
         Name : constant String := "Empty string";
         Result : Natural;
      begin
         Result := Length (To_Chunked_String (0));
         if Result = 0 then
            NT.Item (Report, Name, NT.Success);
         else
            NT.Item (Report, Name, NT.Fail);
            NT.Info (Report, "Found length" & Natural'Image (Result));
         end if;
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;


      declare
         Name : constant String := "Concatenation of the above";
         Result : Natural;
      begin
         Result := Length (To_Chunked_String (Length => 10)
                           & To_Chunked_String (1)
                           & To_Chunked_String (0));
         if Result = 10 + 1 + 0 then
            NT.Item (Report, Name, NT.Success);
         else
            NT.Item (Report, Name, NT.Fail);
            NT.Info (Report, "Found length" & Natural'Image (Result));
         end if;
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      NT.End_Section (Report);

      NT.Section (Report, "Procedure Append (Chunked, Chunked)");

      declare
         Name : constant String := "Non-empty and non-empty";
      begin
         TC_Chunked_String := To_Chunked_String ("Sample string of length L");
         TC_New_Chunked_String := To_Chunked_String (" and then some");
         Append (TC_Chunked_String, TC_New_Chunked_String);
         Test (Report, Name, TC_Chunked_String,
               "Sample string of length L and then some");
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      declare
         Name : constant String := "Non-empty and empty";
      begin
         TC_Chunked_String := To_Chunked_String ("Sample string of length L");
         TC_New_Chunked_String := Null_Chunked_String;
         Test (Report, Name, TC_Chunked_String, "Sample string of length L");
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      declare
         Name : constant String := "Empty and non-empty";
      begin
         TC_Chunked_String := Null_Chunked_String;
         Append (TC_Chunked_String,
                 To_Chunked_String ("New Chunked String"));
         Test (Report, Name, TC_Chunked_String, "New Chunked String");
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      NT.End_Section (Report);


      NT.Section (Report, "Procedure Append (Chunked, String)");

      declare
         Name : constant String := "Non-empty and non-empty";
      begin
         TC_Chunked_String := To_Chunked_String ("A Chunked String and ");
         Append (Source => TC_Chunked_String, New_Item => TC_String);
         Test (Report, Name, TC_Chunked_String,
               "A Chunked String and A Standard String");
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      declare
         Name : constant String := "Non-empty and empty";
      begin
         TC_Chunked_String := To_Chunked_String ("A Chunked String");
         Append (TC_Chunked_String, New_Item => Null_String);
         Test (Report, Name, TC_Chunked_String, "A Chunked String");
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      declare
         Name : constant String := "Empty and non-empty";
      begin
         TC_Chunked_String := Null_Chunked_String;
         Append (TC_Chunked_String, TC_String);
         Test (Report, Name, TC_Chunked_String, "A Standard String");
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      NT.End_Section (Report);


      NT.Section (Report, "Procedure Append (Chunked, Character)");

      declare
         Name : constant String := "Non-empty initial string";
      begin
         TC_Chunked_String := To_Chunked_String ("Lower Case = ");
         for I in LC_Characters'Range loop
            Append (Source   => TC_Chunked_String,
                    New_Item => I);
         end loop;
         Test (Report, Name, TC_Chunked_String,
               "Lower Case = abcdefghijklmnopqrstuvwxyz");
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      declare
         Name : constant String := "Empty initial string";
      begin
         TC_Chunked_String := Null_Chunked_String;
         Append (TC_Chunked_String, New_Item => 'a');
         Test (Report, Name, TC_Chunked_String, "a");
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      NT.End_Section (Report);


      NT.Section (Report, "Function ""=""");
      TC_Chunked_String := To_Chunked_String (TC_String);

      declare
         Name : constant String := "Chunked_String and String";
      begin
         NT.Item (Report, Name, NT.To_Result (TC_Chunked_String = TC_String));
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      declare
         Name : constant String := "String and Chunked_String";
      begin
         NT.Item (Report, Name,
                  NT.To_Result ("A Standard String" = TC_Chunked_String));
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      declare
         Name : constant String := "Empty chunked string and empty string";
      begin
         NT.Item (Report, Name, NT.To_Result (Null_Chunked_String = ""));
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      declare
         Name : constant String := "With inline conversion";
      begin
         NT.Item (Report, Name,
           NT.To_Result ("Test String" = To_Chunked_String ("Test String")));
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      NT.End_Section (Report);


      NT.Section (Report, "Function ""<""");

      declare
         Name : constant String := "Differing by a trailing space";
      begin
         NT.Item (Report, Name,
           NT.To_Result ("Extra Space" < To_Chunked_String ("Extra Space ")));
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      declare
         Name : constant String := "Differing by the last letter";
      begin
         NT.Item (Report, Name,
                  NT.To_Result (To_Chunked_String ("tess") < "test"));
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      declare
         Name : constant String := "Differing by the first letter";
      begin
         NT.Item (Report, Name,
                  NT.To_Result (To_Chunked_String ("best") < "test"));
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      declare
         Name : constant String := "Empty strings";
      begin
         NT.Item (Report, Name,
                  NT.To_Result (not (Null_Chunked_String < Null_String)));
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      declare
         Name : constant String := "Equal with leading blank";
      begin
         NT.Item (Report, Name,
           NT.To_Result (not (" leading blank"
                               < To_Chunked_String (" leading blank"))));
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      declare
         Name : constant String := "Equal with ending blank";
      begin
         NT.Item (Report, Name,
           NT.To_Result (not ("ending blank "
                              < To_Chunked_String ("ending blank "))));
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      NT.End_Section (Report);


      NT.Section (Report, "Function ""<=""");
      TC_Chunked_String := To_Chunked_String ("Sample string");

      declare
         Name : constant String := "Prefix";
      begin
         NT.Item (Report, Name,
           NT.To_Result (not (TC_Chunked_String <= "Sample strin")));
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      declare
         Name : constant String := "Differing by case";
      begin
         NT.Item (Report, Name,
           NT.To_Result (not ("sample string" <= TC_Chunked_String)));
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      declare
         Name : constant String := "Empty strings";
      begin
         NT.Item (Report, Name, NT.To_Result (Null_Chunked_String <= ""));
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      declare
         Name : constant String := "Equal strings";
      begin
         NT.Item (Report, Name,
                  NT.To_Result ("Sample string" <= TC_Chunked_String));
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      NT.End_Section (Report);


      NT.Section (Report, "Function "">""");
      TC_Chunked_String := To_Chunked_String ("A MUCH LONGER STRING");

      declare
         Name : constant String := "Differing by case";
      begin
         NT.Item (Report, Name,
                  NT.To_Result ("A much longer string" > TC_Chunked_String));
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      declare
         Name : constant String := "Prefix";
      begin
         NT.Item (Report, Name,
           NT.To_Result (To_Chunked_String (TC_String) > "A Standard Strin"));
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      declare
         Name : constant String := "Differing by case";
      begin
         NT.Item (Report, Name,
           NT.To_Result ("abcdefgh" > To_Chunked_String ("ABCDEFGH")));
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      declare
         Name : constant String := "Empty strings";
      begin
         NT.Item (Report, Name,
           NT.To_Result (not (Null_Chunked_String > Null_String)));
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      NT.End_Section (Report);


      NT.Section (Report, "Function "">=""");
      TC_Chunked_String := To_Chunked_String (TC_String);

      declare
         Name : constant String := "Equal strings";
      begin
         NT.Item (Report, Name, NT.To_Result (TC_Chunked_String >= TC_String));
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      declare
         Name : constant String := "Empty strings";
      begin
         NT.Item (Report, Name,
                  NT.To_Result (Null_String >= Null_Chunked_String));
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      declare
         Name : constant String := "Differing by the last letter";
      begin
         NT.Item (Report, Name,
                  NT.To_Result ("test" >= To_Chunked_String ("tess")));
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      declare
         Name : constant String := "Differing by case";
      begin
         NT.Item (Report, Name,
           NT.To_Result (To_Chunked_String ("Programming") >= "PROGRAMMING"));
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      NT.End_Section (Report);
   exception
      when Error : others =>
         NT.Item (Report, "Preparation", NT.Error);
         NT.Info (Report, "Exception: "
                          & Ada.Exceptions.Exception_Name (Error));
         NT.Info (Report, Ada.Exceptions.Exception_Message (Error));
   end;

   NT.End_Section (Report);

end Natools.Chunked_Strings.Tests.CXA4031;
