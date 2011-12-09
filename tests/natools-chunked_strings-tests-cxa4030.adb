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

with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Exceptions;
with Ada.Strings;        use Ada.Strings;

procedure Natools.Chunked_Strings.Tests.CXA4030
  (Report : in out Natools.Tests.Reporter'Class)
is
   package NT renames Natools.Tests;
begin
   NT.Section (Report, "Port of ACATS CXA4030");

   declare
      package L1 renames Ada.Characters.Latin_1;

      New_Character_String : Chunked_String
        := To_Chunked_String (L1.LC_A_Grave          & L1.LC_A_Ring
                            & L1.LC_AE_Diphthong     & L1.LC_C_Cedilla
                            & L1.LC_E_Acute          & L1.LC_I_Circumflex
                            & L1.LC_Icelandic_Eth    & L1.LC_N_Tilde
                            & L1.LC_O_Oblique_Stroke & L1.LC_Icelandic_Thorn);

      TC_New_Character_String : constant Chunked_String
        := To_Chunked_String (L1.UC_A_Grave          & L1.UC_A_Ring
                            & L1.UC_AE_Diphthong     & L1.UC_C_Cedilla
                            & L1.UC_E_Acute          & L1.UC_I_Circumflex
                            & L1.UC_Icelandic_Eth    & L1.UC_N_Tilde
                            & L1.UC_O_Oblique_Stroke & L1.UC_Icelandic_Thorn);
      Map_To_Lower_Case_Ptr : constant Maps.Character_Mapping_Function
        := Ada.Characters.Handling.To_Lower'Access;
      Map_To_Upper_Case_Ptr : constant Maps.Character_Mapping_Function
        := Ada.Characters.Handling.To_Upper'Access;
   begin
      NT.Section (Report, "Function Index, Forward direction");
      declare
         Name : constant String := "Mixed case mapped to lower";
      begin
         Test (Report, Name,
               Index (Source  => To_Chunked_String
                                   ("The library package Strings.Unbounded"),
                      Pattern => "unb",
                      Going   => Ada.Strings.Forward,
                      Mapping => Map_To_Lower_Case_Ptr),
               29);
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      declare
         Name : constant String := "Upper case mapped to lower";
      begin
         Test (Report, Name,
               Index (To_Chunked_String
                        ("THE RAIN IN SPAIN FALLS MAINLY ON THE PLAIN"),
                      "ain",
                      Mapping => Map_To_Lower_Case_Ptr),
               6);
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      declare
         Name : constant String := "Lower case mapped to lower";
      begin
         Test (Report, Name,
               Index (To_Chunked_String ("maximum number"),
                      "um",
                      Ada.Strings.Forward,
                      Ada.Characters.Handling.To_Lower'Access),
               6);
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      declare
         Name : constant String := "Mixed case mapped to upper";
      begin
         Test (Report, Name,
               Index (To_Chunked_String ("CoMpLeTeLy MiXeD CaSe StRiNg"),
                      "MIXED CASE STRING",
                      Ada.Strings.Forward,
                      Map_To_Upper_Case_Ptr),
               12);
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      declare
         Name : constant String := "Upper case mapped to lower (no match)";
      begin
         Test (Report, Name,
               Index (To_Chunked_String
                        ("STRING WITH NO MATCHING PATTERNS"),
                      "WITH",
                      Mapping => Map_To_Lower_Case_Ptr),
               0);
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      declare
         Name : constant String := "Upper case mapped to upper";
      begin
         Test (Report, Name,
               Index (To_Chunked_String ("THIS STRING IS IN UPPER CASE"),
                      "IS",
                      Ada.Strings.Forward,
                      Ada.Characters.Handling.To_Upper'Access),
               3);
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      declare
         Name : constant String := "Null string";
      begin
         Test (Report, Name,
               Index (Null_Chunked_String,
                      "is",
                      Mapping => Map_To_Lower_Case_Ptr),
               0);
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      declare
         Name : constant String := "Upper case mapped to lower";
      begin
         Test (Report, Name,
               Index (To_Chunked_String ("AAABBBaaabbb"),
                      "aabb",
                      Mapping => Ada.Characters.Handling.To_Lower'Access),
               2);
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      NT.End_Section (Report);

      NT.Section (Report, "Function Index, Backward direction");

      declare
         Name : constant String := "Mixed case mapped to lower";
      begin
         Test (Report, Name,
               Index (To_Chunked_String ("Case of a Mixed Case String"),
                      "case",
                      Ada.Strings.Backward,
                      Map_To_Lower_Case_Ptr),
               17);
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      declare
         Name : constant String := "Mixed case mapped to upper";
      begin
         Test (Report, Name,
               Index (To_Chunked_String ("Case of a Mixed Case String"),
                      "CASE",
                      Ada.Strings.Backward,
                      Mapping => Map_To_Upper_Case_Ptr),
               17);
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      declare
         Name : constant String := "Upper case mapped to lower";
      begin
         Test (Report, Name,
               Index (To_Chunked_String ("rain, Rain, and more RAIN"),
                      "rain",
                      Ada.Strings.Backward,
                      Ada.Characters.Handling.To_Lower'Access),
               22);
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      declare
         Name : constant String := "Lower case mapped to upper";
      begin
         Test (Report, Name,
               Index (To_Chunked_String ("RIGHT place, right time"),
                      "RIGHT",
                      Going   => Ada.Strings.Backward,
                      Mapping => Ada.Characters.Handling.To_Upper'Access),
               14);
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      declare
         Name : constant String := "Upper case mapped to lower (no match)";
      begin
         Test (Report, Name,
               Index (To_Chunked_String ("WOULD MATCH BUT FOR THE CASE"),
                      "WOULD MATCH BUT FOR THE CASE",
                      Going   => Ada.Strings.Backward,
                      Mapping => Map_To_Lower_Case_Ptr),
               0);
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      NT.End_Section (Report);


      declare
         Null_String : constant String := "";
         TC_Natural  : Natural := 1000;
      begin
         TC_Natural
           := Index (To_Chunked_String ("A Valid Chunked String"),
                     Null_String,
                     Going   => Ada.Strings.Forward,
                     Mapping => Ada.Characters.Handling.To_Lower'Access);
         NT.Item (Report, "Pattern_Error raised in Index", NT.Fail);
         NT.Info (Report, "No exception has been raised.");
         NT.Info (Report, "Return value: " & Natural'Image (TC_Natural));
      exception
         when Pattern_Error =>
            NT.Item (Report, "Pattern_Error raised in Index", NT.Success);
         when Error : others =>
            NT.Item (Report, "Pattern_Error raised in Index", NT.Fail);
            NT.Info (Report, "Wrong exception "
                             & Ada.Exceptions.Exception_Name (Error)
                             & "has been raised.");
      end;


      NT.Section (Report, "Function Count with mapping function");

      declare
         Name : constant String := "Upper case mapped to lower";
      begin
         Test (Report, Name,
               Count (Source  => To_Chunked_String ("ABABABA"),
                      Pattern => "aba",
                      Mapping => Map_To_Lower_Case_Ptr),
               2);
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      declare
         Name : constant String := "Upper case mapped to lower (no match)";
      begin
         Test (Report, Name,
               Count (To_Chunked_String ("ABABABA"),
                      "ABA",
                      Mapping => Map_To_Lower_Case_Ptr),
               0);
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      declare
         Name : constant String := "Mixed case mapped to lower";
      begin
         Test (Report, Name,
               Count (To_Chunked_String ("This IS a MISmatched issue"),
                      "is",
                      Ada.Characters.Handling.To_Lower'Access),
               4);
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      declare
         Name : constant String := "Upper case mapped to upper";
      begin
         Test (Report, Name,
               Count (To_Chunked_String ("ABABABA"),
                      "ABA",
                      Map_To_Upper_Case_Ptr),
               2);
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      declare
         Name : constant String := "Upper case mapped to upper (no match)";
      begin
         Test (Report, Name,
               Count (To_Chunked_String ("This IS a MISmatched issue"),
                      "is",
                      Mapping => Map_To_Upper_Case_Ptr),
               0);
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      declare
         Name : constant String := "Mixed case mapped to lower";
      begin
         Test (Report, Name,
               Count (To_Chunked_String
                        ("She sells sea shells by the sea shore"),
                      "s",
                      Ada.Characters.Handling.To_Lower'Access),
               8);
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      declare
         Name : constant String := "Empty string";
      begin
         Test (Report, Name,
               Count (Null_Chunked_String,
                      "match",
                      Map_To_Upper_Case_Ptr),
               0);
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      NT.End_Section (Report);


      declare
         Null_Pattern_String : constant String := "";
         TC_Natural          : Natural := 1000;
      begin
         TC_Natural := Count (To_Chunked_String ("A Valid String"),
                              Null_Pattern_String,
                              Map_To_Lower_Case_Ptr);
         NT.Item (Report, "Pattern_Error raised in Count", NT.Fail);
         NT.Info (Report, "No exception has been raised.");
         NT.Info (Report, "Return value: " & Natural'Image (TC_Natural));
      exception
         when Pattern_Error =>
            NT.Item (Report, "Pattern_Error raised in Count", NT.Success);
         when Error : others =>
            NT.Item (Report, "Pattern_Error raised in Count", NT.Fail);
            NT.Info (Report, "Wrong exception "
                             & Ada.Exceptions.Exception_Name (Error)
                             & "has been raised.");
      end;


      NT.Section (Report, "Function Translate");

      declare
         Name : constant String := "Mixed case mapped to lower";
      begin
         Test (Report, Name,
               Translate (Source  => To_Chunked_String
                                       ("A Sample Mixed Case String"),
                          Mapping => Map_To_Lower_Case_Ptr),
               "a sample mixed case string");
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      declare
         Name : constant String := "Upper case mapped to lower";
      begin
         Test (Report, Name,
               Translate (To_Chunked_String ("ALL LOWER CASE"),
                          Ada.Characters.Handling.To_Lower'Access),
               "all lower case");
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      declare
         Name : constant String := "Lower case mapped to lower";
      begin
         Test (Report, Name,
               Translate (To_Chunked_String ("end with lower case"),
                          Map_To_Lower_Case_Ptr),
               "end with lower case");
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      declare
         Name : constant String := "Empty string";
      begin
         Test (Report, Name,
               Translate (Null_Chunked_String,
                          Ada.Characters.Handling.To_Lower'Access),
               Null_Chunked_String);
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      declare
         Name : constant String := "Lower case mapped to upper";
      begin
         Test (Report, Name,
               Translate (To_Chunked_String ("start with lower case"),
                          Map_To_Upper_Case_Ptr),
               "START WITH LOWER CASE");
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      declare
         Name : constant String := "Upper case mapped to upper";
      begin
         Test (Report, Name,
               Translate (To_Chunked_String ("ALL UPPER CASE STRING"),
                          Ada.Characters.Handling.To_Upper'Access),
               "ALL UPPER CASE STRING");
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      declare
         Name : constant String := "Mixed case mapped to upper";
      begin
         Test (Report, Name,
               Translate (To_Chunked_String
                            ("LoTs Of MiXeD CaSe ChArAcTeRs"),
                          Map_To_Upper_Case_Ptr),
               "LOTS OF MIXED CASE CHARACTERS");
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      declare
         Name : constant String := "Diacritics";
      begin
         Test (Report, Name,
               Translate (New_Character_String,
                          Ada.Characters.Handling.To_Upper'Access),
               TC_New_Character_String);
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      NT.End_Section (Report);


      NT.Section (Report, "Procedure Translate");

      declare
         use Ada.Characters.Handling;

         Str_1    : Chunked_String
           := To_Chunked_String ("AN ALL UPPER CASE STRING");
         Str_2    : Chunked_String
           := To_Chunked_String ("A Mixed Case String");
         Str_3    : Chunked_String
           := To_Chunked_String ("a string with lower case letters");
         TC_Str_1 : constant Chunked_String := Str_1;
         TC_Str_3 : constant Chunked_String := Str_3;
      begin
         declare
            Name : constant String := "Upper case mapped to lower";
         begin
            Translate (Source => Str_1, Mapping => Map_To_Lower_Case_Ptr);
            Test (Report, Name, Str_1,
                  To_Chunked_String ("an all upper case string"));
         exception
            when Error : others => NT.Report_Exception (Report, Name, Error);
         end;

         declare
            Name : constant String := "Lower case mapped back to upper";
         begin
            Translate (Source => Str_1, Mapping => Map_To_Upper_Case_Ptr);
            Test (Report, Name, Str_1, TC_Str_1);
         exception
            when Error : others => NT.Report_Exception (Report, Name, Error);
         end;

         declare
            Name : constant String := "Mixed case mapped to lower";
         begin
            Translate (Str_2, Mapping => Map_To_Lower_Case_Ptr);
            Test (Report, Name, Str_2,
                  To_Chunked_String ("a mixed case string"));
         exception
            when Error : others => NT.Report_Exception (Report, Name, Error);
         end;

         declare
            Name : constant String := "Lower case mapped to upper";
         begin
            Translate (Str_2, Mapping => To_Upper'Access);
            Test (Report, Name, Str_2,
                  To_Chunked_String ("A MIXED CASE STRING"));
         exception
            when Error : others => NT.Report_Exception (Report, Name, Error);
         end;

         declare
            Name : constant String := "Lower case mapped to lower";
         begin
            Translate (Str_3, To_Lower'Access);
            Test (Report, Name, Str_3, TC_Str_3);
         exception
            when Error : others => NT.Report_Exception (Report, Name, Error);
         end;

         declare
            Name : constant String := "Lower case mapped to upper";
         begin
            Translate (Str_3, To_Upper'Access);
            Test (Report, Name, Str_3,
                  To_Chunked_String ("A STRING WITH LOWER CASE LETTERS"));
         exception
            when Error : others => NT.Report_Exception (Report, Name, Error);
         end;

         declare
            Name : constant String := "Diacritics";
         begin
            Translate (New_Character_String, Map_To_Upper_Case_Ptr);
            Test (Report, Name, New_Character_String, TC_New_Character_String);
         exception
            when Error : others => NT.Report_Exception (Report, Name, Error);
         end;

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

end Natools.Chunked_Strings.Tests.CXA4030;
