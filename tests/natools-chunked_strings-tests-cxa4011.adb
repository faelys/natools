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

with Ada.Strings;        use Ada.Strings;

procedure Natools.Chunked_Strings.Tests.CXA4011
  (Report : in out Natools.Tests.Reporter'Class)
is
   package NT renames Natools.Tests;

   procedure Test (Test_Name : String;
                   C_1       : Character;
                   C_2       : Character;
                   Name_1    : String;
                   Name_2    : String);


   procedure Test (Test_Name : String;
                   C_1       : Character;
                   C_2       : Character;
                   Name_1    : String;
                   Name_2    : String) is
   begin
      if C_1 = C_2 then
         NT.Item (Report, Test_Name, NT.Success);
      else
         NT.Item (Report, Test_Name, NT.Fail);
         NT.Info (Report, Name_1 & ": " & Character'Image (C_1));
         NT.Info (Report, Name_2 & ": " & Character'Image (C_2));
      end if;
   end Test;
begin
   NT.Section (Report, "Port of ACATS CXA4011");

   declare
      Cad_String : constant Chunked_String
        := To_Chunked_String ("cad");
      Complete_String : constant Chunked_String
        := To_Chunked_String ("Incomplete")
           & Ada.Strings.Space
           & To_Chunked_String ("String");
      Incomplete_String : Chunked_String
        := To_Chunked_String ("ncomplete Strin");
      Incorrect_Spelling : Chunked_String
        := To_Chunked_String ("Guob Dai");
      Magic_String : constant Chunked_String
        := To_Chunked_String ("abracadabra");
      Incantation : Chunked_String := Magic_String;

      A_Small_G : constant Character := 'g';
      A_Small_D : constant Character := 'd';

      ABCD_Set : constant Maps.Character_Set := Maps.To_Set ("abcd");
      B_Set    : constant Maps.Character_Set := Maps.To_Set ("b");
      AB_Set   : constant Maps.Character_Set
        := Maps."OR" (Maps.To_Set ('a'), B_Set);

      Code_Map         : constant Maps.Character_Mapping
        := Maps.To_Mapping (From => "abcd", To => "wxyz");
      Reverse_Code_Map : constant Maps.Character_Mapping
        := Maps.To_Mapping (From => "wxyz", To => "abcd");
      Non_Existent_Map : constant Maps.Character_Mapping
        := Maps.To_Mapping (From => "jkl",  To => "mno");

      Token_Start      : array (1 .. 3) of Positive;
      Token_End        : array (1 .. 3) of Natural := (0, 0, 0);
      Matching_Letters : Natural := 0;

      Tests : array (1 .. 5) of Boolean;
   begin
      declare
         Name : constant String := "Operator ""&""";
         Tests : array (1 .. 3) of Boolean;
      begin
         Incomplete_String := 'I' & Incomplete_String;
         Incomplete_String := Incomplete_String & A_Small_G;
         if not Is_Valid (Incomplete_String)
           or not Is_Valid (Complete_String)
         then
            NT.Item (Report, Name, NT.Error);
            if not Is_Valid (Incomplete_String) then
               NT.Info (Report, "Incomplete_String is invalid");
            end if;
            if not Is_Valid (Complete_String) then
               NT.Info (Report, "Complete_String is invalid");
            end if;
         else
            Tests (1) := Incomplete_String  < Complete_String;
            Tests (2) := Incomplete_String  > Complete_String;
            Tests (3) := Incomplete_String /= Complete_String;
            if Tests (1) or Tests (2) or Tests (3) then
               NT.Item (Report, Name, NT.Fail);
               NT.Info (Report, "Incomplete_String: """
                                & To_String (Incomplete_String) & '"');
               NT.Info (Report, "Complete_String:   """
                                & To_String (Complete_String) & '"');
               if Tests (1) then
                  NT.Info (Report, "-> Incomplete_String < Complete_String");
               end if;
               if Tests (2) then
                  NT.Info (Report, "-> Incomplete_String < Complete_String");
               end if;
               if Tests (3) then
                  NT.Info (Report, "-> Incomplete_String /= Complete_String");
               end if;
            else
               NT.Item (Report, Name, NT.Success);
            end if;
         end if;
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;


      NT.Section (Report, "Function Element");

      declare
         Name : constant String := "Element of complete vs constant";
      begin
         Test (Name,
               Element (Incomplete_String, Length (Incomplete_String)),
               A_Small_G,
               "Element (""" & To_String (Incomplete_String)
                 & ',' & Natural'Image (Length (Incomplete_String)) & ')',
               "A_Small_G");
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      declare
         Name : constant String := "Element of complete vs Element of Tail";
      begin
         Test (Name,
               Element (Incomplete_String, 2),
               Element (Tail (Incomplete_String, 2), 1),
               "Element (""" & To_String (Incomplete_String) & ", 2)",
               "Element (""" & To_String (Tail (Incomplete_String, 2))
                 & ", 1)");
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      declare
         Name : constant String := "Element of Head vs Element of constant";
      begin
         Test (Name,
               Element (Head (Incomplete_String, 4), 2),
               Element (To_Chunked_String ("wnqz"), 2),
               "Element (""" & To_String (Head (Incomplete_String, 4))
                 & ", 2)",
               "Element (""wnqz"", 2)");
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      NT.End_Section (Report);


      declare
         Name : constant String := "Procedure Replace_Element";
      begin
         Replace_Element (Incorrect_Spelling, 2, 'o');
         Replace_Element (Incorrect_Spelling,
                          Index (Incorrect_Spelling, B_Set),
                          A_Small_D);
         Replace_Element (Source => Incorrect_Spelling,
                          Index  => Length (Incorrect_Spelling),
                          By     => 'y');
         Test (Report, Name, Incorrect_Spelling, "Good Day");
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      --  Function Count
      Matching_Letters := Count (Source => Magic_String,
                                 Set    => ABCD_Set);
      NT.Item (Report, "Function Count with Set parameter",
                          NT.To_Result (Matching_Letters = 9));
      if Matching_Letters /= 9 then
         NT.Info
           (Report,
            "Count (""" & To_String (Magic_String) & """, ABCD_Set)  "
            & Natural'Image (Matching_Letters)
            & " (should be 9)");
         Dump (Report, Magic_String);
      end if;
      Tests (1) := Count (Magic_String, "ab")
                   = Count (Magic_String, "ac") + Count (Magic_String, "ad");
      Tests (2) := Count (Magic_String, "ab") = 2;
      NT.Item (Report, "Function Count with String parameter",
                          NT.To_Result (Tests (1) and Tests (2)));
      if not Tests (1) or not Tests (2) then
         NT.Info
           (Report,
            "Count (""" & To_String (Magic_String) & """, ""ab"")  "
            & Natural'Image (Count (Magic_String, "ab"))
            & "  (should be 2)");
         NT.Info
           (Report,
            "Count (""" & To_String (Magic_String) & """, ""ac"")  "
            & Natural'Image (Count (Magic_String, "ac")));
         NT.Info
           (Report,
            "Count (""" & To_String (Magic_String) & """, ""ad"")  "
            & Natural'Image (Count (Magic_String, "ad")));
      end if;

      --  Find_Token
      Find_Token (Magic_String,
                  AB_Set,
                  Ada.Strings.Inside,
                  Token_Start (1),
                  Token_End (1));
      Tests (1) := Natural (Token_Start (1)) = To_String (Magic_String)'First
                   and Token_End (1) = Index (Magic_String, B_Set);
      Find_Token (Source => Magic_String,
                  Set    => ABCD_Set,
                  Test   => Ada.Strings.Outside,
                  First  => Token_Start (2),
                  Last   => Token_End (2));
      Tests (2) := Natural (Token_Start (2)) = 3 and Token_End (2) = 3;
      Find_Token (Magic_String,
                  Maps.To_Set (A_Small_G),
                  Ada.Strings.Inside,
                  First => Token_Start (3),
                  Last  => Token_End (3));
      Tests (3) := Token_Start (3) = To_String (Magic_String)'First
                   and Token_End (3) = 0;
      NT.Item (Report, "Procedure Find_Token",
        NT.To_Result (Tests (1) and Tests (2) and Tests (3)));
      if not Tests (1) then
         NT.Info (Report,
                             "Start: "
                             & Positive'Image (Token_Start (1)) & " /= "
                             & Positive'Image (To_String (Magic_String)'First)
                             & "  (should be both 1)");
         NT.Info (Report,
                             "End:   "
                             & Natural'Image (Token_End (1)) & " /= "
                             & Natural'Image (Index (Magic_String, B_Set))
                             & "  (should be both 2)");
      end if;
      if not Tests (2) then
         NT.Info
           (Report,
            "Start: " & Positive'Image (Token_Start (2)) & "  (should be 3)");
         NT.Info
           (Report,
            "End:   " & Natural'Image (Token_End (2)) & "  (should be 3)");
      end if;
      if not Tests (3) then
         NT.Info
           (Report,
            "Start: "
            & Positive'Image (Token_Start (3)) & " /= "
            & Positive'Image (To_String (Magic_String)'First)
            & "  (should be 1)");
         NT.Info
           (Report,
            "End:   "
            & Natural'Image (Token_End (3)) & "  (should be 0)");
      end if;

      --  Translate
      Incantation := Translate (Magic_String, Code_Map);
      Tests (1) := Incantation = To_Chunked_String ("wxrwywzwxrw");
      NT.Item (Report, "Function Translate",
                          NT.To_Result (Tests (1)));
      if not Tests (1) then
         NT.Info (Report,
                             '"' & To_String (Incantation)
                             & """ /= ""wxrwywzwxrw""");
      end if;
      Translate (Incantation, Reverse_Code_Map);
      Tests (1) := Incantation = Translate (Magic_String, Non_Existent_Map);
      NT.Item
        (Report, "Procedure Translate", NT.To_Result (Tests (1)));
      if not Tests (1) then
         NT.Info (Report,
                             '"' & To_String (Incantation) & """ /= """
                             & To_String (Translate (Magic_String,
                                                     Non_Existent_Map))
                             & """  (should be """
                             & To_String (Magic_String) & """)");
      end if;

      --  Trim
      declare
         XYZ_Set      : constant Maps.Character_Set := Maps.To_Set ("xyz");
         PQR_Set      : constant Maps.Character_Set := Maps.To_Set ("pqr");
         Pad          : constant Chunked_String := To_Chunked_String ("Pad");
         The_New_Ada  : constant Chunked_String := To_Chunked_String ("Ada9X");
         Space_Array  : constant array (1 .. 4) of Chunked_String
           := (To_Chunked_String ("  Pad    "),
               To_Chunked_String ("Pad   "),
               To_Chunked_String ("     Pad"),
               Pad);
         String_Array : constant array (1 .. 5) of Chunked_String
           := (To_Chunked_String ("xyzxAda9Xpqr"),
               To_Chunked_String ("Ada9Xqqrp"),
               To_Chunked_String ("zxyxAda9Xqpqr"),
               To_Chunked_String ("xxxyAda9X"),
               The_New_Ada);
      begin
         for I in 1 .. 4 loop
            Tests (I) := Trim (Space_Array (I), Ada.Strings.Both) = Pad;
         end loop;
         NT.Item
           (Report, "Trim spaces",
            NT.To_Result (Tests (1) and Tests (2)
                                 and Tests (3) and Tests (4)));
         for I in 1 .. 4 loop
            if not Tests (I) then
               NT.Info
                 (Report,
                  "Part" & Positive'Image (I) & ": Trim ("""
                  & To_String (Space_Array (I)) & """, Both) -> """
                  & To_String (Trim (Space_Array (I), Ada.Strings.Both))
                  & """  (shoud be """ & To_String (Pad) & '"');
            end if;
         end loop;

         for I in 1 .. 5 loop
            Tests (I) := Trim (String_Array (I),
                               Left  => XYZ_Set,
                               Right => PQR_Set)
                         = The_New_Ada;
         end loop;
         NT.Item
           (Report, "Trim sets of characters",
            NT.To_Result (Tests (1) and Tests (2) and Tests (3)
                                 and Tests (4) and Tests (5)));
         for I in 1 .. 5 loop
            if not Tests (I) then
               NT.Info
                 (Report,
                  "Part" & Positive'Image (I) & ": Trim ("""
                  & To_String (String_Array (I))
                  & """, XYZ_Set, PQR_Set) -> """
                  & To_String (Trim (String_Array (I), XYZ_Set, PQR_Set))
                  & """  (shoud be """ & To_String (The_New_Ada) & '"');
            end if;
         end loop;
      end;

      --  Delete
      Tests (1) := Delete (Source  => Delete (Magic_String,
                                              8, Length (Magic_String)),
                           From    => To_String (Magic_String)'First,
                           Through => 4)
                   = Cad_String;
      NT.Item (Report, "Function Delete",
                          NT.To_Result (Tests (1)));
      if not Tests (1) then
         NT.Info
           (Report,
            '"' & To_String (Delete (Delete (Magic_String,
                                             8, Length (Magic_String)),
                                     To_String (Magic_String)'First, 4))
            & """ /= """ & To_String (Cad_String) & '"');
      end if;

      --  Constructors "*"
      declare
         SOS           : Chunked_String;
         Dot           : constant Chunked_String := To_Chunked_String ("Dot_");
         Dash          : constant String := "Dash_";
         Distress      : constant Chunked_String
           := To_Chunked_String ("Dot_Dot_Dot_")
            & To_Chunked_String ("Dash_Dash_Dash_")
            & To_Chunked_String ("Dot_Dot_Dot");
         Repeat        : constant Natural := 3;
         Separator     : constant Character := '_';
         Separator_Set : constant Maps.Character_Set
           := Maps.To_Set (Separator);
      begin
         SOS := Repeat * Dot;
         SOS := SOS & Repeat * Dash & Repeat * Dot;
         if Trim (SOS, Maps.Null_Set, Separator_Set) /= Distress then
            NT.Item (Report, "Function ""*""", NT.Fail);
            NT.Info
              (Report,
               '"' & To_String (Trim (SOS, Maps.Null_Set, Separator_Set))
               & """ /= """ & To_String (Distress) & '"');
         else
            NT.Item (Report, "Function ""*""",
                                NT.Success);
         end if;
      end;
   exception
      when Error : others =>
         NT.Report_Exception (Report, "Preparation", Error);
   end;

   NT.End_Section (Report);

end Natools.Chunked_Strings.Tests.CXA4011;
