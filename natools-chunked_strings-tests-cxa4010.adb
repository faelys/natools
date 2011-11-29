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

with Ada.Strings;    use Ada.Strings;

procedure Natools.Chunked_Strings.Tests.CXA4010
  (Report : in out Natools.Tests.Reporter'Class) is
begin
   Natools.Tests.Section (Report, "Port of ACATS CXA4010");

   declare

      Pamphlet_Paragraph_Count : constant :=  2;
      Lines                    : constant :=  4;
      Line_Length              : constant := 40;

      type Document_Type is array (Positive range <>) of Chunked_String;

      type Camera_Ready_Copy_Type is
        array (1 .. Lines) of String (1 .. Line_Length);

      procedure Enter_Text_Into_Document (Document : in out Document_Type);
      procedure Create_Camera_Ready_Copy
        (Document : in Document_Type;
         Camera_Copy : out Camera_Ready_Copy_Type);
      procedure Valid_Proofread (Draft, Master : Camera_Ready_Copy_Type);

      Pamphlet            : Document_Type (1 .. Pamphlet_Paragraph_Count);
      Camera_Ready_Copy   : Camera_Ready_Copy_Type :=
                              (others => (others => Ada.Strings.Space));
      TC_Finished_Product : constant Camera_Ready_Copy_Type :=
                           (1 => "Ada is a programming language designed  ",
                            2 => "to support long-lived, reliable software",
                            3 => " systems.                               ",
                            4 => "Go with Ada!                            ");


      procedure Enter_Text_Into_Document (Document : in out Document_Type) is
      begin
         Document (1) := To_Chunked_String ("Ada is a language");
         Document (1) := Insert (Document (1),
                                 Index (Document (1), "language"),
                                 To_String ("progra"
                                          & Chunked_Strings."*" (2, 'm')
                                          & "ing "));
         Document (1) :=
           Overwrite (Document (1),
                      Index (Document (1),
                             To_String (Tail (Document (1), 8, ' ')),
                             Ada.Strings.Backward),
                      "language designed to support long-lifed");
         Document (1) :=
           Overwrite (Document (1),
                      Index (Document (1),
                             To_String (Tail (Document (1), 5, ' ')),
                             Ada.Strings.Backward),
                      "lived, reliable software systems.");
         Document (2) := 'G'
                       & To_Chunked_String ("o ")
                       & To_Chunked_String ("with")
                       & ' '
                       & "Ada!";
      end Enter_Text_Into_Document;


      procedure Create_Camera_Ready_Copy
        (Document : in Document_Type;
         Camera_Copy : out Camera_Ready_Copy_Type) is
      begin
         Camera_Copy (1) :=
           Slice (Document (1),
                  1,
                  Index (To_Chunked_String (Slice (Document (1),
                                                   1, Line_Length)),
                         Ada.Strings.Maps.To_Set (' '),
                         Ada.Strings.Inside,
                         Ada.Strings.Backward))
           & ' ';
         Camera_Copy (2) :=
           Slice (Document (1),
                  40,
                  Index_Non_Blank (To_Chunked_String (Slice (Document (1),
                                                             40, 79)),
                                   Ada.Strings.Backward) + 39);
         Camera_Copy (3) (1 .. 9) :=
           Slice (Document (1), 80, Length (Document (1)));
         Camera_Copy (4) (1 .. Length (Document (2))) :=
           To_String (Head (Document (2), Length (Document (2))));
      end Create_Camera_Ready_Copy;


      procedure Valid_Proofread (Draft, Master : Camera_Ready_Copy_Type) is
      begin
         for I in Draft'Range loop
            declare
               Name : constant String := "Slice" & Positive'Image (I);
            begin
               if Draft (I) = Master (I) then
                  Natools.Tests.Item (Report, Name, Natools.Tests.Success);
               else
                  Natools.Tests.Item (Report, Name, Natools.Tests.Fail);
                  Natools.Tests.Info (Report, "Draft:  """ & Draft (I) & '"');
                  Natools.Tests.Info (Report, "Master: """ & Master (I) & '"');
               end if;
            exception
               when Error : others =>
                  Natools.Tests.Report_Exception (Report, Name, Error);
            end;
         end loop;
      end Valid_Proofread;
   begin
      Enter_Text_Into_Document (Pamphlet);
      Create_Camera_Ready_Copy (Document    => Pamphlet,
                                Camera_Copy => Camera_Ready_Copy);
      Valid_Proofread (Draft  => Camera_Ready_Copy,
                       Master => TC_Finished_Product);
   exception
      when Error : others =>
         Natools.Tests.Report_Exception (Report, "Preparation", Error);
   end;

   Natools.Tests.End_Section (Report);

end Natools.Chunked_Strings.Tests.CXA4010;
