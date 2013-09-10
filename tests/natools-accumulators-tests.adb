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

package body Natools.Accumulators.Tests is
   package NT renames Natools.Tests;

   procedure Check_Emptiness
     (Report : in out NT.Reporter'Class;
      Accumulator : in out String_Accumulator'Class;
      Test_Name : in String);
   --  Add an item to Report, success being emptiness of Accumulator

   procedure Check_Contents
     (Report : in out NT.Reporter'Class;
      Accumulator : in out String_Accumulator'Class;
      Test_Name : in String;
      Reference : in String);
   --  Add an item to Report, success being Accumulator matching Reference



   procedure Check_Contents
     (Report : in out NT.Reporter'Class;
      Accumulator : in out String_Accumulator'Class;
      Test_Name : in String;
      Reference : in String)
   is
      S : constant String := Accumulator.To_String;
      L : constant Natural := Accumulator.Length;
   begin
      if S'Length /= L then
         NT.Item (Report, Test_Name, NT.Fail);
         NT.Info
           (Report,
            "Inconsistent length" & Natural'Image (L)
            & " for string """ & S & '"');
      elsif S /= Reference then
         NT.Item (Report, Test_Name, NT.Fail);
         NT.Info (Report, "Accumulated """ & S & '"');
         NT.Info (Report, "Reference   """ & Reference & '"');
      else
         NT.Item (Report, Test_Name, NT.Success);
      end if;
   end Check_Contents;


   procedure Check_Emptiness
     (Report : in out NT.Reporter'Class;
      Accumulator : in out String_Accumulator'Class;
      Test_Name : in String)
   is
      L : constant Natural := Accumulator.Length;
      S : constant String := Accumulator.To_String;
   begin
      if L /= 0 or S /= "" then
         NT.Item (Report, Test_Name, NT.Fail);
         NT.Info (Report, "Accumulator.Length is" & Natural'Image (L));
         NT.Info (Report, "Accumulator.To_String is """ & S & '"');
      else
         NT.Item (Report, Test_Name, NT.Success);
      end if;
   end Check_Emptiness;


   procedure Test
     (Report : in out Natools.Tests.Reporter'Class;
      Accumulator : in out String_Accumulator'Class)
   is
      Part_1 : constant String
        := "The quick brown fox jumps over the lazy dog.";
      Part_2 : constant String
        := "Lorem ipsum dolor sit amet, consectetur adipisicing elit.";
      L_1 : constant Natural := 10;
      L_2 : constant Natural := 7;
   begin
      declare
         Name : constant String := "Soft_Reset";
      begin
         Accumulator.Soft_Reset;
         Check_Emptiness (Report, Accumulator, Name);
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      declare
         Name : constant String := "String append";
      begin
         Accumulator.Append (Part_1);
         Check_Contents (Report, Accumulator, Name, Part_1);
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      declare
         Name : constant String := "Character accumulation";
      begin
         for I in Part_2'Range loop
            Accumulator.Append (String'(1 => Part_2 (I)));
         end loop;

         Check_Contents (Report, Accumulator, Name, Part_1 & Part_2);
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      declare
         Name : constant String := "Tail extraction";
         OK : Boolean := True;
      begin
         declare
            T : constant String := Accumulator.Tail (L_1);
         begin
            if T'Length /= L_1 then
               NT.Item (Report, Name, NT.Fail);
               NT.Info (Report,
                  "Wrong tail length" & Natural'Image (T'Length)
                  & ", expected" & Natural'Image (L_1));
               OK := False;
            elsif T /= Part_2 (Part_2'Last - L_1 + 1 .. Part_2'Last) then
               NT.Item (Report, Name, NT.Fail);
               NT.Info (Report, "Incorrect tail """ & T & '"');
               NT.Info (Report, "Expected """
                 & Part_2 (Part_2'Last - L_1 + 1 .. Part_2'Last) & '"');
               OK := False;
            end if;
         end;

         if OK then
            Check_Contents (Report, Accumulator, Name, Part_1 & Part_2);
         end if;
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      declare
         Name : constant String := "Unappend";
      begin
         Accumulator.Unappend (Part_2 (Part_2'Last - L_2 + 1 .. Part_2'Last));
         Check_Contents (Report, Accumulator, Name,
           Part_1 & Part_2 (Part_2'First .. Part_2'Last - L_2));
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      declare
         Name : constant String := "No-op Unappend";
      begin
         Accumulator.Unappend (Part_1);
         Check_Contents (Report, Accumulator, Name,
           Part_1 & Part_2 (Part_2'First .. Part_2'Last - L_2));
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      declare
         Name : constant String := "In-place To_String";
         Target : String (1 .. Part_1'Length + Part_2'Length)
           := (others => '*');
      begin
         Accumulator.To_String (Target);
         Target (Part_1'Length + Part_2'Length - L_2 + 1 .. Target'Last)
           := Part_2 (Part_2'Last - L_2 + 1 .. Part_2'Last);

         if Target /= Part_1 & Part_2 then
            NT.Item (Report, Name, NT.Fail);
            NT.Info (Report, "Found """ & Target & '"');
            NT.Info (Report, "Expected """ & Target & '"');
         else
            Check_Contents (Report, Accumulator, Name,
              Part_1 & Part_2 (Part_2'First .. Part_2'Last - L_2));
         end if;
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;

      declare
         Name : constant String := "Hard_Reset";
      begin
         Accumulator.Hard_Reset;
         Check_Emptiness (Report, Accumulator, Name);
      exception
         when Error : others => NT.Report_Exception (Report, Name, Error);
      end;
   end Test;

end Natools.Accumulators.Tests;
