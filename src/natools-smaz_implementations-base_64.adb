------------------------------------------------------------------------------
-- Copyright (c) 2016, Natacha Porté                                        --
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

package body Natools.Smaz_Implementations.Base_64 is

   package Tools renames Natools.Smaz_Implementations.Base_64_Tools;

   use type Ada.Streams.Stream_Element_Offset;
   use type Tools.Base_64_Digit;


   ----------------------
   -- Public Interface --
   ----------------------

   procedure Read_Code
     (Input : in Ada.Streams.Stream_Element_Array;
      Offset : in out Ada.Streams.Stream_Element_Offset;
      Code : out Natools.Smaz_Implementations.Base_64_Tools.Base_64_Digit;
      Verbatim_Length : out Natural;
      Last_Code : in Natools.Smaz_Implementations.Base_64_Tools.Base_64_Digit;
      Variable_Length_Verbatim : in Boolean)
   is
      Ignored : String (1 .. 2);
      Offset_Backup : Ada.Streams.Stream_Element_Offset;
   begin
      Tools.Next_Digit (Input, Offset, Code);

      if Code <= Last_Code then
         Verbatim_Length := 0;

      elsif Variable_Length_Verbatim then
         Verbatim_Length := 64 - Natural (Code);
         Code := 0;

      elsif Code = 63 then
         Tools.Next_Digit (Input, Offset, Code);
         Verbatim_Length := Natural (Code) * 3 + 3;
         Code := 0;

      elsif Code = 62 then
         Offset_Backup := Offset;
         Tools.Decode_Single (Input, Offset, Ignored (1), Code);
         Verbatim_Length := Natural (Code) * 3 + 1;
         Offset := Offset_Backup;
         Code := 0;

      else
         Offset_Backup := Offset;
         Verbatim_Length := (61 - Natural (Code)) * 4;
         Tools.Decode_Double (Input, Offset, Ignored, Code);
         Verbatim_Length := (Verbatim_Length + Natural (Code)) * 3 + 2;
         Offset := Offset_Backup;
         Code := 0;
      end if;
   end Read_Code;


   procedure Read_Verbatim
     (Input : in Ada.Streams.Stream_Element_Array;
      Offset : in out Ada.Streams.Stream_Element_Offset;
      Output : out String)
   is
      Ignored : Tools.Base_64_Digit;
      Output_Index : Natural := Output'First - 1;
   begin
      if Output'Length mod 3 = 1 then
         Tools.Decode_Single
           (Input, Offset, Output (Output_Index + 1), Ignored);
         Output_Index := Output_Index + 1;
      elsif Output'Length mod 3 = 2 then
         Tools.Decode_Double
           (Input, Offset,
            Output (Output_Index + 1 .. Output_Index + 2), Ignored);
         Output_Index := Output_Index + 2;
      end if;

      if Output_Index < Output'Last then
         Tools.Decode
           (Input, Offset, Output (Output_Index + 1 .. Output'Last));
      end if;
   end Read_Verbatim;


   procedure Skip_Verbatim
     (Input : in Ada.Streams.Stream_Element_Array;
      Offset : in out Ada.Streams.Stream_Element_Offset;
      Verbatim_Length : in Positive)
   is
      Code : Tools.Base_64_Digit;
   begin
      for I in 1 .. Tools.Image_Length (Verbatim_Length) loop
         Tools.Next_Digit (Input, Offset, Code);
      end loop;
   end Skip_Verbatim;


   function Verbatim_Size
     (Input_Length : in Positive;
      Last_Code : in Natools.Smaz_Implementations.Base_64_Tools.Base_64_Digit;
      Variable_Length_Verbatim : in Boolean)
     return Ada.Streams.Stream_Element_Count is
   begin
      if Variable_Length_Verbatim then
         declare
            Largest_Run : constant Positive := 63 - Natural (Last_Code);
            Run_Count : constant Positive
              := (Input_Length + Largest_Run - 1) / Largest_Run;
            Last_Run_Size : constant Positive
              := Input_Length - (Run_Count - 1) * Largest_Run;
         begin
            return Ada.Streams.Stream_Element_Count (Run_Count - 1)
                    * (Tools.Image_Length (Largest_Run) + 1)
                 + Tools.Image_Length (Last_Run_Size) + 1;
         end;
      elsif Input_Length mod 3 = 0 then
         declare
            Largest_Run : constant Positive := 64 * 3;
            Run_Count : constant Positive
              := (Input_Length + Largest_Run - 1) / Largest_Run;
            Last_Run_Size : constant Positive
              := Input_Length - (Run_Count - 1) * Largest_Run;
         begin
            return Ada.Streams.Stream_Element_Count (Run_Count - 1)
                    * (Tools.Image_Length (Largest_Run) + 2)
                 + Tools.Image_Length (Last_Run_Size) + 2;
         end;
      elsif Input_Length mod 3 = 1 then
         declare
            Largest_Final_Run : constant Positive := 15 * 3 + 1;
            Largest_Prefix_Run : constant Positive := 64 * 3;
            Prefix_Run_Count : constant Natural
              := (Input_Length + Largest_Prefix_Run - Largest_Final_Run)
               / Largest_Prefix_Run;
            Last_Run_Size : constant Positive
              := Input_Length - Prefix_Run_Count * Largest_Prefix_Run;
         begin
            return Ada.Streams.Stream_Element_Count (Prefix_Run_Count)
                    * (Tools.Image_Length (Largest_Prefix_Run) + 2)
                 + Tools.Image_Length (Last_Run_Size) + 1;
         end;
      elsif Input_Length mod 3 = 2 then
         declare
            Largest_Final_Run : constant Positive
              := ((62 - Natural (Last_Code)) * 4 - 1) * 3 + 2;
            Largest_Prefix_Run : constant Positive := 64 * 3;
            Prefix_Run_Count : constant Natural
              := (Input_Length + Largest_Prefix_Run - Largest_Final_Run)
               / Largest_Prefix_Run;
            Last_Run_Size : constant Positive
              := Input_Length - Prefix_Run_Count * Largest_Prefix_Run;
         begin
            return Ada.Streams.Stream_Element_Count (Prefix_Run_Count)
                    * (Tools.Image_Length (Largest_Prefix_Run) + 2)
                 + Tools.Image_Length (Last_Run_Size) + 1;
         end;
      else
         raise Program_Error with "Condition unreachable";
      end if;
   end Verbatim_Size;


   procedure Write_Code
     (Output : in out Ada.Streams.Stream_Element_Array;
      Offset : in out Ada.Streams.Stream_Element_Offset;
      Code : in Natools.Smaz_Implementations.Base_64_Tools.Base_64_Digit) is
   begin
      Output (Offset) := Tools.Image (Code);
      Offset := Offset + 1;
   end Write_Code;


   procedure Write_Verbatim
     (Output : in out Ada.Streams.Stream_Element_Array;
      Offset : in out Ada.Streams.Stream_Element_Offset;
      Input : in String;
      Last_Code : in Natools.Smaz_Implementations.Base_64_Tools.Base_64_Digit;
      Variable_Length_Verbatim : in Boolean)
   is
      Index : Positive := Input'First;
   begin
      if Variable_Length_Verbatim then
         declare
            Largest_Run : constant Positive := 63 - Natural (Last_Code);
            Length, Last : Positive;
         begin
            while Index in Input'Range loop
               Length := Positive'Min (Largest_Run, Input'Last + 1 - Index);
               Last := Index + Length - 1;
               Output (Offset)
                 := Tools.Image (63 - Tools.Base_64_Digit (Length - 1));
               Offset := Offset + 1;
               Tools.Encode (Input (Index .. Last), Output, Offset);
               Index := Last + 1;
            end loop;
         end;
      else
         if Input'Length mod 3 = 1 then
            declare
               Extra_Blocks : constant Natural
                 := Natural'Min (15, Input'Length / 3);
            begin
               Output (Offset) := Tools.Image (62);
               Offset := Offset + 1;

               Tools.Encode_Single
                 (Input (Index), Tools.Single_Byte_Padding (Extra_Blocks),
                  Output, Offset);
               Index := Index + 1;

               if Extra_Blocks > 0 then
                  Tools.Encode
                    (Input (Index .. Index + Extra_Blocks * 3 - 1),
                     Output, Offset);
                  Index := Index + Extra_Blocks * 3;
               end if;
            end;
         elsif Input'Length mod 3 = 2 then
            declare
               Extra_Blocks : constant Natural := Natural'Min
                 (Input'Length / 3,
                  (62 - Natural (Last_Code)) * 4 - 1);
            begin
               Output (Offset)
                 := Tools.Image (61 - Tools.Base_64_Digit (Extra_Blocks / 4));
               Offset := Offset + 1;

               Tools.Encode_Double
                 (Input (Index .. Index + 1),
                  Tools.Double_Byte_Padding (Extra_Blocks mod 4),
                  Output, Offset);
               Index := Index + 2;

               if Extra_Blocks > 0 then
                  Tools.Encode
                    (Input (Index .. Index + Extra_Blocks * 3 - 1),
                     Output, Offset);
                  Index := Index + Extra_Blocks * 3;
               end if;
            end;
         end if;

         pragma Assert ((Input'Last + 1 - Index) mod 3 = 0);

         while Index <= Input'Last loop
            declare
               Block_Count : constant Natural
                 := Natural'Min (64, (Input'Last + 1 - Index) / 3);
            begin
               Output (Offset) := Tools.Image (63);
               Output (Offset + 1)
                 := Tools.Image (Tools.Base_64_Digit (Block_Count - 1));
               Offset := Offset + 2;

               Tools.Encode
                 (Input (Index .. Index + Block_Count * 3 - 1),
                  Output, Offset);
               Index := Index + Block_Count * 3;
            end;
         end loop;
      end if;
   end Write_Verbatim;

end Natools.Smaz_Implementations.Base_64;
