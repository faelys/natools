------------------------------------------------------------------------------
-- Copyright (c) 2016-2017, Natacha Porté                                   --
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

with Natools.Smaz_Implementations.Base_64_Tools;

package body Natools.Smaz_Implementations.Base_4096 is

   package Tools renames Natools.Smaz_Implementations.Base_64_Tools;

   use type Ada.Streams.Stream_Element_Offset;

   procedure Read_Code
     (Input : in Ada.Streams.Stream_Element_Array;
      Offset : in out Ada.Streams.Stream_Element_Offset;
      Code : out Base_4096_Digit);
      --  Read two base-64 symbols and assemble them into a base-4096 number


   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

   procedure Read_Code
     (Input : in Ada.Streams.Stream_Element_Array;
      Offset : in out Ada.Streams.Stream_Element_Offset;
      Code : out Base_4096_Digit)
   is
      Low, High : Tools.Base_64_Digit;
   begin
      Tools.Next_Digit (Input, Offset, Low);
      Tools.Next_Digit (Input, Offset, High);
      Code := Base_4096_Digit (Low) + Base_4096_Digit (High) * 64;
   end Read_Code;



   ----------------------
   -- Public Interface --
   ----------------------

   procedure Read_Code
     (Input : in Ada.Streams.Stream_Element_Array;
      Offset : in out Ada.Streams.Stream_Element_Offset;
      Code : out Base_4096_Digit;
      Verbatim_Length : out Natural;
      Last_Code : in Base_4096_Digit;
      Variable_Length_Verbatim : in Boolean) is
   begin
      Read_Code (Input, Offset, Code);

      if Code <= Last_Code then
         Verbatim_Length := 0;
      elsif not Variable_Length_Verbatim then
         Verbatim_Length := Positive (Base_4096_Digit'Last - Code + 1);
         Code := 0;
      elsif Code < Base_4096_Digit'Last then
         Verbatim_Length := Positive (Base_4096_Digit'Last - Code);
         Code := 0;
      else
         Read_Code (Input, Offset, Code);
         Verbatim_Length
           := Natural (Code) + Natural (Base_4096_Digit'Last - Last_Code);
         Code := 0;
      end if;
   end Read_Code;


   procedure Read_Verbatim
     (Input : in Ada.Streams.Stream_Element_Array;
      Offset : in out Ada.Streams.Stream_Element_Offset;
      Output : out String) is
   begin
      Tools.Decode (Input, Offset, Output);
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
      Last_Code : in Base_4096_Digit;
      Variable_Length_Verbatim : in Boolean)
     return Ada.Streams.Stream_Element_Count
   is
      Verbatim1_Max_Size : constant Natural
        := Natural (Base_4096_Digit'Last - Last_Code)
         - Boolean'Pos (Variable_Length_Verbatim);
      Verbatim2_Max_Size : constant Natural
        := Natural (Base_4096_Digit'Last) + Verbatim1_Max_Size + 1;

      Input_Index : Natural := 0;
      Remaining_Length, Block_Length : Positive;
      Result : Ada.Streams.Stream_Element_Count := 0;
   begin
      while Input_Index < Input_Length loop
         Remaining_Length := Input_Length - Input_Index;

         if Variable_Length_Verbatim
           and then Remaining_Length > Verbatim1_Max_Size
         then
            Block_Length := Positive'Min
              (Remaining_Length, Verbatim2_Max_Size);
            Result := Result + 4;
         else
            Block_Length := Positive'Min
              (Remaining_Length, Verbatim1_Max_Size);
            Result := Result + 2;
         end if;

         Result := Result + Tools.Image_Length (Block_Length);
         Input_Index := Input_Index + Block_Length;
      end loop;

      return Result;
   end Verbatim_Size;


   procedure Write_Code
     (Output : in out Ada.Streams.Stream_Element_Array;
      Offset : in out Ada.Streams.Stream_Element_Offset;
      Code : in Base_4096_Digit)
   is
      Low : constant Tools.Base_64_Digit := Tools.Base_64_Digit (Code mod 64);
      High : constant Tools.Base_64_Digit := Tools.Base_64_Digit (Code / 64);
   begin
      Output (Offset + 0) := Tools.Image (Low);
      Output (Offset + 1) := Tools.Image (High);
      Offset := Offset + 2;
   end Write_Code;


   procedure Write_Verbatim
     (Output : in out Ada.Streams.Stream_Element_Array;
      Offset : in out Ada.Streams.Stream_Element_Offset;
      Input : in String;
      Last_Code : in Base_4096_Digit;
      Variable_Length_Verbatim : in Boolean)
   is
      Verbatim1_Max_Size : constant Natural
        := Natural (Base_4096_Digit'Last - Last_Code)
         - Boolean'Pos (Variable_Length_Verbatim);
      Verbatim2_Max_Size : constant Natural
        := Natural (Base_4096_Digit'Last) + Verbatim1_Max_Size + 1;

      Input_Index : Positive := Input'First;
      Remaining_Length, Block_Length : Positive;
   begin
      while Input_Index in Input'Range loop
         Remaining_Length := Input'Last - Input_Index + 1;

         if Variable_Length_Verbatim
           and then Remaining_Length > Verbatim1_Max_Size
         then
            Block_Length := Positive'Min
              (Remaining_Length, Verbatim2_Max_Size);
            Write_Code (Output, Offset, Base_4096_Digit'Last);
            Write_Code (Output, Offset, Base_4096_Digit
              (Block_Length - Verbatim1_Max_Size - 1));
         else
            Block_Length := Positive'Min
              (Remaining_Length, Verbatim1_Max_Size);
            Write_Code (Output, Offset, Base_4096_Digit
              (Base_4096_Digit'Last - Base_4096_Digit (Block_Length)
                  + 1 - Boolean'Pos (Variable_Length_Verbatim)));
         end if;

         Tools.Encode
           (Input (Input_Index .. Input_Index + Block_Length - 1),
            Output, Offset);
         Input_Index := Input_Index + Block_Length;
      end loop;
   end Write_Verbatim;

end Natools.Smaz_Implementations.Base_4096;
