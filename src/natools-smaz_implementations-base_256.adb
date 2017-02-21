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

package body Natools.Smaz_Implementations.Base_256 is

   use type Ada.Streams.Stream_Element;
   use type Ada.Streams.Stream_Element_Offset;


   procedure Read_Code
     (Input : in Ada.Streams.Stream_Element_Array;
      Offset : in out Ada.Streams.Stream_Element_Offset;
      Code : out Ada.Streams.Stream_Element;
      Verbatim_Length : out Natural;
      Last_Code : in Ada.Streams.Stream_Element;
      Variable_Length_Verbatim : in Boolean)
   is
      Input_Byte : constant Ada.Streams.Stream_Element := Input (Offset);
   begin
      if Input_Byte <= Last_Code then
         Code := Input_Byte;
         Verbatim_Length := 0;
      else
         Code := 0;

         if not Variable_Length_Verbatim then
            Verbatim_Length
              := Natural (Ada.Streams.Stream_Element'Last - Input_Byte) + 1;
         elsif Input_Byte < Ada.Streams.Stream_Element'Last then
            Verbatim_Length
              := Positive (Ada.Streams.Stream_Element'Last - Input_Byte);
         else
            Offset := Offset + 1;
            Verbatim_Length
              := Positive (Input (Offset))
               + Natural (Ada.Streams.Stream_Element'Last - Last_Code)
               - 1;
         end if;
      end if;

      Offset := Offset + 1;
   end Read_Code;


   procedure Read_Verbatim
     (Input : in Ada.Streams.Stream_Element_Array;
      Offset : in out Ada.Streams.Stream_Element_Offset;
      Output : out String) is
   begin
      for I in Output'Range loop
         Output (I) := Character'Val (Input (Offset));
         Offset := Offset + 1;
      end loop;
   end Read_Verbatim;


   procedure Skip_Verbatim
     (Input : in Ada.Streams.Stream_Element_Array;
      Offset : in out Ada.Streams.Stream_Element_Offset;
      Verbatim_Length : in Positive)
   is
      pragma Unreferenced (Input);
   begin
      Offset := Offset + Ada.Streams.Stream_Element_Offset (Verbatim_Length);
   end Skip_Verbatim;


   function Verbatim_Size
     (Input_Length : in Positive;
      Last_Code : in Ada.Streams.Stream_Element;
      Variable_Length_Verbatim : in Boolean)
     return Ada.Streams.Stream_Element_Count
   is
      Verbatim1_Max_Size : constant Ada.Streams.Stream_Element_Count
        := Ada.Streams.Stream_Element_Count
            (Ada.Streams.Stream_Element'Last - Last_Code)
         - Boolean'Pos (Variable_Length_Verbatim);
      Verbatim2_Max_Size : constant Ada.Streams.Stream_Element_Count
        := Ada.Streams.Stream_Element_Count (Ada.Streams.Stream_Element'Last)
         + Verbatim1_Max_Size;

      Remaining : Ada.Streams.Stream_Element_Count
        := Ada.Streams.Stream_Element_Count (Input_Length);
      Overhead : Ada.Streams.Stream_Element_Count := 0;
   begin
      if Variable_Length_Verbatim then
         if Remaining >= Verbatim2_Max_Size then
            declare
               Full_Blocks : constant Ada.Streams.Stream_Element_Count
                 := Remaining / Verbatim2_Max_Size;
            begin
               Overhead := Overhead + 2 * Full_Blocks;
               Remaining := Remaining - Verbatim2_Max_Size * Full_Blocks;
            end;
         end if;

         if Remaining > Verbatim1_Max_Size then
            Overhead := Overhead + 2;
            Remaining := 0;
         end if;
      end if;

      declare
         Block_Count : constant Ada.Streams.Stream_Element_Count
           := (Remaining + Verbatim1_Max_Size - 1) / Verbatim1_Max_Size;
      begin
         Overhead := Overhead + Block_Count;
      end;

      return Overhead + Ada.Streams.Stream_Element_Count (Input_Length);
   end Verbatim_Size;


   procedure Write_Code
     (Output : in out Ada.Streams.Stream_Element_Array;
      Offset : in out Ada.Streams.Stream_Element_Offset;
      Code : in Ada.Streams.Stream_Element) is
   begin
      Output (Offset) := Code;
      Offset := Offset + 1;
   end Write_Code;


   procedure Write_Verbatim
     (Output : in out Ada.Streams.Stream_Element_Array;
      Offset : in out Ada.Streams.Stream_Element_Offset;
      Input : in String;
      Last_Code : in Ada.Streams.Stream_Element;
      Variable_Length_Verbatim : in Boolean)
   is
      Verbatim1_Max_Size : constant Natural
        := Natural (Ada.Streams.Stream_Element'Last - Last_Code)
         - Boolean'Pos (Variable_Length_Verbatim);
      Verbatim2_Max_Size : constant Natural
        := Natural (Ada.Streams.Stream_Element'Last)
         + Verbatim1_Max_Size;

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
            Output (Offset) := Ada.Streams.Stream_Element'Last;
            Output (Offset + 1) := Ada.Streams.Stream_Element
              (Block_Length - Verbatim1_Max_Size);
            Offset := Offset + 2;
         else
            Block_Length := Positive'Min
              (Remaining_Length, Verbatim1_Max_Size);
            Output (Offset)
              := Ada.Streams.Stream_Element'Last
               - Ada.Streams.Stream_Element
                  (Block_Length - 1 + Boolean'Pos (Variable_Length_Verbatim));
            Offset := Offset + 1;
         end if;

         Verbatim_Copy :
         for I in 1 .. Block_Length loop
            Output (Offset) := Character'Pos (Input (Input_Index));
            Offset := Offset + 1;
            Input_Index := Input_Index + 1;
         end loop Verbatim_Copy;
      end loop;
   end Write_Verbatim;

end Natools.Smaz_Implementations.Base_256;
