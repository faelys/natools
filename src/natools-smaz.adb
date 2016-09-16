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

package body Natools.Smaz is

   use type Ada.Streams.Stream_Element_Offset;

   function Dict_Entry
     (Dict : in Dictionary;
      Index : in Ada.Streams.Stream_Element)
     return String
     with Pre => Index <= Dict.Dict_Last;

   procedure Find_Entry
     (Dict : in Dictionary;
      Template : in String;
      Index : out Ada.Streams.Stream_Element;
      Length : out Natural);

   function To_String (Data : in Ada.Streams.Stream_Element_Array)
     return String;

   function Verbatim_Size (Dict : Dictionary; Original_Size : Natural)
     return Ada.Streams.Stream_Element_Count;


   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

   function Dict_Entry
     (Dict : in Dictionary;
      Index : in Ada.Streams.Stream_Element)
     return String
   is
      First : constant Positive := Dict.Offsets (Index);
      Last : Natural := Dict.Values'Last;
   begin
      if Index + 1 in Dict.Offsets'Range then
         Last := Dict.Offsets (Index + 1) - 1;
      end if;

      return Dict.Values (First .. Last);
   end Dict_Entry;


   procedure Find_Entry
     (Dict : in Dictionary;
      Template : in String;
      Index : out Ada.Streams.Stream_Element;
      Length : out Natural)
   is
      I : Ada.Streams.Stream_Element;
      N : Natural;
   begin
      Index := Ada.Streams.Stream_Element'Last;
      Length := 0;

      for Last in reverse Template'Range loop
         N := Dict.Hash (Template (Template'First .. Last));

         if N <= Natural (Dict.Dict_Last) then
            I := Ada.Streams.Stream_Element (N);
            if Dict_Entry (Dict, I) = Template (Template'First .. Last) then
               Index := I;
               Length := 1 + Last - Template'First;
               return;
            end if;
         end if;
      end loop;
   end Find_Entry;


   function To_String (Data : in Ada.Streams.Stream_Element_Array)
     return String is
   begin
      return Result : String (1 .. Data'Length) do
         for I in Result'Range loop
            Result (I) := Character'Val (Data
              (Data'First + Ada.Streams.Stream_Element_Offset (I - 1)));
         end loop;
      end return;
   end To_String;


   function Verbatim_Size (Dict : Dictionary; Original_Size : Natural)
     return Ada.Streams.Stream_Element_Count
   is
      Verbatim1_Max_Size : constant Ada.Streams.Stream_Element_Count
        := Ada.Streams.Stream_Element_Count
            (Ada.Streams.Stream_Element'Last - Dict.Dict_Last)
         - Boolean'Pos (Dict.Variable_Length_Verbatim);
      Verbatim2_Max_Size : constant Ada.Streams.Stream_Element_Count
        := Ada.Streams.Stream_Element_Count (Ada.Streams.Stream_Element'Last)
         + Verbatim1_Max_Size;

      Remaining : Ada.Streams.Stream_Element_Count
        := Ada.Streams.Stream_Element_Count (Original_Size);
      Overhead : Ada.Streams.Stream_Element_Count := 0;
   begin
      if Dict.Variable_Length_Verbatim then
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
         Full_Blocks : constant Ada.Streams.Stream_Element_Count
           := Remaining / Verbatim1_Max_Size;
      begin
         Overhead := Overhead + Full_Blocks;
      end;

      return Overhead + Ada.Streams.Stream_Element_Count (Original_Size);
   end Verbatim_Size;



   ----------------------
   -- Public Interface --
   ----------------------

   function Compressed_Upper_Bound
     (Dict : in Dictionary;
      Input : in String)
     return Ada.Streams.Stream_Element_Count is
   begin
      return Verbatim_Size (Dict, Input'Length);
   end Compressed_Upper_Bound;


   procedure Compress
     (Dict : in Dictionary;
      Input : in String;
      Output_Buffer : out Ada.Streams.Stream_Element_Array;
      Output_Last : out Ada.Streams.Stream_Element_Offset)
   is
      procedure Find_Entry;

      Verbatim1_Max_Size : constant Natural
        := Natural (Ada.Streams.Stream_Element'Last - Dict.Dict_Last)
         - Boolean'Pos (Dict.Variable_Length_Verbatim);
      Verbatim2_Max_Size : constant Natural
        := Natural (Ada.Streams.Stream_Element'Last)
         + Verbatim1_Max_Size;

      Input_Index : Positive := Input'First;
      Length : Natural;
      Word : Ada.Streams.Stream_Element;

      procedure Find_Entry is
      begin
         Find_Entry
           (Dict,
            Input (Input_Index
                   .. Natural'Min (Input_Index + Dict.Max_Word_Length - 1,
                                   Input'Last)),
            Word,
            Length);
      end Find_Entry;

      Previous_Verbatim_Beginning : Natural := 0;
      Previous_Verbatim_Last : Ada.Streams.Stream_Element_Offset := 0;
   begin
      Output_Last := Output_Buffer'First - 1;
      Find_Entry;

      Main_Loop :
      while Input_Index in Input'Range loop
         Data_In_Dict :
         while Length > 0 loop
            Output_Last := Output_Last + 1;
            Output_Buffer (Output_Last) := Word;
            Input_Index := Input_Index + Length;
            exit Main_Loop when Input_Index not in Input'Range;
            Find_Entry;
         end loop Data_In_Dict;

         Verbatim_Block :
         declare
            Beginning : Positive := Input_Index;
            Verbatim_Length, Block_Length : Natural;
         begin
            Verbatim_Scan :
            while Length = 0 and Input_Index in Input'Range loop
               Input_Index := Input_Index + 1;
               Find_Entry;
            end loop Verbatim_Scan;

            Verbatim_Length := Input_Index - Beginning;

            if Previous_Verbatim_Beginning > 0
              and then Output_Last + Verbatim_Size (Dict, Verbatim_Length)
                 > Previous_Verbatim_Last + Verbatim_Size
                    (Dict, Input_Index - Previous_Verbatim_Beginning)
            then
               Beginning := Previous_Verbatim_Beginning;
               Output_Last := Previous_Verbatim_Last;
               Verbatim_Length := Input_Index - Beginning;
            else
               Previous_Verbatim_Beginning := Beginning;
               Previous_Verbatim_Last := Output_Last;
            end if;

            Verbatim_Encode :
            while Verbatim_Length > 0 loop
               if Dict.Variable_Length_Verbatim
                 and then Verbatim_Length > Verbatim1_Max_Size
               then
                  Block_Length := Natural'Min
                    (Verbatim_Length, Verbatim2_Max_Size);
                  Output_Buffer (Output_Last + 1)
                    := Ada.Streams.Stream_Element'Last;
                  Output_Buffer (Output_Last + 2) := Ada.Streams.Stream_Element
                    (Block_Length - Verbatim1_Max_Size);
                  Output_Last := Output_Last + 2;
               else
                  Block_Length := Natural'Min
                    (Verbatim_Length, Verbatim1_Max_Size);
                  Output_Last := Output_Last + 1;
                  Output_Buffer (Output_Last)
                    := Ada.Streams.Stream_Element'Last
                     - Ada.Streams.Stream_Element
                        (Block_Length - 1
                          + Boolean'Pos (Dict.Variable_Length_Verbatim));
               end if;

               Verbatim_Copy :
               for I in Beginning .. Beginning + Block_Length - 1 loop
                  Output_Last := Output_Last + 1;
                  Output_Buffer (Output_Last) := Character'Pos (Input (I));
               end loop Verbatim_Copy;

               Verbatim_Length := Verbatim_Length - Block_Length;
               Beginning := Beginning + Block_Length;
            end loop Verbatim_Encode;
         end Verbatim_Block;
      end loop Main_Loop;
   end Compress;


   function Decompressed_Length
     (Dict : in Dictionary;
      Input : in Ada.Streams.Stream_Element_Array)
     return Natural
   is
      Result : Natural := 0;
      Verbatim_Code_Count : constant Ada.Streams.Stream_Element_Offset
        := Ada.Streams.Stream_Element_Offset
           (Ada.Streams.Stream_Element'Last - Dict.Dict_Last);
      Input_Index : Ada.Streams.Stream_Element_Offset := Input'First;
      Input_Byte : Ada.Streams.Stream_Element;
      Verbatim_Length : Ada.Streams.Stream_Element_Offset;
   begin
      while Input_Index in Input'Range loop
         Input_Byte := Input (Input_Index);

         if Input_Byte in Dict.Offsets'Range then
            Result := Result + Dict_Entry (Dict, Input_Byte)'Length;
            Input_Index := Input_Index + 1;
         else
            if not Dict.Variable_Length_Verbatim then
               Verbatim_Length := Ada.Streams.Stream_Element_Offset
                 (Ada.Streams.Stream_Element'Last - Input_Byte) + 1;
            elsif Input_Byte < Ada.Streams.Stream_Element'Last then
               Verbatim_Length := Ada.Streams.Stream_Element_Offset
                 (Ada.Streams.Stream_Element'Last - Input_Byte);
            else
               Input_Index := Input_Index + 1;
               Verbatim_Length := Ada.Streams.Stream_Element_Offset
                 (Input (Input_Index)) + Verbatim_Code_Count - 1;
            end if;

            Result := Result + Positive (Verbatim_Length);
            Input_Index := Input_Index + Verbatim_Length + 1;
         end if;
      end loop;

      return Result;
   end Decompressed_Length;


   procedure Decompress
     (Dict : in Dictionary;
      Input : in Ada.Streams.Stream_Element_Array;
      Output_Buffer : out String;
      Output_Last : out Natural)
   is
      procedure Append (S : in String);
      procedure Append (S : in Ada.Streams.Stream_Element_Array);

      procedure Append (S : in String) is
      begin
         Output_Buffer (Output_Last + 1 .. Output_Last + S'Length) := S;
         Output_Last := Output_Last + S'Length;
      end Append;

      procedure Append (S : in Ada.Streams.Stream_Element_Array) is
      begin
         Append (To_String (S));
      end Append;

      Verbatim_Code_Count : constant Ada.Streams.Stream_Element_Offset
        := Ada.Streams.Stream_Element_Offset
           (Ada.Streams.Stream_Element'Last - Dict.Dict_Last);

      Input_Index : Ada.Streams.Stream_Element_Offset := Input'First;
      Input_Byte : Ada.Streams.Stream_Element;
      Verbatim_Length : Ada.Streams.Stream_Element_Offset;
   begin
      Output_Last := Output_Buffer'First - 1;

      while Input_Index in Input'Range loop
         Input_Byte := Input (Input_Index);

         if Input_Byte in Dict.Offsets'Range then
            Append (Dict_Entry (Dict, Input_Byte));
            Input_Index := Input_Index + 1;
         else
            if not Dict.Variable_Length_Verbatim then
               Verbatim_Length := Ada.Streams.Stream_Element_Offset
                 (Ada.Streams.Stream_Element'Last - Input_Byte) + 1;
            elsif Input_Byte < Ada.Streams.Stream_Element'Last then
               Verbatim_Length := Ada.Streams.Stream_Element_Offset
                 (Ada.Streams.Stream_Element'Last - Input_Byte);
            else
               Input_Index := Input_Index + 1;
               Verbatim_Length := Ada.Streams.Stream_Element_Offset
                 (Input (Input_Index)) + Verbatim_Code_Count - 1;
            end if;

            Append (Input (Input_Index + 1 .. Input_Index + Verbatim_Length));
            Input_Index := Input_Index + Verbatim_Length + 1;
         end if;
      end loop;
   end Decompress;

end Natools.Smaz;
