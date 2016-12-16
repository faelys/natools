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

package body Natools.Smaz_Generic is

   use type Ada.Streams.Stream_Element_Offset;


   procedure Find_Entry
     (Dict : in Dictionary;
      Template : in String;
      Code : out Dictionary_Code;
      Length : out Natural);
      --  Try to find the longest entry in Dict that is a prefix of Template,
      --  setting Length to 0 when no such entry exists.

   function Verbatim_Size
     (Dict : in Dictionary;
      Length : in Positive)
     return Ada.Streams.Stream_Element_Count
   is (Verbatim_Size (Length, Dict.Last_Code, Dict.Variable_Length_Verbatim));
      --  Wrapper around the formal Verbatim_Size


   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

   procedure Find_Entry
     (Dict : in Dictionary;
      Template : in String;
      Code : out Dictionary_Code;
      Length : out Natural)
   is
      N : Natural;
      Is_Valid : Boolean;
   begin
      Length := 0;

      for Last in reverse Template'Range loop
         Is_Valid := False;
         N := Dict.Hash (Template (Template'First .. Last));

         To_Code :
         begin
            Code := Dictionary_Code'Val (N);
            if Is_Valid_Code (Dict, Code) then
               Is_Valid := True;
            end if;
         exception
            when Constraint_Error => null;
         end To_Code;

         if Is_Valid
           and then Template (Template'First .. Last)
              = Dict.Values (Code_First (Dict.Offsets, Code, Dict.Values'First)
                          .. Code_Last (Dict.Offsets, Code, Dict.Values'Last))
         then
            Length := 1 + Last - Template'First;
            return;
         end if;
      end loop;
   end Find_Entry;



   ----------------------
   -- Public Interface --
   ----------------------

   function Compressed_Upper_Bound
     (Dict : in Dictionary;
      Input : in String)
     return Ada.Streams.Stream_Element_Count is
   begin
      return Verbatim_Size
        (Input'Length, Dict.Last_Code, Dict.Variable_Length_Verbatim);
   end Compressed_Upper_Bound;


   procedure Compress
     (Dict : in Dictionary;
      Input : in String;
      Output_Buffer : out Ada.Streams.Stream_Element_Array;
      Output_Last : out Ada.Streams.Stream_Element_Offset)
   is
      procedure Find_Current_Entry;

      Input_Index : Positive := Input'First;
      Length : Natural;
      Code : Dictionary_Code;
      Output_Index : Ada.Streams.Stream_Element_Offset;

      procedure Find_Current_Entry is
      begin
         Find_Entry
           (Dict,
            Input (Input_Index
                   .. Natural'Min (Input_Index + Dict.Max_Word_Length - 1,
                                   Input'Last)),
            Code,
            Length);
      end Find_Current_Entry;

      Previous_Verbatim_Beginning : Natural := 0;
      Previous_Verbatim_Index : Ada.Streams.Stream_Element_Offset := 0;
   begin
      Output_Index := Output_Buffer'First;
      Find_Current_Entry;

      Main_Loop :
      while Input_Index in Input'Range loop
         Data_In_Dict :
         while Length > 0 loop
            Write_Code (Output_Buffer, Output_Index, Code);
            Input_Index := Input_Index + Length;
            exit Main_Loop when Input_Index not in Input'Range;
            Find_Current_Entry;
         end loop Data_In_Dict;

         Verbatim_Block :
         declare
            Beginning : Positive := Input_Index;
            Verbatim_Length : Natural;
         begin
            Verbatim_Scan :
            while Length = 0 and Input_Index in Input'Range loop
               Input_Index := Input_Index + 1;
               Find_Current_Entry;
            end loop Verbatim_Scan;

            Verbatim_Length := Input_Index - Beginning;

            if Previous_Verbatim_Beginning > 0
              and then Output_Index + Verbatim_Size (Dict, Verbatim_Length)
                 >= Previous_Verbatim_Index + Verbatim_Size
                    (Dict, Input_Index - Previous_Verbatim_Beginning)
            then
               Beginning := Previous_Verbatim_Beginning;
               Output_Index := Previous_Verbatim_Index;
               Verbatim_Length := Input_Index - Beginning;
            else
               Previous_Verbatim_Beginning := Beginning;
               Previous_Verbatim_Index := Output_Index;
            end if;

            Write_Verbatim
              (Output_Buffer, Output_Index,
               Input (Beginning .. Input_Index - 1),
               Dict.Last_Code, Dict.Variable_Length_Verbatim);
         end Verbatim_Block;
      end loop Main_Loop;

      Output_Last := Output_Index - 1;
   end Compress;


   function Compress (Dict : in Dictionary; Input : in String)
     return Ada.Streams.Stream_Element_Array
   is
      Result : Ada.Streams.Stream_Element_Array
        (1 .. Compressed_Upper_Bound (Dict, Input));
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      Compress (Dict, Input, Result, Last);
      return Result (Result'First .. Last);
   end Compress;


   function Decompressed_Length
     (Dict : in Dictionary;
      Input : in Ada.Streams.Stream_Element_Array)
     return Natural
   is
      Result : Natural := 0;
      Input_Index : Ada.Streams.Stream_Element_Offset := Input'First;
      Code : Dictionary_Code;
      Verbatim_Length : Natural;
   begin
      while Input_Index in Input'Range loop
         Read_Code
           (Input, Input_Index,
            Code, Verbatim_Length,
            Dict.Last_Code, Dict.Variable_Length_Verbatim);

         if Verbatim_Length > 0 then
            Skip_Verbatim (Input, Input_Index, Verbatim_Length);
            Result := Result + Verbatim_Length;
         else
            Result := Result + Dict_Entry_Length (Dict, Code);
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
      Input_Index : Ada.Streams.Stream_Element_Offset := Input'First;
      Code : Dictionary_Code;
      Verbatim_Length : Natural;
   begin
      Output_Last := Output_Buffer'First - 1;

      while Input_Index in Input'Range loop
         Read_Code
           (Input, Input_Index,
            Code, Verbatim_Length,
            Dict.Last_Code, Dict.Variable_Length_Verbatim);

         if Verbatim_Length > 0 then
            Read_Verbatim
              (Input, Input_Index,
               Output_Buffer
                 (Output_Last + 1 .. Output_Last + Verbatim_Length));
            Output_Last := Output_Last + Verbatim_Length;
         else
            declare
               Decoded : constant String := Dict_Entry (Dict, Code);
            begin
               Output_Buffer (Output_Last + 1 .. Output_Last + Decoded'Length)
                 := Decoded;
               Output_Last := Output_Last + Decoded'Length;
            end;
         end if;
      end loop;
   end Decompress;


   function Decompress
     (Dict : in Dictionary; Input : in Ada.Streams.Stream_Element_Array)
     return String
   is
      Result : String (1 .. Decompressed_Length (Dict, Input));
      Last : Natural;
   begin
      Decompress (Dict, Input, Result, Last);
      pragma Assert (Last = Result'Last);
      return Result;
   end Decompress;

end Natools.Smaz_Generic;
