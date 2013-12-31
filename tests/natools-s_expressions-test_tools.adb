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

package body Natools.S_Expressions.Test_Tools is

   Hex_Digits : constant String := "0123456789ABCDEF";

   function Encode_Hex (Value : Offset; Length : Positive) return String;
   function Hex_Slice
     (Address : Offset;
      Address_Length : Positive;
      Data : Atom;
      Width : Positive)
     return String;

   function Is_Printable (Data : Octet) return Boolean;
   function Is_Printable (Data : Atom) return Boolean;
      --  Return whether Data can be dumped directed as a String or Character


   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

   function Encode_Hex (Value : Offset; Length : Positive) return String is
      I : Natural := Length;
      Digit : Natural;
      Current : Offset := Value;
   begin
      return Result : String (1 .. Length) := (others => '0') do
         while Current /= 0 and I /= 0 loop
            Digit := Natural (Current mod 16);
            Result (I) := Hex_Digits (Hex_Digits'First + Digit);
            I := I - 1;
            Current := Current / 16;
         end loop;
      end return;
   end Encode_Hex;


   function Hex_Slice
     (Address : Offset;
      Address_Length : Positive;
      Data : Atom;
      Width : Positive)
     return String
   is
      Total_Length : constant Positive
        := Address_Length + 4 + 4 * Width;
      Hex_Start : constant Positive := Address_Length + 2;
      Raw_Start : constant Positive := Hex_Start + 3 * Width + 1;
      Digit : Octet;
   begin
      return Result : String (1 .. Total_Length) := (others => ' ') do
         Result (1 .. Address_Length) := Encode_Hex (Address, Address_Length);

         for I in 0 .. Width - 1 loop
            exit when Data'First + Offset (I) not in Data'Range;

            Digit := Data (Data'First + Offset (I));

            Result (Hex_Start + 3 * I) := Hex_Digits (Hex_Digits'First
              + Natural (Digit / 16));
            Result (Hex_Start + 3 * I + 1) := Hex_Digits (Hex_Digits'First
              + Natural (Digit mod 16));

            if Is_Printable (Digit) then
               Result (Raw_Start + I) := Character'Val (Digit);
            else
               Result (Raw_Start + I) := '.';
            end if;
         end loop;
      end return;
   end Hex_Slice;


   function Is_Printable (Data : Octet) return Boolean is
   begin
      return Data in 32 .. 127;
   end Is_Printable;


   function Is_Printable (Data : Atom) return Boolean is
   begin
      if Data'Length > 100 then
         return False;
      end if;

      for I in Data'Range loop
         if not Is_Printable (Data (I)) then
            return False;
         end if;
      end loop;

      return True;
   end Is_Printable;



   ------------------
   -- Public Tools --
   ------------------

   procedure Dump_Atom
     (Report : in out NT.Reporter'Class;
      Data : in Atom;
      Label : in String := "")
   is
      I, Length : Offset := 0;
   begin
      if Is_Printable (Data) then
         if Label'Length > 0 then
            Report.Info (Label & ": """ & To_String (Data) & '"');
         else
            Report.Info ('"' & To_String (Data) & '"');
         end if;
      else
         if Label'Length > 0 then
            Report.Info
              (Label & ": " & Natural'Image (Data'Length) & " octets");
         end if;

         while I < Data'Length loop
            Length := Offset'Min (16, Data'Length - I);
            Report.Info (Hex_Slice
              (I, 8,
               Data (Data'First + I .. Data'First + I + Length - 1), 16));
            I := I + 16;
         end loop;
      end if;
   end Dump_Atom;


   procedure Test_Atom
     (Report : in out NT.Reporter'Class;
      Test_Name : in String;
      Expected : in Atom;
      Found : in Atom) is
   begin
      if Found = Expected then
         Report.Item (Test_Name, NT.Success);
      else
         Report.Item (Test_Name, NT.Fail);
         Dump_Atom (Report, Found, "Found");
         Dump_Atom (Report, Expected, "Expected");
      end if;
   end Test_Atom;

end Natools.S_Expressions.Test_Tools;
