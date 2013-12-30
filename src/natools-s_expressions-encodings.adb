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

package body Natools.S_Expressions.Encodings is

   --------------------------
   -- Hexadecimal Decoding --
   --------------------------

   function Is_Hex_Digit (Value : in Octet) return Boolean is
   begin
      case Value is
         when Digit_0 .. Digit_9 => return True;
         when Lower_A .. Lower_F => return True;
         when Upper_A .. Upper_F => return True;
         when others             => return False;
      end case;
   end Is_Hex_Digit;


   function Decode_Hex (Value : in Octet) return Octet is
   begin
      case Value is
         when Digit_0 .. Digit_9 => return Value - Digit_0;
         when Lower_A .. Lower_F => return Value - Lower_A + 10;
         when Upper_A .. Upper_F => return Value - Upper_A + 10;
         when others             => raise Constraint_Error;
      end case;
   end Decode_Hex;


   function Decode_Hex (High, Low : in Octet) return Octet is
   begin
      return Decode_Hex (High) * 16 + Decode_Hex (Low);
   end Decode_Hex;


   function Decode_Hex (Data : in Atom) return Atom is
      Length : Count := 0;
   begin
      for I in Data'Range loop
         if Is_Hex_Digit (Data (I)) then
            Length := Length + 1;
         end if;
      end loop;

      Length := (Length + 1) / 2;

      return Result : Atom (0 .. Length - 1) do
         declare
            O : Offset := Result'First;
            High : Octet := 0;
            Has_High : Boolean := False;
         begin
            for I in Data'Range loop
               if Is_Hex_Digit (Data (I)) then
                  if Has_High then
                     Result (O) := Decode_Hex (High, Data (I));
                     O := O + 1;
                     High := 0;
                     Has_High := False;
                  else
                     High := Data (I);
                     Has_High := True;
                  end if;
               end if;
            end loop;
            if Has_High then
               Result (O) := Decode_Hex (High, 0);
               O := O + 1;
            end if;
            pragma Assert (O - 1 = Result'Last);
         end;
      end return;
   end Decode_Hex;



   --------------------------
   -- Hexadecimal Encoding --
   --------------------------

   function Encode_Hex (Value : in Octet; Casing : in Hex_Casing)
     return Octet is
   begin
      case Value is
         when 0 .. 9 =>
            return Digit_0 + Value;
         when 10 .. 15 =>
            case Casing is
               when Upper => return Upper_A + Value - 10;
               when Lower => return Lower_A + Value - 10;
            end case;
         when others =>
            raise Constraint_Error;
      end case;
   end Encode_Hex;


   procedure Encode_Hex
     (Value : in Octet;
      Casing : in Hex_Casing;
      High, Low : out Octet) is
   begin
      High := Encode_Hex (Value / 2**4 mod 2**4, Casing);
      Low  := Encode_Hex (Value mod 2**4, Casing);
   end Encode_Hex;


   function Encode_Hex (Data : in Atom; Casing : in Hex_Casing) return Atom is
      Result : Atom (0 .. Data'Length * 2 - 1);
      Cursor : Offset := Result'First;
   begin
      for I in Data'Range loop
         Encode_Hex (Data (I), Casing, Result (Cursor), Result (Cursor + 1));
         Cursor := Cursor + 2;
      end loop;
      pragma Assert (Cursor = Result'Last + 1);

      return Result;
   end Encode_Hex;



   ----------------------
   -- Base-64 Decoding --
   ----------------------

   function Is_Base64_Digit (Value : in Octet) return Boolean is
   begin
      return Value in Digit_0 .. Digit_9
        or Value in Lower_A .. Lower_Z
        or Value in Upper_A .. Upper_Z
        or Value = Plus
        or Value = Slash;
   end Is_Base64_Digit;


   function Decode_Base64 (Value : in Octet) return Octet is
   begin
      case Value is
         when Upper_A .. Upper_Z => return Value - Upper_A + 0;
         when Lower_A .. Lower_Z => return Value - Lower_A + 26;
         when Digit_0 .. Digit_9 => return Value - Digit_0 + 52;
         when Plus               => return 62;
         when Slash              => return 63;
         when others             => raise Constraint_Error;
      end case;
   end Decode_Base64;


   function Decode_Base64 (A, B : in Octet) return Atom is
      VA : constant Octet := Decode_Base64 (A);
      VB : constant Octet := Decode_Base64 (B);
   begin
      return (0 => VA * 2**2 + VB / 2**4);
   end Decode_Base64;


   function Decode_Base64 (A, B, C : in Octet) return Atom is
      VA : constant Octet := Decode_Base64 (A);
      VB : constant Octet := Decode_Base64 (B);
      VC : constant Octet := Decode_Base64 (C);
   begin
      return (0 => VA * 2**2 + VB / 2**4,
              1 => VB * 2**4 + VC / 2**2);
   end Decode_Base64;


   function Decode_Base64 (A, B, C, D : in Octet) return Atom is
      VA : constant Octet := Decode_Base64 (A);
      VB : constant Octet := Decode_Base64 (B);
      VC : constant Octet := Decode_Base64 (C);
      VD : constant Octet := Decode_Base64 (D);
   begin
      return (0 => VA * 2**2 + VB / 2**4,
              1 => VB * 2**4 + VC / 2**2,
              2 => VC * 2**6 + VD);
   end Decode_Base64;


   function Decode_Base64 (Data : in Atom) return Atom is
      Length : Count := 0;
   begin
      for I in Data'Range loop
         if Is_Base64_Digit (Data (I)) then
            Length := Length + 1;
         end if;
      end loop;

      declare
         Chunks : constant Count := Length / 4;
         Remains : constant Count := Length mod 4;
      begin
         if Remains >= 2 then
            Length := Chunks * 3 + Remains - 1;
         else
            Length := Chunks * 3;
         end if;
      end;

      return Result : Atom (0 .. Length - 1) do
         declare
            O : Count := Result'First;
            Buffer : Atom (0 .. 3);
            Accumulated : Count := 0;
         begin
            for I in Data'Range loop
               if Is_Base64_Digit (Data (I)) then
                  Buffer (Accumulated) := Data (I);
                  Accumulated := Accumulated + 1;
                  if Accumulated = 4 then
                     Result (O .. O + 2) := Decode_Base64 (Buffer (0),
                                                           Buffer (1),
                                                           Buffer (2),
                                                           Buffer (3));
                     O := O + 3;
                     Accumulated := 0;
                  end if;
               end if;
            end loop;

            if Accumulated = 2 then
               Result (O .. O) := Decode_Base64 (Buffer (0), Buffer (1));
               O := O + 1;
            elsif Accumulated = 3 then
               Result (O .. O + 1) := Decode_Base64 (Buffer (0),
                                                     Buffer (1),
                                                     Buffer (2));
               O := O + 2;
            end if;

            pragma Assert (O = Length);
         end;
      end return;
   end Decode_Base64;



   ----------------------
   -- Base-64 Encoding --
   ----------------------

   function Encode_Base64 (Value : in Octet) return Octet is
   begin
      case Value is
         when 0 .. 25 =>
            return Upper_A + Value;
         when 26 .. 51 =>
            return Lower_A + Value - 26;
         when 52 .. 61 =>
            return Digit_0 + Value - 52;
         when 62 =>
            return Plus;
         when 63 =>
            return Slash;
         when others =>
            raise Constraint_Error;
      end case;
   end Encode_Base64;


   procedure Encode_Base64 (Output : out Atom; A : in Octet) is
   begin
      Output (Output'First + 0) := Encode_Base64 (A / 2**2 mod 2**6);
      Output (Output'First + 1) := Encode_Base64 (A * 2**4 mod 2**6);
      Output (Output'First + 2) := Base64_Filler;
      Output (Output'First + 3) := Base64_Filler;
   end Encode_Base64;


   procedure Encode_Base64 (Output : out Atom; A, B : in Octet) is
   begin
      Output (Output'First + 0) := Encode_Base64 (A / 2**2 mod 2**6);
      Output (Output'First + 1) := Encode_Base64 ((A * 2**4 + B / 2**4)
                                                  mod 2**6);
      Output (Output'First + 2) := Encode_Base64 (B * 2**2 mod 2**6);
      Output (Output'First + 3) := Base64_Filler;
   end Encode_Base64;


   procedure Encode_Base64 (Output : out Atom; A, B, C : in Octet) is
   begin
      Output (Output'First + 0) := Encode_Base64 (A / 2**2 mod 2**6);
      Output (Output'First + 1) := Encode_Base64 ((A * 2**4 + B / 2**4)
                                                  mod 2**6);
      Output (Output'First + 2) := Encode_Base64 ((B * 2**2 + C / 2**6)
                                                  mod 2**6);
      Output (Output'First + 3) := Encode_Base64 (C mod 2**6);
   end Encode_Base64;


   function Encode_Base64 (Data : in Atom) return Atom is
      Chunks : constant Count := (Data'Length + 2) / 3;
      Result : Atom (0 .. Chunks * 4 - 1);
      Cursor : Offset := Result'First;
      I      : Offset := Data'First;
   begin
      while I in Data'Range loop
         if I + 2 in Data'Range then
            Encode_Base64
              (Result (Cursor .. Cursor + 3),
               Data (I),
               Data (I + 1),
               Data (I + 2));
            I := I + 3;
         elsif I + 1 in Data'Range then
            Encode_Base64
              (Result (Cursor .. Cursor + 3),
               Data (I),
               Data (I + 1));
            I := I + 2;
         else
            Encode_Base64
              (Result (Cursor .. Cursor + 3),
               Data (I));
            I := I + 1;
         end if;
         Cursor := Cursor + 4;
      end loop;

      return Result;
   end Encode_Base64;

end Natools.S_Expressions.Encodings;
