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

package Natools.S_Expressions.Encodings is
   pragma Pure (Natools.S_Expressions.Encodings);


   -----------------------------
   -- Meaningful Octet Values --
   -----------------------------

   --  S-expression Active Characters  --

   Base64_Atom_Begin : constant Octet := Character'Pos ('|');
   Base64_Atom_End   : constant Octet := Character'Pos ('|');
   Base64_Expr_Begin : constant Octet := Character'Pos ('{');
   Base64_Expr_End   : constant Octet := Character'Pos ('}');
   Base64_Filler     : constant Octet := Character'Pos ('=');
   Escape            : constant Octet := Character'Pos ('\');
   Hex_Atom_Begin    : constant Octet := Character'Pos ('#');
   Hex_Atom_End      : constant Octet := Character'Pos ('#');
   List_Begin        : constant Octet := Character'Pos ('(');
   List_End          : constant Octet := Character'Pos (')');
   Quoted_Atom_Begin : constant Octet := Character'Pos ('"');
   Quoted_Atom_End   : constant Octet := Character'Pos ('"');
   Verbatim_Begin    : constant Octet := Character'Pos (':');


   --  Blanks  --

   HT      : constant Octet := 9;
   LF      : constant Octet := 10;
   VT      : constant Octet := 11;
   FF      : constant Octet := 12;
   CR      : constant Octet := 13;
   Space   : constant Octet := 32;


   --  Encoding-related Values  --

   Digit_0 : constant Octet := Character'Pos ('0');
   Digit_9 : constant Octet := Character'Pos ('9');
   Lower_A : constant Octet := Character'Pos ('a');
   Lower_F : constant Octet := Character'Pos ('f');
   Lower_Z : constant Octet := Character'Pos ('z');
   Upper_A : constant Octet := Character'Pos ('A');
   Upper_F : constant Octet := Character'Pos ('F');
   Upper_Z : constant Octet := Character'Pos ('Z');
   Plus    : constant Octet := Character'Pos ('+');
   Slash   : constant Octet := Character'Pos ('/');



   ---------------------------------------
   -- Hexadecimal Encoding and Decoding --
   ---------------------------------------

   type Hex_Casing is (Upper, Lower);

   function Is_Hex_Digit (Value : in Octet) return Boolean;
   function Decode_Hex (Value : in Octet) return Octet;
   function Decode_Hex (High, Low : in Octet) return Octet;
   function Decode_Hex (Data : in Atom) return Atom;

   function Encode_Hex (Value : in Octet; Casing : in Hex_Casing) return Octet;
   procedure Encode_Hex
     (Value : in Octet;
      Casing : in Hex_Casing;
      High, Low : out Octet);
   function Encode_Hex (Data : in Atom; Casing : in Hex_Casing) return Atom;



   -----------------------------------
   -- Base-64 Encoding and Decoding --
   -----------------------------------

   function Is_Base64_Digit (Value : in Octet) return Boolean;
   function Decode_Base64 (Value : in Octet) return Octet;
   function Decode_Base64 (A, B : in Octet) return Atom;
   function Decode_Base64 (A, B, C : in Octet) return Atom;
   function Decode_Base64 (A, B, C, D : in Octet) return Atom;
   function Decode_Base64 (Data : in Atom) return Atom;

   function Encode_Base64 (Value : in Octet) return Octet;
   procedure Encode_Base64 (Output : out Atom; A : in Octet);
   procedure Encode_Base64 (Output : out Atom; A, B : in Octet);
   procedure Encode_Base64 (Output : out Atom; A, B, C : in Octet);
   function Encode_Base64 (Data : in Atom) return Atom;

end Natools.S_Expressions.Encodings;
