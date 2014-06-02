--  Generated at 2014-06-02 19:12:04 +0000 by Natools.Static_Hash_Maps
--  from ../src/natools-s_expressions-printers-pretty-config-commands.sx

private package Natools.S_Expressions.Printers.Pretty.Config.Commands is
   pragma Preelaborate;

   type Main_Command is
     (Set_Char_Encoding,
      Set_Fallback,
      Set_Hex_Casing,
      Set_Indentation,
      Set_Newline,
      Set_Newline_Encoding,
      Set_Quoted,
      Set_Quoted_String,
      Set_Space_At,
      Set_Tab_Stop,
      Set_Token,
      Set_Width);

   type Newline_Command is
     (Set_Newline_Command_Encoding,
      Set_Newline_Separator);

   type Quoted_String_Command is
     (Set_Quoted_Option,
      Set_Quoted_Escape);

   type Separator_Command is
     (All_Separators,
      No_Separators,
      Invert_Separators,
      Open_Open,
      Open_Atom,
      Open_Close,
      Atom_Open,
      Atom_Atom,
      Atom_Close,
      Close_Open,
      Close_Atom,
      Close_Close);

   function Main (Key : String) return Main_Command;
   function Newline (Key : String) return Newline_Command;
   function Quoted_String (Key : String) return Quoted_String_Command;
   function Separator (Key : String) return Separator_Command;
   function To_Atom_Encoding (Key : String) return Atom_Encoding;
   function To_Character_Encoding (Key : String) return Character_Encoding;
   function To_Hex_Casing (Key : String) return Encodings.Hex_Casing;
   function To_Newline_Encoding (Key : String) return Newline_Encoding;
   function To_Quoted_Escape (Key : String) return Quoted_Escape_Type;
   function To_Quoted_Option (Key : String) return Quoted_Option;
   function To_Token_Option (Key : String) return Token_Option;

private

   Map_1_Key_0 : aliased constant String := "ascii";
   Map_1_Key_1 : aliased constant String := "ASCII";
   Map_1_Key_2 : aliased constant String := "latin-1";
   Map_1_Key_3 : aliased constant String := "latin";
   Map_1_Key_4 : aliased constant String := "iso-8859-1";
   Map_1_Key_5 : aliased constant String := "ISO-8859-1";
   Map_1_Key_6 : aliased constant String := "utf-8";
   Map_1_Key_7 : aliased constant String := "UTF-8";
   Map_1_Key_8 : aliased constant String := "utf8";
   Map_1_Key_9 : aliased constant String := "UTF8";
   Map_1_Key_10 : aliased constant String := "base64";
   Map_1_Key_11 : aliased constant String := "base-64";
   Map_1_Key_12 : aliased constant String := "lower-hex";
   Map_1_Key_13 : aliased constant String := "lower-hexa";
   Map_1_Key_14 : aliased constant String := "hex";
   Map_1_Key_15 : aliased constant String := "hexa";
   Map_1_Key_16 : aliased constant String := "hexadecimal";
   Map_1_Key_17 : aliased constant String := "upper-hex";
   Map_1_Key_18 : aliased constant String := "upper-hexa";
   Map_1_Key_19 : aliased constant String := "verbatim";
   Map_1_Key_20 : aliased constant String := "lower";
   Map_1_Key_21 : aliased constant String := "lower-case";
   Map_1_Key_22 : aliased constant String := "upper";
   Map_1_Key_23 : aliased constant String := "upper-case";
   Map_1_Key_24 : aliased constant String := "indent";
   Map_1_Key_25 : aliased constant String := "indentation";
   Map_1_Key_26 : aliased constant String := "no-indent";
   Map_1_Key_27 : aliased constant String := "no-indentation";
   Map_1_Key_28 : aliased constant String := "newline";
   Map_1_Key_29 : aliased constant String := "cr";
   Map_1_Key_30 : aliased constant String := "CR";
   Map_1_Key_31 : aliased constant String := "lf";
   Map_1_Key_32 : aliased constant String := "LF";
   Map_1_Key_33 : aliased constant String := "CRLF";
   Map_1_Key_34 : aliased constant String := "CR-LF";
   Map_1_Key_35 : aliased constant String := "crlf";
   Map_1_Key_36 : aliased constant String := "cr-lf";
   Map_1_Key_37 : aliased constant String := "lf-cr";
   Map_1_Key_38 : aliased constant String := "lfcr";
   Map_1_Key_39 : aliased constant String := "LF-CR";
   Map_1_Key_40 : aliased constant String := "LFCR";
   Map_1_Key_41 : aliased constant String := "no-quoted";
   Map_1_Key_42 : aliased constant String := "no-quoted-string";
   Map_1_Key_43 : aliased constant String := "quoted-when-shorter";
   Map_1_Key_44 : aliased constant String := "quoted-string-when-shorter";
   Map_1_Key_45 : aliased constant String := "single-line-quoted";
   Map_1_Key_46 : aliased constant String := "single-line-quoted-string";
   Map_1_Key_47 : aliased constant String := "escape";
   Map_1_Key_48 : aliased constant String := "quoted";
   Map_1_Key_49 : aliased constant String := "space";
   Map_1_Key_50 : aliased constant String := "tab-stop";
   Map_1_Key_51 : aliased constant String := "extended-token";
   Map_1_Key_52 : aliased constant String := "no-token";
   Map_1_Key_53 : aliased constant String := "standard-token";
   Map_1_Key_54 : aliased constant String := "token";
   Map_1_Key_55 : aliased constant String := "width";
   Map_1_Key_56 : aliased constant String := "no-width";
   Map_1_Keys : constant array (0 .. 56) of access constant String
     := (Map_1_Key_0'Access,
         Map_1_Key_1'Access,
         Map_1_Key_2'Access,
         Map_1_Key_3'Access,
         Map_1_Key_4'Access,
         Map_1_Key_5'Access,
         Map_1_Key_6'Access,
         Map_1_Key_7'Access,
         Map_1_Key_8'Access,
         Map_1_Key_9'Access,
         Map_1_Key_10'Access,
         Map_1_Key_11'Access,
         Map_1_Key_12'Access,
         Map_1_Key_13'Access,
         Map_1_Key_14'Access,
         Map_1_Key_15'Access,
         Map_1_Key_16'Access,
         Map_1_Key_17'Access,
         Map_1_Key_18'Access,
         Map_1_Key_19'Access,
         Map_1_Key_20'Access,
         Map_1_Key_21'Access,
         Map_1_Key_22'Access,
         Map_1_Key_23'Access,
         Map_1_Key_24'Access,
         Map_1_Key_25'Access,
         Map_1_Key_26'Access,
         Map_1_Key_27'Access,
         Map_1_Key_28'Access,
         Map_1_Key_29'Access,
         Map_1_Key_30'Access,
         Map_1_Key_31'Access,
         Map_1_Key_32'Access,
         Map_1_Key_33'Access,
         Map_1_Key_34'Access,
         Map_1_Key_35'Access,
         Map_1_Key_36'Access,
         Map_1_Key_37'Access,
         Map_1_Key_38'Access,
         Map_1_Key_39'Access,
         Map_1_Key_40'Access,
         Map_1_Key_41'Access,
         Map_1_Key_42'Access,
         Map_1_Key_43'Access,
         Map_1_Key_44'Access,
         Map_1_Key_45'Access,
         Map_1_Key_46'Access,
         Map_1_Key_47'Access,
         Map_1_Key_48'Access,
         Map_1_Key_49'Access,
         Map_1_Key_50'Access,
         Map_1_Key_51'Access,
         Map_1_Key_52'Access,
         Map_1_Key_53'Access,
         Map_1_Key_54'Access,
         Map_1_Key_55'Access,
         Map_1_Key_56'Access);
   Map_1_Elements : constant array (0 .. 56) of Main_Command
     := (Set_Char_Encoding,
         Set_Char_Encoding,
         Set_Char_Encoding,
         Set_Char_Encoding,
         Set_Char_Encoding,
         Set_Char_Encoding,
         Set_Char_Encoding,
         Set_Char_Encoding,
         Set_Char_Encoding,
         Set_Char_Encoding,
         Set_Fallback,
         Set_Fallback,
         Set_Fallback,
         Set_Fallback,
         Set_Fallback,
         Set_Fallback,
         Set_Fallback,
         Set_Fallback,
         Set_Fallback,
         Set_Fallback,
         Set_Hex_Casing,
         Set_Hex_Casing,
         Set_Hex_Casing,
         Set_Hex_Casing,
         Set_Indentation,
         Set_Indentation,
         Set_Indentation,
         Set_Indentation,
         Set_Newline,
         Set_Newline_Encoding,
         Set_Newline_Encoding,
         Set_Newline_Encoding,
         Set_Newline_Encoding,
         Set_Newline_Encoding,
         Set_Newline_Encoding,
         Set_Newline_Encoding,
         Set_Newline_Encoding,
         Set_Newline_Encoding,
         Set_Newline_Encoding,
         Set_Newline_Encoding,
         Set_Newline_Encoding,
         Set_Quoted,
         Set_Quoted,
         Set_Quoted,
         Set_Quoted,
         Set_Quoted,
         Set_Quoted,
         Set_Quoted_String,
         Set_Quoted_String,
         Set_Space_At,
         Set_Tab_Stop,
         Set_Token,
         Set_Token,
         Set_Token,
         Set_Token,
         Set_Width,
         Set_Width);

   Map_2_Key_0 : aliased constant String := "cr";
   Map_2_Key_1 : aliased constant String := "CR";
   Map_2_Key_2 : aliased constant String := "lf";
   Map_2_Key_3 : aliased constant String := "LF";
   Map_2_Key_4 : aliased constant String := "CRLF";
   Map_2_Key_5 : aliased constant String := "CR-LF";
   Map_2_Key_6 : aliased constant String := "crlf";
   Map_2_Key_7 : aliased constant String := "cr-lf";
   Map_2_Key_8 : aliased constant String := "lf-cr";
   Map_2_Key_9 : aliased constant String := "lfcr";
   Map_2_Key_10 : aliased constant String := "LF-CR";
   Map_2_Key_11 : aliased constant String := "LFCR";
   Map_2_Key_12 : aliased constant String := "all";
   Map_2_Key_13 : aliased constant String := "none";
   Map_2_Key_14 : aliased constant String := "not";
   Map_2_Key_15 : aliased constant String := "open-open";
   Map_2_Key_16 : aliased constant String := "open-atom";
   Map_2_Key_17 : aliased constant String := "open-close";
   Map_2_Key_18 : aliased constant String := "atom-open";
   Map_2_Key_19 : aliased constant String := "atom-atom";
   Map_2_Key_20 : aliased constant String := "atom-close";
   Map_2_Key_21 : aliased constant String := "close-open";
   Map_2_Key_22 : aliased constant String := "close-atom";
   Map_2_Key_23 : aliased constant String := "close-close";
   Map_2_Keys : constant array (0 .. 23) of access constant String
     := (Map_2_Key_0'Access,
         Map_2_Key_1'Access,
         Map_2_Key_2'Access,
         Map_2_Key_3'Access,
         Map_2_Key_4'Access,
         Map_2_Key_5'Access,
         Map_2_Key_6'Access,
         Map_2_Key_7'Access,
         Map_2_Key_8'Access,
         Map_2_Key_9'Access,
         Map_2_Key_10'Access,
         Map_2_Key_11'Access,
         Map_2_Key_12'Access,
         Map_2_Key_13'Access,
         Map_2_Key_14'Access,
         Map_2_Key_15'Access,
         Map_2_Key_16'Access,
         Map_2_Key_17'Access,
         Map_2_Key_18'Access,
         Map_2_Key_19'Access,
         Map_2_Key_20'Access,
         Map_2_Key_21'Access,
         Map_2_Key_22'Access,
         Map_2_Key_23'Access);
   Map_2_Elements : constant array (0 .. 23) of Newline_Command
     := (Set_Newline_Command_Encoding,
         Set_Newline_Command_Encoding,
         Set_Newline_Command_Encoding,
         Set_Newline_Command_Encoding,
         Set_Newline_Command_Encoding,
         Set_Newline_Command_Encoding,
         Set_Newline_Command_Encoding,
         Set_Newline_Command_Encoding,
         Set_Newline_Command_Encoding,
         Set_Newline_Command_Encoding,
         Set_Newline_Command_Encoding,
         Set_Newline_Command_Encoding,
         Set_Newline_Separator,
         Set_Newline_Separator,
         Set_Newline_Separator,
         Set_Newline_Separator,
         Set_Newline_Separator,
         Set_Newline_Separator,
         Set_Newline_Separator,
         Set_Newline_Separator,
         Set_Newline_Separator,
         Set_Newline_Separator,
         Set_Newline_Separator,
         Set_Newline_Separator);

   Map_3_Key_0 : aliased constant String := "never";
   Map_3_Key_1 : aliased constant String := "single-line";
   Map_3_Key_2 : aliased constant String := "when-shorter";
   Map_3_Key_3 : aliased constant String := "octal";
   Map_3_Key_4 : aliased constant String := "hex";
   Map_3_Key_5 : aliased constant String := "hexa";
   Map_3_Key_6 : aliased constant String := "hexadecimal";
   Map_3_Key_7 : aliased constant String := "lower-hex";
   Map_3_Key_8 : aliased constant String := "lower-hexa";
   Map_3_Key_9 : aliased constant String := "upper-hex";
   Map_3_Key_10 : aliased constant String := "upper-hexa";
   Map_3_Keys : constant array (0 .. 10) of access constant String
     := (Map_3_Key_0'Access,
         Map_3_Key_1'Access,
         Map_3_Key_2'Access,
         Map_3_Key_3'Access,
         Map_3_Key_4'Access,
         Map_3_Key_5'Access,
         Map_3_Key_6'Access,
         Map_3_Key_7'Access,
         Map_3_Key_8'Access,
         Map_3_Key_9'Access,
         Map_3_Key_10'Access);
   Map_3_Elements : constant array (0 .. 10) of Quoted_String_Command
     := (Set_Quoted_Option,
         Set_Quoted_Option,
         Set_Quoted_Option,
         Set_Quoted_Escape,
         Set_Quoted_Escape,
         Set_Quoted_Escape,
         Set_Quoted_Escape,
         Set_Quoted_Escape,
         Set_Quoted_Escape,
         Set_Quoted_Escape,
         Set_Quoted_Escape);

   Map_4_Key_0 : aliased constant String := "all";
   Map_4_Key_1 : aliased constant String := "none";
   Map_4_Key_2 : aliased constant String := "not";
   Map_4_Key_3 : aliased constant String := "open-open";
   Map_4_Key_4 : aliased constant String := "open-atom";
   Map_4_Key_5 : aliased constant String := "open-close";
   Map_4_Key_6 : aliased constant String := "atom-open";
   Map_4_Key_7 : aliased constant String := "atom-atom";
   Map_4_Key_8 : aliased constant String := "atom-close";
   Map_4_Key_9 : aliased constant String := "close-open";
   Map_4_Key_10 : aliased constant String := "close-atom";
   Map_4_Key_11 : aliased constant String := "close-close";
   Map_4_Keys : constant array (0 .. 11) of access constant String
     := (Map_4_Key_0'Access,
         Map_4_Key_1'Access,
         Map_4_Key_2'Access,
         Map_4_Key_3'Access,
         Map_4_Key_4'Access,
         Map_4_Key_5'Access,
         Map_4_Key_6'Access,
         Map_4_Key_7'Access,
         Map_4_Key_8'Access,
         Map_4_Key_9'Access,
         Map_4_Key_10'Access,
         Map_4_Key_11'Access);
   Map_4_Elements : constant array (0 .. 11) of Separator_Command
     := (All_Separators,
         No_Separators,
         Invert_Separators,
         Open_Open,
         Open_Atom,
         Open_Close,
         Atom_Open,
         Atom_Atom,
         Atom_Close,
         Close_Open,
         Close_Atom,
         Close_Close);

   Map_5_Key_0 : aliased constant String := "base64";
   Map_5_Key_1 : aliased constant String := "base-64";
   Map_5_Key_2 : aliased constant String := "lower-hex";
   Map_5_Key_3 : aliased constant String := "lower-hexa";
   Map_5_Key_4 : aliased constant String := "hex";
   Map_5_Key_5 : aliased constant String := "hexa";
   Map_5_Key_6 : aliased constant String := "hexadecimal";
   Map_5_Key_7 : aliased constant String := "upper-hex";
   Map_5_Key_8 : aliased constant String := "upper-hexa";
   Map_5_Key_9 : aliased constant String := "verbatim";
   Map_5_Keys : constant array (0 .. 9) of access constant String
     := (Map_5_Key_0'Access,
         Map_5_Key_1'Access,
         Map_5_Key_2'Access,
         Map_5_Key_3'Access,
         Map_5_Key_4'Access,
         Map_5_Key_5'Access,
         Map_5_Key_6'Access,
         Map_5_Key_7'Access,
         Map_5_Key_8'Access,
         Map_5_Key_9'Access);
   Map_5_Elements : constant array (0 .. 9) of Atom_Encoding
     := (Base64,
         Base64,
         Hexadecimal,
         Hexadecimal,
         Hexadecimal,
         Hexadecimal,
         Hexadecimal,
         Hexadecimal,
         Hexadecimal,
         Verbatim);

   Map_6_Key_0 : aliased constant String := "ascii";
   Map_6_Key_1 : aliased constant String := "ASCII";
   Map_6_Key_2 : aliased constant String := "latin-1";
   Map_6_Key_3 : aliased constant String := "latin";
   Map_6_Key_4 : aliased constant String := "iso-8859-1";
   Map_6_Key_5 : aliased constant String := "ISO-8859-1";
   Map_6_Key_6 : aliased constant String := "utf-8";
   Map_6_Key_7 : aliased constant String := "UTF-8";
   Map_6_Key_8 : aliased constant String := "utf8";
   Map_6_Key_9 : aliased constant String := "UTF8";
   Map_6_Keys : constant array (0 .. 9) of access constant String
     := (Map_6_Key_0'Access,
         Map_6_Key_1'Access,
         Map_6_Key_2'Access,
         Map_6_Key_3'Access,
         Map_6_Key_4'Access,
         Map_6_Key_5'Access,
         Map_6_Key_6'Access,
         Map_6_Key_7'Access,
         Map_6_Key_8'Access,
         Map_6_Key_9'Access);
   Map_6_Elements : constant array (0 .. 9) of Character_Encoding
     := (ASCII,
         ASCII,
         Latin,
         Latin,
         Latin,
         Latin,
         UTF_8,
         UTF_8,
         UTF_8,
         UTF_8);

   Map_7_Key_0 : aliased constant String := "lower";
   Map_7_Key_1 : aliased constant String := "lower-case";
   Map_7_Key_2 : aliased constant String := "upper";
   Map_7_Key_3 : aliased constant String := "upper-case";
   Map_7_Keys : constant array (0 .. 3) of access constant String
     := (Map_7_Key_0'Access,
         Map_7_Key_1'Access,
         Map_7_Key_2'Access,
         Map_7_Key_3'Access);
   Map_7_Elements : constant array (0 .. 3) of Encodings.Hex_Casing
     := (Encodings.Lower,
         Encodings.Lower,
         Encodings.Upper,
         Encodings.Upper);

   Map_8_Key_0 : aliased constant String := "CR";
   Map_8_Key_1 : aliased constant String := "cr";
   Map_8_Key_2 : aliased constant String := "LF";
   Map_8_Key_3 : aliased constant String := "lf";
   Map_8_Key_4 : aliased constant String := "CRLF";
   Map_8_Key_5 : aliased constant String := "CR-LF";
   Map_8_Key_6 : aliased constant String := "crlf";
   Map_8_Key_7 : aliased constant String := "cr-lf";
   Map_8_Key_8 : aliased constant String := "LFCR";
   Map_8_Key_9 : aliased constant String := "LF-CR";
   Map_8_Key_10 : aliased constant String := "lfcr";
   Map_8_Key_11 : aliased constant String := "lf-cr";
   Map_8_Keys : constant array (0 .. 11) of access constant String
     := (Map_8_Key_0'Access,
         Map_8_Key_1'Access,
         Map_8_Key_2'Access,
         Map_8_Key_3'Access,
         Map_8_Key_4'Access,
         Map_8_Key_5'Access,
         Map_8_Key_6'Access,
         Map_8_Key_7'Access,
         Map_8_Key_8'Access,
         Map_8_Key_9'Access,
         Map_8_Key_10'Access,
         Map_8_Key_11'Access);
   Map_8_Elements : constant array (0 .. 11) of Newline_Encoding
     := (CR,
         CR,
         LF,
         LF,
         CR_LF,
         CR_LF,
         CR_LF,
         CR_LF,
         LF_CR,
         LF_CR,
         LF_CR,
         LF_CR);

   Map_9_Key_0 : aliased constant String := "octal";
   Map_9_Key_1 : aliased constant String := "hex";
   Map_9_Key_2 : aliased constant String := "hexa";
   Map_9_Key_3 : aliased constant String := "hexadecimal";
   Map_9_Key_4 : aliased constant String := "lower-hex";
   Map_9_Key_5 : aliased constant String := "lower-hexa";
   Map_9_Key_6 : aliased constant String := "upper-hex";
   Map_9_Key_7 : aliased constant String := "upper-hexa";
   Map_9_Keys : constant array (0 .. 7) of access constant String
     := (Map_9_Key_0'Access,
         Map_9_Key_1'Access,
         Map_9_Key_2'Access,
         Map_9_Key_3'Access,
         Map_9_Key_4'Access,
         Map_9_Key_5'Access,
         Map_9_Key_6'Access,
         Map_9_Key_7'Access);
   Map_9_Elements : constant array (0 .. 7) of Quoted_Escape_Type
     := (Octal_Escape,
         Hex_Escape,
         Hex_Escape,
         Hex_Escape,
         Hex_Escape,
         Hex_Escape,
         Hex_Escape,
         Hex_Escape);

   Map_10_Key_0 : aliased constant String := "when-shorter";
   Map_10_Key_1 : aliased constant String := "quoted-when-shorter";
   Map_10_Key_2 : aliased constant String := "quoted-string-when-shorter";
   Map_10_Key_3 : aliased constant String := "single-line";
   Map_10_Key_4 : aliased constant String := "single-line-quoted";
   Map_10_Key_5 : aliased constant String := "single-line-quoted-string";
   Map_10_Key_6 : aliased constant String := "never";
   Map_10_Key_7 : aliased constant String := "no-quoted";
   Map_10_Key_8 : aliased constant String := "no-quoted-string";
   Map_10_Keys : constant array (0 .. 8) of access constant String
     := (Map_10_Key_0'Access,
         Map_10_Key_1'Access,
         Map_10_Key_2'Access,
         Map_10_Key_3'Access,
         Map_10_Key_4'Access,
         Map_10_Key_5'Access,
         Map_10_Key_6'Access,
         Map_10_Key_7'Access,
         Map_10_Key_8'Access);
   Map_10_Elements : constant array (0 .. 8) of Quoted_Option
     := (When_Shorter,
         When_Shorter,
         When_Shorter,
         Single_Line,
         Single_Line,
         Single_Line,
         No_Quoted,
         No_Quoted,
         No_Quoted);

   Map_11_Key_0 : aliased constant String := "extended-token";
   Map_11_Key_1 : aliased constant String := "extended";
   Map_11_Key_2 : aliased constant String := "standard-token";
   Map_11_Key_3 : aliased constant String := "token";
   Map_11_Key_4 : aliased constant String := "standard";
   Map_11_Key_5 : aliased constant String := "no-token";
   Map_11_Key_6 : aliased constant String := "no";
   Map_11_Key_7 : aliased constant String := "none";
   Map_11_Key_8 : aliased constant String := "never";
   Map_11_Keys : constant array (0 .. 8) of access constant String
     := (Map_11_Key_0'Access,
         Map_11_Key_1'Access,
         Map_11_Key_2'Access,
         Map_11_Key_3'Access,
         Map_11_Key_4'Access,
         Map_11_Key_5'Access,
         Map_11_Key_6'Access,
         Map_11_Key_7'Access,
         Map_11_Key_8'Access);
   Map_11_Elements : constant array (0 .. 8) of Token_Option
     := (Extended_Token,
         Extended_Token,
         Standard_Token,
         Standard_Token,
         Standard_Token,
         No_Token,
         No_Token,
         No_Token,
         No_Token);

end Natools.S_Expressions.Printers.Pretty.Config.Commands;
