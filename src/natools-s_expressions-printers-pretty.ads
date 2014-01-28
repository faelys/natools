------------------------------------------------------------------------------
-- Copyright (c) 2013-2014, Natacha Porté                                   --
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

------------------------------------------------------------------------------
-- Natools.S_Expressions.Printers.Pretty provides an implementation of the  --
-- S-expression Printer interface, using user-defines perferences to pretty --
-- print the S-expression into the output stream.                           --
------------------------------------------------------------------------------

with Ada.Streams;

with Natools.S_Expressions.Encodings;

package Natools.S_Expressions.Printers.Pretty is
   pragma Pure (Pretty);

   type Atom_Encoding is (Base64, Hexadecimal, Verbatim);
   type Character_Encoding is (ASCII, Latin, UTF_8);
   type Newline_Encoding is (CR, LF, CR_LF, LF_CR);
   type Entity is (Opening, Atom_Data, Closing);
   type Entity_Separator is array (Entity, Entity) of Boolean;
   type Indent_Type is (Spaces, Tabs, Tabs_And_Spaces);
   type Quoted_Escape_Type is (Hex_Escape, Octal_Escape);
   type Quoted_Option is (When_Shorter, Single_Line, No_Quoted);
   type Token_Option is (Extended_Token, Standard_Token, No_Token);
   type Screen_Offset is new Natural;

   subtype Screen_Column is Screen_Offset range 1 .. Screen_Offset'Last;

   type Parameters is record
      Width         : Screen_Offset;
      Newline_At    : Entity_Separator;
      Space_At      : Entity_Separator;
      Tab_Stop      : Screen_Column;
      Indentation   : Screen_Offset;
      Indent        : Indent_Type;
      Quoted        : Quoted_Option;
      Token         : Token_Option;
      Hex_Casing    : Encodings.Hex_Casing;
      Quoted_Escape : Quoted_Escape_Type;
      Char_Encoding : Character_Encoding;
      Fallback      : Atom_Encoding;
      Newline       : Newline_Encoding;
   end record;

   Canonical : constant Parameters;

   type Printer (Stream : access Ada.Streams.Root_Stream_Type'Class) is
     new Printers.Printer with private;

   overriding procedure Open_List (Output : in out Printer);
   overriding procedure Append_Atom
     (Output : in out Printer;
      Data : in Atom);
   overriding procedure Close_List (Output : in out Printer);

   procedure Set_Parameters (Output : in out Printer; Param : in Parameters);
   function Get_Parameters (Output : Printer) return Parameters;

   procedure Set_Width
     (Output : in out Printer;
      Width : in Screen_Offset);
   procedure Set_Newline_At
     (Output : in out Printer;
      Newline_At : in Entity_Separator);
   procedure Set_Space_At
     (Output : in out Printer;
      Space_At : in Entity_Separator);
   procedure Set_Tab_Stop
     (Output : in out Printer;
      Tab_Stop : in Screen_Column);
   procedure Set_Indentation
     (Output : in out Printer;
      Indentation : in Screen_Offset);
   procedure Set_Indent
     (Output : in out Printer;
      Indent : in Indent_Type);
   procedure Set_Quoted
     (Output : in out Printer;
      Quoted : in Quoted_Option);
   procedure Set_Token
     (Output : in out Printer;
      Token : in Token_Option);
   procedure Set_Hex_Casing
     (Output : in out Printer;
      Hex_Casing : in Encodings.Hex_Casing);
   procedure Set_Quoted_Escape
     (Output : in out Printer;
      Quoted_Escape : in Quoted_Escape_Type);
   procedure Set_Char_Encoding
     (Output : in out Printer;
      Char_Encoding : in Character_Encoding);
   procedure Set_Fallback
     (Output : in out Printer;
      Fallback : in Atom_Encoding);
   procedure Set_Newline
     (Output : in out Printer;
      Newline : in Newline_Encoding);

private

   type Printer (Stream : access Ada.Streams.Root_Stream_Type'Class) is
     new Printers.Printer with record
      Param        : Parameters;
      Cursor       : Screen_Column := 1;
      Previous     : Entity;
      First        : Boolean := True;
      Indent_Level : Screen_Offset := 0;
      Need_Blank   : Boolean := False;
   end record;

   Canonical : constant Parameters :=
     (Width         => 0,
      Newline_At    => (others => (others => False)),
      Space_At      => (others => (others => False)),
      Tab_Stop      => 8,  --  unused
      Indentation   => 0,
      Indent        => Spaces,  --  unused
      Quoted        => No_Quoted,
      Token         => No_Token,
      Hex_Casing    => Encodings.Upper,  --  unused
      Quoted_Escape => Octal_Escape,  --  unused
      Char_Encoding => ASCII,  --  unused
      Fallback      => Verbatim,
      Newline       => LF);  --  unused

end Natools.S_Expressions.Printers.Pretty;