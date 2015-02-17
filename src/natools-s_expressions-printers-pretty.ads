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
      Width         : Screen_Offset        := 0;
      Newline_At    : Entity_Separator     := (others => (others => False));
      Space_At      : Entity_Separator     := (others => (others => False));
      Tab_Stop      : Screen_Column        := 8;                --  *
      Indentation   : Screen_Offset        := 0;
      Indent        : Indent_Type          := Spaces;           --  *
      Quoted        : Quoted_Option        := No_Quoted;
      Token         : Token_Option         := No_Token;
      Hex_Casing    : Encodings.Hex_Casing := Encodings.Upper;  --  *
      Quoted_Escape : Quoted_Escape_Type   := Hex_Escape;       --  *
      Char_Encoding : Character_Encoding   := ASCII;            --  *
      Fallback      : Atom_Encoding        := Verbatim;
      Newline       : Newline_Encoding     := LF;               --  *
   end record;
      --  Default values yield canonical encoding, though fields marked with
      --  an asterisk (*) can have any value and still be canonical.

   Canonical : constant Parameters := (others => <>);

   type Printer is abstract limited new Printers.Printer with private;
   pragma Preelaborable_Initialization (Printer);

   procedure Write_Raw
     (Output : in out Printer;
      Data : in Ada.Streams.Stream_Element_Array)
     is abstract;

   overriding procedure Open_List (Output : in out Printer);
   overriding procedure Append_Atom
     (Output : in out Printer;
      Data : in Atom);
   overriding procedure Close_List (Output : in out Printer);

   procedure Newline (Output : in out Printer);
      --  Open a new indented line in the output

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


   type Stream_Printer (Stream : access Ada.Streams.Root_Stream_Type'Class) is
     limited new Printer with private;

private

   type Printer is abstract limited new Printers.Printer with record
      Param        : Parameters;
      Cursor       : Screen_Column := 1;
      Previous     : Entity;
      First        : Boolean := True;
      Indent_Level : Screen_Offset := 0;
      Need_Blank   : Boolean := False;
   end record;

   type Stream_Printer (Stream : access Ada.Streams.Root_Stream_Type'Class) is
     new Printer with null record;

   overriding procedure Write_Raw
     (Output : in out Stream_Printer;
      Data : in Ada.Streams.Stream_Element_Array);

end Natools.S_Expressions.Printers.Pretty;
