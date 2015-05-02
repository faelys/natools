------------------------------------------------------------------------------
-- Copyright (c) 2013-2015, Natacha Porté                                   --
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
-- Natools.S_Expressions.Printers mainly provides an interface for objects  --
-- that are "printer-like", which means the client somehow transmits        --
-- sequentially whatever information is needed to output a S-expression.    --
-- This contrasts with Descriptor interface, in that the latter provides an --
-- interface for S-expression from the object to its client, while Printer  --
-- interface is for S-epression transmitted from the client to its the      --
-- object.                                                                  --
--                                                                          --
-- The package also provide concrete type Canonical, which outputs the      --
-- S-expression provided through the interface into the given output        --
-- stream, using canonical encoding.                                        --
------------------------------------------------------------------------------

with Ada.Streams;

package Natools.S_Expressions.Printers is
   pragma Pure (Natools.S_Expressions.Printers);

   type Printer is limited interface;

   procedure Open_List (Output : in out Printer) is abstract;
   procedure Append_Atom (Output : in out Printer; Data : in Atom) is abstract;
   procedure Close_List (Output : in out Printer) is abstract;


   procedure Append_String (Output : in out Printer'Class; Data : in String);

   procedure Transfer
     (Source : in out Descriptor'Class;
      Target : in out Printer'Class;
      Check_Level : in Boolean := False);

   type Canonical (Stream : access Ada.Streams.Root_Stream_Type'Class) is
     new Printer with null record;

   overriding procedure Open_List (Output : in out Canonical);
   overriding procedure Append_Atom (Output : in out Canonical;
                                     Data : in Atom);
   overriding procedure Close_List (Output : in out Canonical);

end Natools.S_Expressions.Printers;
