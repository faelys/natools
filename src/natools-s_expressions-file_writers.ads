------------------------------------------------------------------------------
-- Copyright (c) 2014, Natacha Porté                                        --
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
-- Natools.S_Expressions.File_Writers provides a pretty printer             --
-- implementation using Stream_IO as backend.                               --
------------------------------------------------------------------------------

with Natools.S_Expressions.Printers.Pretty;

private with Ada.Finalization;
private with Ada.Streams.Stream_IO;

package Natools.S_Expressions.File_Writers is

   type Writer is limited new Printers.Pretty.Printer with private;

   function Create (Name : String; Form : String := "") return Writer;
   function Open (Name : String; Form : String := "") return Writer;
      --  Constructors using respectively Stream_IO.Create and Stream_IO.Open

   procedure Create
     (Self : in out Writer;
      Name : in String;
      Form : in String := "");
   procedure Open
     (Self : in out Writer;
      Name : in String;
      Form : in String := "");
      --  Reinitialize Self using Stream_IO.Create or Stream_IO.Open

   function Name (Self : Writer) return String;
      --  Return the underlying file name

private

   type Autoclose is new Ada.Finalization.Limited_Controlled with record
      File : Ada.Streams.Stream_IO.File_Type;
   end record;

   overriding procedure Finalize (Object : in out Autoclose);
      --  Close the underlying file if it was opened


   type Writer is limited new Printers.Pretty.Printer with record
      Holder : Autoclose;
   end record;

   overriding procedure Write_Raw
     (Output : in out Writer;
      Data : in Ada.Streams.Stream_Element_Array);
      --  Write data into the underlying stream

end Natools.S_Expressions.File_Writers;
