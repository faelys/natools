------------------------------------------------------------------------------
-- Copyright (c) 2015, Natacha Porté                                        --
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
-- Natools.File_Streams provides a standard stream object associated with   --
-- a file on disk. The semantics of the provided subprograms mirror those   --
-- of Ada.Streams.Stream_IO, except that the file has to be opened on       --
-- object initialization and closed on finalization.                        --
-- The current implementation does not expose most useful primitives, but   --
-- is enough for a direct use as a stream object.                           --
------------------------------------------------------------------------------

with Ada.Streams.Stream_IO;

private with Ada.Finalization;

package Natools.File_Streams is
   pragma Preelaborate;

   package Stream_IO renames Ada.Streams.Stream_IO;

   type File_Stream is limited new Ada.Streams.Root_Stream_Type with private;

   overriding procedure Read
     (Stream : in out File_Stream;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset);

   overriding procedure Write
     (Stream : in out File_Stream;
      Item : in Ada.Streams.Stream_Element_Array);

   not overriding function Create
     (Mode : in Stream_IO.File_Mode := Stream_IO.Out_File;
      Name : in String := "";
      Form : in String := "")
     return File_Stream;

   not overriding function Open
     (Mode : in Stream_IO.File_Mode;
      Name : in String;
      Form : in String := "")
     return File_Stream;

   not overriding function Mode (File : in File_Stream)
     return Stream_IO.File_Mode;

   not overriding function Name (File : in File_Stream) return String;

   not overriding function Form (File : in File_Stream) return String;

private

   type Autoclose is new Ada.Finalization.Limited_Controlled with record
      Backend : Stream_IO.File_Type;
   end record;

   overriding procedure Finalize (Object : in out Autoclose);


   type File_Stream is limited new Ada.Streams.Root_Stream_Type
   with record
      Internal : Autoclose;
   end record;

end Natools.File_Streams;
