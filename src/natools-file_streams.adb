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

package body Natools.File_Streams is

   overriding procedure Finalize (Object : in out Autoclose) is
   begin
      if Stream_IO.Is_Open (Object.Backend) then
         Stream_IO.Close (Object.Backend);
      end if;
   end Finalize;


   not overriding function Form (File : in File_Stream) return String is
   begin
      return Stream_IO.Form (File.Internal.Backend);
   end Form;


   not overriding function Create
     (Mode : in Stream_IO.File_Mode := Stream_IO.Out_File;
      Name : in String := "";
      Form : in String := "")
     return File_Stream is
   begin
      return Result : File_Stream do
         Stream_IO.Create (Result.Internal.Backend, Mode, Name, Form);
      end return;
   end Create;


   not overriding function Mode (File : in File_Stream)
     return Stream_IO.File_Mode is
   begin
      return Stream_IO.Mode (File.Internal.Backend);
   end Mode;


   not overriding function Name (File : in File_Stream) return String is
   begin
      return Stream_IO.Name (File.Internal.Backend);
   end Name;


   not overriding function Open
     (Mode : in Stream_IO.File_Mode;
      Name : in String;
      Form : in String := "")
     return File_Stream is
   begin
      return Result : File_Stream do
         Stream_IO.Open (Result.Internal.Backend, Mode, Name, Form);
      end return;
   end Open;


   overriding procedure Read
     (Stream : in out File_Stream;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset)
   is
      pragma Unmodified (Stream);
   begin
      Stream_IO.Read (Stream.Internal.Backend, Item, Last);
   end Read;


   overriding procedure Write
     (Stream : in out File_Stream;
      Item : in Ada.Streams.Stream_Element_Array)
   is
      pragma Unmodified (Stream);
   begin
      Stream_IO.Write (Stream.Internal.Backend, Item);
   end Write;

end Natools.File_Streams;
