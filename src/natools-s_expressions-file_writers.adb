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

package body Natools.S_Expressions.File_Writers is

   package Stream_IO renames Ada.Streams.Stream_IO;


   overriding procedure Finalize (Object : in out Autoclose) is
   begin
      if Stream_IO.Is_Open (Object.File) then
         Stream_IO.Close (Object.File);
      end if;
   end Finalize;


   -------------------------
   -- S-Expression Writer --
   -------------------------

   overriding procedure Write_Raw
     (Output : in out Writer;
      Data : in Ada.Streams.Stream_Element_Array) is
   begin
      Stream_IO.Write (Output.Holder.File, Data);
   end Write_Raw;


   function Create (Name : String; Form : String := "") return Writer is
   begin
      return Result : Writer do
         Create (Result, Name, Form);
      end return;
   end Create;


   function Open (Name : String; Form : String := "") return Writer is
   begin
      return Result : Writer do
         Open (Result, Name, Form);
      end return;
   end Open;


   procedure Create
     (Self : in out Writer;
      Name : in String;
      Form : in String := "") is
   begin
      Finalize (Self.Holder);
      Stream_IO.Create (Self.Holder.File, Stream_IO.Append_File, Name, Form);
   end Create;


   procedure Open
     (Self : in out Writer;
      Name : in String;
      Form : in String := "") is
   begin
      Finalize (Self.Holder);
      Stream_IO.Open (Self.Holder.File, Stream_IO.Append_File, Name, Form);
   end Open;


   function Name (Self : Writer) return String is
   begin
      return Stream_IO.Name (Self.Holder.File);
   end Name;

end Natools.S_Expressions.File_Writers;
