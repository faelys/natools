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

with Ada.Streams.Stream_IO;
with Natools.S_Expressions.Test_Tools;

package body Natools.S_Expressions.Enumeration_IO.Tests is

   package Stream_IO renames Ada.Streams.Stream_IO;

   package Test_IO is new Typed_IO (Stream_IO.File_Mode);


   -------------------------
   -- Complete Test Suite --
   -------------------------

   procedure All_Tests (Report : in out NT.Reporter'Class) is
   begin
      Basic_Usage (Report);
      Invalid_Atom (Report);
   end All_Tests;



   ----------------------
   -- Individual Tests --
   ----------------------

   procedure Basic_Usage (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Basic usage with Stream_IO.File_Mode");
      use type Stream_IO.File_Mode;
   begin
      Test_Tools.Test_Atom
        (Test,
         To_Atom ("append-file"),
         Test_IO.Image (Stream_IO.Append_File));

      declare
         Expected : constant Stream_IO.File_Mode := Stream_IO.Out_File;
         Found : constant Stream_IO.File_Mode
           := Test_IO.Value (To_Atom ("out-file"));
      begin
         if Expected /= Found then
            Test.Fail ("Test_IO.Value returned "
              & Stream_IO.File_Mode'Image (Found)
              & ", expected "
              & Stream_IO.File_Mode'Image (Expected));
         end if;
      end;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Basic_Usage;


   procedure Invalid_Atom (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Value on invalid atoms");
      use type Stream_IO.File_Mode;
   begin
      Without_Fallback :
      declare
         Found : Stream_IO.File_Mode;
      begin
         Found := Test_IO.Value (To_Atom ("invalid-atom"));
         Test.Fail ("Exception expected, but Value returned "
           & Stream_IO.File_Mode'Image (Found));
      exception
         when Constraint_Error => null;
      end Without_Fallback;

      With_Fallback :
      declare
         Expected : constant Stream_IO.File_Mode := Stream_IO.Out_File;
         Found : constant Stream_IO.File_Mode
           := Test_IO.Value (To_Atom ("invalid-atom"), Expected);
      begin
         if Expected /= Found then
            Test.Fail ("Test_IO.Value returned "
              & Stream_IO.File_Mode'Image (Found)
              & ", expected "
              & Stream_IO.File_Mode'Image (Expected));
         end if;
      end With_Fallback;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Invalid_Atom;

end Natools.S_Expressions.Enumeration_IO.Tests;
