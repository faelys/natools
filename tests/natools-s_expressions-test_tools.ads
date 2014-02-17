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

------------------------------------------------------------------------------
-- Natools.S_Expressions.Test_Tools provides tools used in S-expression     --
-- test suites.                                                             --
-- Memory_Stream is a stream implementation around a memory buffer where    --
-- written data can be subsequently read. A secondary buffer of expected    --
-- data can be optionally used, and the mismatch marker is set when written --
-- data does not match expected data.                                       --
------------------------------------------------------------------------------

with Ada.Streams;

with Natools.Tests;

with Natools.S_Expressions.Atom_Buffers;

package Natools.S_Expressions.Test_Tools is
   pragma Preelaborate (Test_Tools);

   package NT renames Natools.Tests;

   procedure Dump_Atom
     (Report : in out NT.Reporter'Class;
      Data : in Atom;
      Label : in String := "");
   procedure Dump_Atom
     (Test : in out NT.Test;
      Data : in Atom;
      Label : in String := "");
      --  Dump contents on Data as info in Report

   procedure Test_Atom
     (Report : in out NT.Reporter'Class;
      Test_Name : in String;
      Expected : in Atom;
      Found : in Atom);
   procedure Test_Atom
     (Test : in out NT.Test;
      Expected : in Atom;
      Found : in Atom);
      --  Report success when Found is equal to Expected, and failure
      --  with diagnostics otherwise.

   procedure Test_Atom_Accessors
     (Test : in out NT.Test;
      Tested : in Descriptor'Class;
      Expected : in Atom;
      Expected_Level : in Integer := -1);
      --  Test all the ways of accessing atom in Tested


   type Memory_Stream is new Ada.Streams.Root_Stream_Type with private;

   overriding procedure Read
     (Stream : in out Memory_Stream;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset);
      --  Consume data from the beginning of internal buffer

   overriding procedure Write
     (Stream : in out Memory_Stream;
      Item : in Ada.Streams.Stream_Element_Array);
      --  Append data at the end of internal buffer

   function Get_Data (Stream : Memory_Stream) return Atom;
      --  Return internal buffer

   function Unread_Data (Stream : Memory_Stream) return Atom;
      --  Return part of internal buffer that has not yet been read

   procedure Set_Data
     (Stream : in out Memory_Stream;
      Data : in Atom);
      --  Replace whole internal buffer with Data

   function Unread_Expected (Stream : Memory_Stream) return Atom;
      --  Return part of expected buffer that has not been matched yet

   procedure Set_Expected
     (Stream : in out Memory_Stream;
      Data : in Atom;
      Reset_Mismatch : in Boolean := True);
      --  Replace buffer of expected data

   function Has_Mismatch (Stream : Memory_Stream) return Boolean;
   procedure Reset_Mismatch (Stream : in out Memory_Stream);
      --  Accessor and mutator of the mismatch flag

   function Mismatch_Index (Stream : Memory_Stream) return Count;
      --  Return the position of the first mismatching octet,
      --  or 0 when there has been no mismatch.

   procedure Check_Stream
     (Stream : in Test_Tools.Memory_Stream;
      Test : in out NT.Test);
      --  On error in Stream, report error and dump relevant information.

private

   type Memory_Stream is new Ada.Streams.Root_Stream_Type with record
      Internal : Atom_Buffers.Atom_Buffer;
      Expected : Atom_Buffers.Atom_Buffer;
      Read_Pointer : Count := 0;
      Expect_Pointer : Count := 0;
      Mismatch : Boolean := False;
   end record;

end Natools.S_Expressions.Test_Tools;
