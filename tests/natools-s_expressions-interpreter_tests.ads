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
-- Natools.S_Expressions.Interpreter_Tests provides a test suite for        --
-- S-expression interpreters.                                               --
------------------------------------------------------------------------------

with Natools.Tests;

private with Natools.S_Expressions.Interpreters;
private with Natools.S_Expressions.Lockable;
private with Natools.S_Expressions.Printers;

package Natools.S_Expressions.Interpreter_Tests is
   pragma Preelaborate (Interpreter_Tests);

   package NT renames Natools.Tests;

   procedure All_Tests (Report : in out NT.Reporter'Class);

   procedure Test_Basic_Usage (Report : in out NT.Reporter'Class);
   procedure Test_Exception_Fallback (Report : in out NT.Reporter'Class);
   procedure Test_Inspection (Report : in out NT.Reporter'Class);
   procedure Test_Local_Fallback (Report : in out NT.Reporter'Class);
   procedure Test_Premanent_Fallback (Report : in out NT.Reporter'Class);
   procedure Test_Unknown_Commands (Report : in out NT.Reporter'Class);

private

   package Test_Interpreters is new Natools.S_Expressions.Interpreters
     (Printers.Printer'Class, Boolean);

   type Recorder is new Test_Interpreters.Command with null record;

   overriding procedure Execute
     (Self : in Recorder;
      State : in out Printers.Printer'Class;
      Context : in Boolean;
      Name : in Atom);

   overriding procedure Execute
     (Self : in Recorder;
      State : in out Printers.Printer'Class;
      Context : in Boolean;
      Cmd : in out Lockable.Descriptor'Class);

   Special_Exception : exception;

   type Raiser is new Test_Interpreters.Command with null record;

   overriding procedure Execute
     (Self : in Raiser;
      State : in out Printers.Printer'Class;
      Context : in Boolean;
      Name : in Atom);

   overriding procedure Execute
     (Self : in Raiser;
      State : in out Printers.Printer'Class;
      Context : in Boolean;
      Cmd : in out Lockable.Descriptor'Class);

end Natools.S_Expressions.Interpreter_Tests;
