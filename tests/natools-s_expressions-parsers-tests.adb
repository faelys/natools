------------------------------------------------------------------------------
-- Copyright (c) 2014-2017, Natacha Porté                                   --
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

with Natools.S_Expressions.Lockable.Tests;
with Natools.S_Expressions.Printers;
with Natools.S_Expressions.Test_Tools;

package body Natools.S_Expressions.Parsers.Tests is

   generic
      Name : String;
      Source, Expected : Atom;
   procedure Blackbox_Test (Report : in out NT.Reporter'Class);
      --  Perform a simple blackbox test, feeding Source to a new parser
      --  plugged on a canonical printer and comparing with Expected.



   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

   procedure Blackbox_Test (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item (Name);
   begin
      declare
         Input, Output : aliased Test_Tools.Memory_Stream;
         Printer : Printers.Canonical (Output'Access);
         Parser : Parsers.Stream_Parser (Input'Access);
      begin
         Output.Set_Expected (Expected);
         Input.Set_Data (Source);
         Parser.Next;
         Printers.Transfer (Parser, Printer);
         Output.Check_Stream (Test);
      end;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Blackbox_Test;



   -------------------------
   -- Complete Test Suite --
   -------------------------

   procedure All_Tests (Report : in out NT.Reporter'Class) is
   begin
      Canonical_Encoding (Report);
      Atom_Encodings (Report);
      Base64_Subexpression (Report);
      Special_Subexpression (Report);
      Nested_Subpexression (Report);
      Number_Prefixes (Report);
      Quoted_Escapes (Report);
      Lockable_Interface (Report);
      Reset (Report);
      Locked_Next (Report);
      Memory_Parser (Report);
   end All_Tests;



   -----------------------
   -- Inidividual Tests --
   -----------------------

   procedure Atom_Encodings (Report : in out NT.Reporter'Class) is
      procedure Test is new Blackbox_Test
        (Name => "Basic atom encodings",
         Source => To_Atom ("17:Verbatim encoding"
           & """Quoted\040string"""
           & "#48657861646563696d616c2064756d70#"
           & "token "
           & "|QmFzZS02NCBlbmNvZGluZw==|"),
         Expected => To_Atom ("17:Verbatim encoding"
           & "13:Quoted string"
           & "16:Hexadecimal dump"
           & "5:token"
           & "16:Base-64 encoding"));
   begin
      Test (Report);
   end Atom_Encodings;


   procedure Canonical_Encoding (Report : in out NT.Reporter'Class) is
      Sample_Image : constant String
        := "3:The(5:quick((5:brown3:fox)5:jumps))9:over3:the()4:lazy0:3:dog";

      procedure Test is new Blackbox_Test
        (Name => "Canonical encoding",
         Source => To_Atom (Sample_Image),
         Expected => To_Atom (Sample_Image));
   begin
      Test (Report);
   end Canonical_Encoding;


   procedure Base64_Subexpression (Report : in out NT.Reporter'Class) is
      procedure Test is new Blackbox_Test
        (Name => "Base-64 subexpression",
         Source => To_Atom ("head({KDc6c3VibGlzdCk1OnRva2Vu})""tail"""),
         Expected => To_Atom ("4:head((7:sublist)5:token)4:tail"));
   begin
      Test (Report);
   end Base64_Subexpression;


   procedure Lockable_Interface (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Lockable.Descriptor interface");
   begin
      declare
         Input : aliased Test_Tools.Memory_Stream;
         Parser : Parsers.Stream_Parser (Input'Access);
      begin
         Input.Set_Data (Lockable.Tests.Test_Expression);
         Test_Tools.Next_And_Check (Test, Parser, Events.Open_List, 1);
         Lockable.Tests.Test_Interface (Test, Parser);
      end;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Lockable_Interface;


   procedure Locked_Next (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Next on locked parser");
   begin
      declare
         Input : aliased Test_Tools.Memory_Stream;
         Parser : Parsers.Stream_Parser (Input'Access);
         Lock_State : Lockable.Lock_State;
      begin
         Input.Set_Data (To_Atom ("(command (subcommand arg (arg list)))0:"));
         Test_Tools.Next_And_Check (Test, Parser, Events.Open_List, 1);
         Test_Tools.Next_And_Check (Test, Parser, To_Atom ("command"), 1);
         Test_Tools.Next_And_Check (Test, Parser, Events.Open_List, 2);
         Test_Tools.Next_And_Check (Test, Parser, To_Atom ("subcommand"), 2);
         Parser.Lock (Lock_State);
         Test_Tools.Test_Atom_Accessors
           (Test, Parser, To_Atom ("subcommand"), 0);
         Test_Tools.Next_And_Check (Test, Parser, To_Atom ("arg"), 0);
         Test_Tools.Next_And_Check (Test, Parser, Events.Open_List, 1);
         Test_Tools.Next_And_Check (Test, Parser, To_Atom ("arg"), 1);
         Test_Tools.Next_And_Check (Test, Parser, To_Atom ("list"), 1);
         Test_Tools.Next_And_Check (Test, Parser, Events.Close_List, 0);
         Test_Tools.Next_And_Check (Test, Parser, Events.End_Of_Input, 0);
         Test_Tools.Next_And_Check (Test, Parser, Events.End_Of_Input, 0);
         Test_Tools.Next_And_Check (Test, Parser, Events.End_Of_Input, 0);
         Parser.Unlock (Lock_State);
         Test_Tools.Next_And_Check (Test, Parser, Events.Close_List, 0);
         Test_Tools.Next_And_Check (Test, Parser, Null_Atom, 0);
      end;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Locked_Next;


   procedure Memory_Parser (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Memory-backed parser");
   begin
      declare
         Parser : Parsers.Memory_Parser
           := Create_From_String ("(command (subcommand arg (arg list)))0:");
      begin
         Test_Tools.Next_And_Check (Test, Parser, Events.Open_List, 1);
         Test_Tools.Next_And_Check (Test, Parser, To_Atom ("command"), 1);
         Test_Tools.Next_And_Check (Test, Parser, Events.Open_List, 2);
         Test_Tools.Next_And_Check (Test, Parser, To_Atom ("subcommand"), 2);
         Test_Tools.Next_And_Check (Test, Parser, To_Atom ("arg"), 2);
         Test_Tools.Next_And_Check (Test, Parser, Events.Open_List, 3);
         Test_Tools.Next_And_Check (Test, Parser, To_Atom ("arg"), 3);
         Test_Tools.Next_And_Check (Test, Parser, To_Atom ("list"), 3);
         Test_Tools.Next_And_Check (Test, Parser, Events.Close_List, 2);
         Test_Tools.Next_And_Check (Test, Parser, Events.Close_List, 1);
         Test_Tools.Next_And_Check (Test, Parser, Events.Close_List, 0);
         Test_Tools.Next_And_Check (Test, Parser, Null_Atom, 0);
         Test_Tools.Next_And_Check (Test, Parser, Events.End_Of_Input, 0);
         Test_Tools.Next_And_Check (Test, Parser, Events.End_Of_Input, 0);
      end;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Memory_Parser;


   procedure Nested_Subpexression (Report : in out NT.Reporter'Class) is
      procedure Test is new Blackbox_Test
        (Name => "Nested base-64 subepxressions",
         Source => To_Atom ("(5:begin"
           & "{KG5lc3RlZCB7S0dSbFpYQWdjR0Y1Ykc5aFpDaz19KQ==}"
           & "end)"),
         Expected => To_Atom ("(5:begin"
           & "(6:nested(4:deep7:payload))"
           & "3:end)"));
   begin
      Test (Report);
   end Nested_Subpexression;


   procedure Number_Prefixes (Report : in out NT.Reporter'Class) is
      procedure Test is new Blackbox_Test
        (Name => "Number prefixes",
         Source => To_Atom ("8:verbatim"
           & "(valid 6""quoted"" 11#68657861646563696d616c#"
           & " 7|YmFzZS02NA==| 9{NzpleHByLTY0})"
           & "(undefined 42 10% 123() 10)"
           & "(invalid 10""quoted"" 3#68657861646563696d616c#"
           & " 75|YmFzZS02NA==| 1{NzpleHByLTY0})"),
         Expected => To_Atom ("8:verbatim"
           & "(5:valid6:quoted11:hexadecimal7:base-647:expr-64)"
           & "(9:undefined2:423:10%3:123()2:10)"
           & "(7:invalid6:quoted11:hexadecimal7:base-647:expr-64)"));
   begin
      Test (Report);
   end Number_Prefixes;


   procedure Quoted_Escapes (Report : in out NT.Reporter'Class) is
      CR : constant Character := Character'Val (13);
      LF : constant Character := Character'Val (10);

      procedure Test is new Blackbox_Test
        (Name => "Escapes in quoted encoding",
         Source => To_Atom ("(single-letters ""\b\t\n\v\f\r\\\k"")"
           & "(newlines ""head\" & CR & "tail"" ""head\" & LF & "tail"""
           & " ""head\" & CR & LF & "tail"" ""head\" & LF & CR & "tail"")"
           & "(octal ""head\040\04\xtail"")"
           & "(hexadecimal ""head\x20\x2a\x2D\x2gtail"")"
           & "(special ""\x""1:"")"),
         Expected => To_Atom ("(14:single-letters9:"
           & Character'Val (8) & Character'Val (9)
           & Character'Val (10) & Character'Val (11)
           & Character'Val (12) & Character'Val (13)
           & "\\k)"
           & "(8:newlines8:headtail8:headtail8:headtail8:headtail)"
           & "(5:octal14:head \04\xtail)"
           & "(11:hexadecimal15:head *-\x2gtail)"
           & "(7:special2:\x1:"")"));
   begin
      Test (Report);
   end Quoted_Escapes;


   procedure Reset (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Parser reset");
   begin
      declare
         Input : aliased Test_Tools.Memory_Stream;
         Parser : Parsers.Stream_Parser (Input'Access);
         Empty : Parsers.Stream_Parser (Input'Access);

         use type Atom_Buffers.Atom_Buffer;
         use type Lockable.Lock_Stack;
      begin
         Input.Write (To_Atom ("(begin(first second"));
         Test_Tools.Next_And_Check (Test, Parser, Events.Open_List, 1);
         Test_Tools.Next_And_Check (Test, Parser, To_Atom ("begin"), 1);
         Test_Tools.Next_And_Check (Test, Parser, Events.Open_List, 2);
         Test_Tools.Next_And_Check (Test, Parser, To_Atom ("first"), 2);
         Test_Tools.Next_And_Check (Test, Parser, Events.End_Of_Input, 2);
         Parser.Reset (Hard => False);
         Input.Write (To_Atom ("other(new list)end"));
         Test_Tools.Next_And_Check (Test, Parser, To_Atom ("other"), 0);
         Test_Tools.Next_And_Check (Test, Parser, Events.Open_List, 1);
         Test_Tools.Next_And_Check (Test, Parser, To_Atom ("new"), 1);
         Test_Tools.Next_And_Check (Test, Parser, To_Atom ("list"), 1);
         Test_Tools.Next_And_Check (Test, Parser, Events.Close_List, 0);
         Parser.Reset (Hard => True);

         if Parser.Internal /= Empty.Internal
           or else Parser.Next_Event /= Empty.Next_Event
           or else Parser.Latest /= Empty.Latest
           or else Parser.Pending.Capacity /= 0
           or else Parser.Buffer.Capacity /= 0
           or else Parser.Level /= Empty.Level
           or else Parser.Lock_Stack /= Empty.Lock_Stack
           or else Parser.Locked /= Empty.Locked
         then
            Test.Fail ("Parser after hard reset is not empty");
         end if;
      end;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Reset;


   procedure Special_Subexpression (Report : in out NT.Reporter'Class) is
      procedure Test is new Blackbox_Test
        (Name => "Special base-64 subexpression",
         Source => To_Atom ("(begin "
           & "{aGlkZGVuLWVuZCkoaGlkZGVuLWJlZ2lu}"
           & " end)"
           & "({MTY6b3ZlcmZsb3dpbmc=} atom)"),
         Expected => To_Atom ("(5:begin"
           & "10:hidden-end)(12:hidden-begin"
           & "3:end)"
           & "(16:overflowing atom)"));
   begin
      Test (Report);
   end Special_Subexpression;

end Natools.S_Expressions.Parsers.Tests;
