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

with Natools.S_Expressions.Lockable.Tests;
with Natools.S_Expressions.Printers;
with Natools.S_Expressions.Test_Tools;

package body Natools.S_Expressions.Parsers.Tests is

   procedure Check_Parsing
     (Report : in out NT.Reporter'Class;
      Name : in String;
      Parser : in Parsers.Parser;
      Input, Output : in Test_Tools.Memory_Stream);
      --  Report failure or success depending on Output seeing a mismatch
      --  or having pending data. Dump stream status if needed.

   generic
      Name : String;
      Source, Expected : Atom;
   procedure Blackbox_Test (Report : in out NT.Reporter'Class);
      --  Perform a simple blackbox test, feeding Source to a new parser
      --  plugged on a canonical printer and comparing with Expected.



   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

   procedure Check_Parsing
     (Report : in out NT.Reporter'Class;
      Name : in String;
      Parser : in Parsers.Parser;
      Input, Output : in Test_Tools.Memory_Stream) is
   begin
      if Parser.Current_Event = Events.Error
        or else Output.Has_Mismatch
        or else Output.Unread_Expected /= Null_Atom
      then
         Report.Item (Name, NT.Fail);

         if Parser.Current_Event = Events.Error then
            Report.Info ("Parser in error state");
         end if;

         if Output.Has_Mismatch then
            Report.Info ("Mismatch at position"
              & Count'Image (Output.Mismatch_Index));
            declare
               Output_Data : Atom renames Output.Get_Data;
            begin
               Report.Info ("Mismatching data: """
                 & To_String
                    (Output_Data (Output.Mismatch_Index .. Output_Data'Last))
                 & '"');
            end;
         end if;

         if Output.Unread_Expected /= Null_Atom then
            Report.Info ("Left to expect: """
              & To_String (Output.Unread_Expected) & '"');
         end if;

         Report.Info ("Remaining unread data: """
           & To_String (Input.Unread_Data) & '"');
         Report.Info ("Written data: """
           & To_String (Output.Get_Data) & '"');
      else
         Report.Item (Name, NT.Success);
      end if;
   end Check_Parsing;


   procedure Blackbox_Test (Report : in out NT.Reporter'Class) is
   begin
      declare
         Input, Output : aliased Test_Tools.Memory_Stream;
         Printer : Printers.Canonical (Output'Access);
         Parser : aliased Parsers.Parser;
         Sub : Subparser (Parser'Access, Input'Access);
      begin
         Output.Set_Expected (Expected);
         Input.Set_Data (Source);
         Sub.Next;

         Printers.Transfer (Sub, Printer);

         Check_Parsing (Report, Name, Parser, Input, Output);
      end;
   exception
      when Error : others => Report.Report_Exception (Name, Error);
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
      Parser_Interface (Report);
      Subparser_Interface (Report);
      Lockable_Interface (Report);
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
         Parser : aliased Parsers.Parser;
         Sub : Subparser (Parser'Access, Input'Access);
      begin
         Input.Set_Data (Lockable.Tests.Test_Expression);
         Test_Tools.Next_And_Check (Test, Sub, Events.Open_List, 1);
         Lockable.Tests.Test_Interface (Test, Sub);
      end;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Lockable_Interface;


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


   procedure Parser_Interface (Report : in out NT.Reporter'Class) is
      Name : constant String := "Parser interface";
      Source : constant Atom
        := To_Atom ("(5:first6:second)");
   begin
      declare
         Input : aliased Test_Tools.Memory_Stream;
         Parser : Parsers.Parser;
      begin
         Input.Set_Data (Source);

         Parser.Next_Event (Input'Access);

         if Parser.Current_Event /= Events.Open_List then
            Report.Item (Name, NT.Fail);
            Report.Info ("Unexpected current event "
              & Events.Event'Image (Parser.Current_Event));
            return;
         end if;

         if Parser.Current_Level /= 1 then
            Report.Item (Name, NT.Fail);
            Report.Info ("Unexpected current level"
              & Integer'Image (Parser.Current_Level));
            return;
         end if;

         Parser.Next_Event (Input'Access);

         if Parser.Current_Event /= Events.Add_Atom then
            Report.Item (Name, NT.Fail);
            Report.Info ("Unexpected current event "
              & Events.Event'Image (Parser.Current_Event));
            return;
         end if;

         if Parser.Current_Atom /= To_Atom ("first") then
            Report.Item (Name, NT.Fail);
            Report.Info ("Unexpected current atom"
              & Integer'Image (Parser.Current_Atom'Length)
              & ":"
              & To_String (Parser.Current_Atom));
            return;
         end if;

         Parser.Next_Event (Input'Access);

         if Parser.Current_Event /= Events.Add_Atom then
            Report.Item (Name, NT.Fail);
            Report.Info ("Unexpected current event "
              & Events.Event'Image (Parser.Current_Event));
            return;
         end if;

         declare
            Buffer : Atom (50 .. 69);
            Length : Count;
         begin
            Parser.Read_Atom (Buffer, Length);
            if Length /= 6
              or else Buffer (Buffer'First .. Buffer'First + Length - 1)
                        /= To_Atom ("second")
            then
               Report.Item (Name, NT.Fail);
               Report.Info ("Unexpected read atom"
                 & Count'Image (Length)
                 & ":"
                 & To_String (Buffer
                     (Buffer'First .. Buffer'First + Length - 1)));
               return;
            end if;
         end;

         declare
            Buffer : Atom (11 .. 13);
            Length : Count;
         begin
            Parser.Read_Atom (Buffer, Length);
            if Length /= 6
              or else Buffer /= To_Atom ("sec")
            then
               Report.Item (Name, NT.Fail);
               Report.Info ("Unexpected read atom"
                 & Count'Image (Length)
                 & ":"
                 & To_String (Buffer));
               return;
            end if;
         end;

         Parser.Next_Event (Input'Access);

         if Parser.Current_Event /= Events.Close_List then
            Report.Item (Name, NT.Fail);
            Report.Info ("Unexpected current event "
              & Events.Event'Image (Parser.Current_Event));
            return;
         end if;

         begin
            declare
               Result : constant Atom := Parser.Current_Atom;
            begin
               Report.Item (Name, NT.Fail);
               Report.Info
                 ("Current_Atom raised no exception and returned"
                  & Integer'Image (Result'Length)
                  & ':'
                  & To_String (Result));
            end;
         exception
            when Program_Error => null;
            when Error : others =>
               Report.Report_Exception (Name & " (in Current_Event)", Error);
         end;

         declare
            Buffer : Atom (1 .. 10);
            Length : Count;
         begin
            Parser.Read_Atom (Buffer, Length);
            Report.Item (Name, NT.Fail);
            Report.Info
              ("Read_Atom raised no exception and returned"
               & Count'Image (Length)
               & ':'
               & To_String (Buffer));
         exception
            when Program_Error => null;
            when Error : others =>
               Report.Report_Exception (Name & " (in Read_Atom)", Error);
         end;

         declare
            Called : Boolean := False;
            Output : Test_Tools.Memory_Stream;

            procedure Process (Data : in Atom);

            procedure Process (Data : in Atom) is
            begin
               Called := True;
               Output.Set_Data (Data);
            end Process;
         begin
            Parser.Query_Atom (Process'Access);
            Report.Item (Name, NT.Fail);
            Report.Info ("Query_Atom raised no exception");
            if Called then
               Report.Info ("   Process was called with atom """
                 & To_String (Output.Get_Data) & '"');
            end if;
         exception
            when Program_Error => null;
            when Error : others =>
               Report.Report_Exception (Name & " (in Query_Event)", Error);
               if Called then
                  Report.Info ("   Process was called with atom """
                    & To_String (Output.Get_Data) & '"');
               end if;
         end;
      end;

      Report.Item (Name, NT.Success);
   exception
      when Error : others => Report.Report_Exception (Name, Error);
   end Parser_Interface;


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


   procedure Subparser_Interface (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Subparser interface");
      Source : constant Atom
        := To_Atom ("(begin(command arg1 (subarg1 subarg2) arg3)end)");
   begin
      declare
         Input : aliased Test_Tools.Memory_Stream;
         Parser : aliased Parsers.Parser;
         Sub : Subparser (Parser'Access, Input'Access);
         Event : Events.Event;
      begin
         Input.Set_Data (Source);

         --  Read header

         Parser.Next_Event (Input'Access);
         pragma Assert (Parser.Current_Event = Events.Open_List);
         Parser.Next_Event (Input'Access);
         pragma Assert (Parser.Current_Event = Events.Add_Atom
           and then Parser.Current_Atom = To_Atom ("begin"));
         Parser.Next_Event (Input'Access);
         pragma Assert (Parser.Current_Event = Events.Open_List);
         Parser.Next_Event (Input'Access);
         pragma Assert (Parser.Current_Event = Events.Add_Atom
           and then Parser.Current_Atom = To_Atom ("command"));

         --  Use subparser as command arguments

         Test_Tools.Next_And_Check (Test, Sub, To_Atom ("arg1"), 0);
         Test_Tools.Next_And_Check (Test, Sub, Events.Open_List, 1);
         Test_Tools.Next_And_Check (Test, Sub, To_Atom ("subarg1"), 1);
         Test_Tools.Next_And_Check (Test, Sub, To_Atom ("subarg2"), 1);

         Sub.Finish;

         --  Check final state of parser

         if Parser.Current_Event /= Events.Close_List then
            Test.Fail ("Unexpected parser final state: "
              & Events.Event'Image (Parser.Current_Event));
         end if;

         if Parser.Current_Level /= 1 then
            Test.Fail ("Unexpected parser final level:"
              & Natural'Image (Parser.Current_Level));
         end if;

         Parser.Next_Event (Input'Access);

         if Parser.Current_Event /= Events.Add_Atom then
            Test.Fail ("Unexpected parser penultimate state: "
              & Events.Event'Image (Parser.Current_Event));
         end if;

         if Parser.Current_Atom /= To_Atom ("end") then
            Test.Fail;
            Test_Tools.Dump_Atom (Test, Parser.Current_Atom,
              "Parser last atom");
         end if;

         --  Check subparser error states

         if Sub.Current_Event /= Events.End_Of_Input then
            Test.Fail ("Unexpected subparser final state: "
              & Events.Event'Image (Sub.Current_Event));
         end if;

         if Sub.Current_Level /= 0 then
            Test.Fail ("Unexpected subparser final level:"
              & Natural'Image (Sub.Current_Level));
         end if;

         begin
            declare
               Buffer : constant Atom := Sub.Current_Atom;
            begin
               Test.Fail
                 ("No exception raised in Current_Atom on finished subparser");
               Test_Tools.Dump_Atom (Test, Buffer);
            end;
            return;
         exception
            when Program_Error => null;
            when Error : others =>
               Test.Report_Exception (Error);
               Test.Info ("in Current_Atom");
         end;

         declare
            Buffer : Atom (1 .. 100);
            Length : Count := 0;
         begin
            Sub.Read_Atom (Buffer, Length);
            Test.Fail
              ("No exception raised in Read_Atom on finished subparser");
            Test_Tools.Dump_Atom (Test, Buffer (1 .. Length));
            return;
         exception
            when Program_Error => null;
            when Error : others =>
               Test.Report_Exception (Error);
               Test.Info ("in Read_Atom");
               Test_Tools.Dump_Atom (Test, Buffer (1 .. Length), "Buffer");
               return;
         end;

         declare
            Called : Boolean := False;
            Output : Test_Tools.Memory_Stream;

            procedure Process (Data : in Atom);

            procedure Process (Data : in Atom) is
            begin
               Called := True;
               Output.Set_Data (Data);
            end Process;
         begin
            Sub.Query_Atom (Process'Access);
            Test.Fail
              ("No exception raised in Query_Atom on finished subparser");
            if Called then
               Test_Tools.Dump_Atom (Test, Output.Get_Data,
                 "Process called with");
            end if;
         exception
            when Program_Error => null;
            when Error : others =>
               Test.Report_Exception (Error);
               Test.Info ("in Query_Event");
               if Called then
                  Test_Tools.Dump_Atom (Test, Output.Get_Data,
                    "Process called with");
               end if;
         end;

         begin
            Sub.Next (Event);
            Test.Fail ("No exception raised in Next on finished subparser");
            Test.Info ("   returned event: " & Events.Event'Image (Event));
         exception
            when Constraint_Error => null;
            when Error : others =>
               Test.Report_Exception (Error);
               Test.Info ("in Next");
         end;

         --  Check that above subparser calls have not tampered with Parser

         if Parser.Current_Event /= Events.Add_Atom
           or else Parser.Current_Level /= 1
           or else Parser.Current_Atom /= To_Atom ("end")
         then
            Test.Fail ("Parser state changed after calling methods on "
              & "finished subparser");
            return;
         end if;
      end;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Subparser_Interface;

end Natools.S_Expressions.Parsers.Tests;
