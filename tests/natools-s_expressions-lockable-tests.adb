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

with Natools.S_Expressions.Test_Tools;
with Natools.S_Expressions.Parsers;

package body Natools.S_Expressions.Lockable.Tests is

   -------------------------
   -- Complete Test Suite --
   -------------------------

   procedure All_Tests (Report : in out NT.Reporter'Class) is
   begin
      Test_Stack (Report);
      Test_Wrapper_Interface (Report);
      Test_Wrapper_Extra (Report);
   end All_Tests;



   ---------------------------
   -- Individual Test Cases --
   ---------------------------

   procedure Test_Stack (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Level stack");
   begin
      declare
         Stack : Lock_Stack;
         State : array (1 .. 4) of Lock_State;

         procedure Check_Level
           (Stack : in Lock_Stack;
            Expected : in Natural;
            Context : in String);
         procedure Dump_Data;

         procedure Check_Level
           (Stack : in Lock_Stack;
            Expected : in Natural;
            Context : in String)
         is
            Level : constant Natural := Current_Level (Stack);
         begin
            if Level /= Expected then
               Test.Fail (Context & ": level is"
                 & Natural'Image (Level) & ", expected"
                 & Natural'Image (Expected));
               Dump_Data;
            end if;
         end Check_Level;

         procedure Dump_Data is
         begin
            Test.Info ("   Stack: (Depth =>"
              & Natural'Image (Stack.Depth)
              & ", Level =>"
              & Natural'Image (Stack.Level) & ')');
            for I in State'Range loop
               Test.Info ("   State"
                 & Natural'Image (I)
                 & ": (Depth =>"
                 & Natural'Image (Stack.Depth)
                 & ", Level =>"
                 & Natural'Image (Stack.Level) & ')');
            end loop;
         end Dump_Data;
      begin
         Check_Level (Stack, 0, "1");
         Push_Level (Stack, 14, State (1));
         Check_Level (Stack, 14, "2");

         begin
            Pop_Level (Stack, State (2));
            Test.Fail ("No exception raised after popping blank state");
         exception
            when Constraint_Error =>
               null;
            when Error : others =>
               Test.Fail
                 ("Unexpected exception raised after popping blank state");
               Test.Report_Exception (Error, NT.Fail);
         end;

         Pop_Level (Stack, State (1));
         Check_Level (Stack, 0, "3");
         Push_Level (Stack, 15, State (1));
         Check_Level (Stack, 15, "4");
         Push_Level (Stack, 92, State (2));
         Check_Level (Stack, 92, "5");
         Push_Level (Stack, 65, State (3));
         Check_Level (Stack, 65, "6");
         Pop_Level (Stack, State (3));
         Check_Level (Stack, 92, "7");
         Push_Level (Stack, 35, State (3));
         Check_Level (Stack, 35, "8");
         Push_Level (Stack, 89, State (4));
         Check_Level (Stack, 89, "9");

         begin
            Pop_Level (Stack, State (3));
            Test.Fail ("No exception raised after popping a forbidden gap");
         exception
            when Constraint_Error =>
               null;
            when Error : others =>
               Test.Fail
                 ("Unexpected exception raised after popping a forbidden gap");
               Test.Report_Exception (Error, NT.Fail);
         end;

         Check_Level (Stack, 89, "10");
         Pop_Level (Stack, State (3), True);
         Check_Level (Stack, 92, "11");

         begin
            Pop_Level (Stack, State (4));
            Test.Fail ("No exception raised after popping stale state");
         exception
            when Constraint_Error =>
               null;
            when Error : others =>
               Test.Fail
                 ("Unexpected exception raised after popping stale state");
               Test.Report_Exception (Error, NT.Fail);
         end;

         Check_Level (Stack, 92, "12");
         Pop_Level (Stack, State (2));
         Check_Level (Stack, 15, "13");
         Pop_Level (Stack, State (1));
         Check_Level (Stack, 0, "14");
      end;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Test_Stack;

end Natools.S_Expressions.Lockable.Tests;
