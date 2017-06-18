------------------------------------------------------------------------------
-- Copyright (c) 2015-2017, Natacha Porté                                   --
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

with Natools.S_Expressions.Caches;
with Natools.S_Expressions.Test_Tools;

package body Natools.S_Expressions.Conditionals.Strings.Tests is

   procedure Check
     (Test : in out NT.Test;
      Context : in Strings.Context;
      Expression : in Caches.Reference;
      Image : in String;
      Expected : in Boolean := True);

   procedure Check
     (Test : in out NT.Test;
      Value : in String;
      Expression : in Caches.Reference;
      Image : in String;
      Expected : in Boolean := True);


   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

   procedure Check
     (Test : in out NT.Test;
      Context : in Strings.Context;
      Expression : in Caches.Reference;
      Image : in String;
      Expected : in Boolean := True)
   is
      function Match_Image return String;

      Cursor : Caches.Cursor := Expression.First;

      function Match_Image return String is
      begin
         if Expected then
            return " does not match ";
         else
            return " does match ";
         end if;
      end Match_Image;
   begin
      if Evaluate (Context, Cursor) /= Expected then
         Test.Fail ('"' & Context.Data.all & '"' & Match_Image & Image);
      end if;
   end Check;


   procedure Check
     (Test : in out NT.Test;
      Value : in String;
      Expression : in Caches.Reference;
      Image : in String;
      Expected : in Boolean := True)
   is
      function Match_Image return String;

      Cursor : Caches.Cursor := Expression.First;

      function Match_Image return String is
      begin
         if Expected then
            return " does not match ";
         else
            return " does match ";
         end if;
      end Match_Image;
   begin
      if Evaluate (Value, Cursor) /= Expected then
         Test.Fail ('"' & Value & '"' & Match_Image & Image);
      end if;
   end Check;



   -------------------------
   -- Complete Test Suite --
   -------------------------

   procedure All_Tests (Report : in out NT.Reporter'Class) is
   begin
      Basic_Usage (Report);
      Fallbacks (Report);
   end All_Tests;



   ----------------------
   -- Individual Tests --
   ----------------------

   procedure Basic_Usage (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Basic usage");
   begin
      declare
         procedure Check (Value : in String; Expected : in Boolean := True);

         Image : constant String := "Expression 1";
         Exp : constant Caches.Reference := Test_Tools.To_S_Expression
           ("(or is-empty (starts-with Hi)"
            & "(is BY) (case-insensitive (is HELLO))"
            & "(and (contains 1:.) (contains-any-of 1:! 1:?))"
            & "(case-insensitive (or (contains aLiCe)"
            & " (case-sensitive (contains Bob))))"
            & "(not is-ascii))");

         procedure Check (Value : in String; Expected : in Boolean := True) is
         begin
            Check (Test, Value, Exp, Image, Expected);
         end Check;
      begin
         Check ("");
         Check ("A", False);
         Check ("Hi, my name is John.");
         Check ("Hello, my name is John.", False);
         Check ("Hello. My name is John!");
         Check ("Hello. My name is John?");
         Check ("Alice and Bob");
         Check ("BOBBY!", False);
         Check ("AlicE and Malory");
         Check ("©");
         Check ("BY");
         Check ("By", False);
         Check ("Hello");
         Check ("Hell", False);
      end;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Basic_Usage;


   procedure Fallbacks (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Fallback functions");
   begin
      declare
         procedure Check
           (Value : in String;
            With_Fallback : in Boolean);

         procedure Check_Counts
           (Expected_Parametric, Expected_Simple : in Natural);

         function Parametric_Fallback
           (Settings : in Strings.Settings;
            Name : in Atom;
            Arguments : in out Lockable.Descriptor'Class)
           return Boolean;

         function Simple_Fallback
           (Settings : in Strings.Settings;
            Name : in Atom)
           return Boolean;

         Parametric_Count : Natural := 0;
         Simple_Count : Natural := 0;

         Exp : constant Caches.Reference := Test_Tools.To_S_Expression
           ("(or"
            & "(and (starts-with a) non-existant)"
            & "(does-not-exist ohai ()))");

         procedure Check
           (Value : in String;
            With_Fallback : in Boolean)
         is
            Copy : aliased constant String := Value;
            Context : Strings.Context
              (Data => Copy'Access,
               Parametric_Fallback => (if With_Fallback
                                       then Parametric_Fallback'Access
                                       else null),
               Simple_Fallback     => (if With_Fallback
                                       then Simple_Fallback'Access
                                       else null));
         begin
            Context.Settings.Case_Sensitive := False;

            begin
               Check (Test, Context, Exp, "Fallback expression");

               if not With_Fallback then
                  Test.Fail ("Exception expected from """ & Value & '"');
               end if;
            exception
               when Constraint_Error =>
                  if With_Fallback then
                     raise;
                  end if;
            end;
         end Check;

         procedure Check_Counts
           (Expected_Parametric, Expected_Simple : in Natural) is
         begin
            if Parametric_Count /= Expected_Parametric then
               Test.Fail ("Parametric_Count is"
                 & Natural'Image (Parametric_Count) & ", expected"
                 & Natural'Image (Expected_Parametric));
            end if;

            if Simple_Count /= Expected_Simple then
               Test.Fail ("Simple_Count is"
                 & Natural'Image (Simple_Count) & ", expected"
                 & Natural'Image (Expected_Simple));
            end if;
         end Check_Counts;

         function Parametric_Fallback
           (Settings : in Strings.Settings;
            Name : in Atom;
            Arguments : in out Lockable.Descriptor'Class)
           return Boolean
         is
            pragma Unreferenced (Settings, Arguments);
         begin
            Parametric_Count := Parametric_Count + 1;
            return To_String (Name) = "does-not-exist";
         end Parametric_Fallback;

         function Simple_Fallback
           (Settings : in Strings.Settings;
            Name : in Atom)
           return Boolean
         is
            pragma Unreferenced (Settings);
         begin
            Simple_Count := Simple_Count + 1;
            return To_String (Name) = "non-existant";
         end Simple_Fallback;
      begin
         Check ("Oook?", False);
         Check ("Alice", False);
         Check ("Alpha", True);
         Check_Counts (0, 1);
         Check ("Bob", True);
         Check_Counts (1, 1);
      end;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Fallbacks;

end Natools.S_Expressions.Conditionals.Strings.Tests;
