------------------------------------------------------------------------------
-- Copyright (c) 2011, Natacha Porté                                        --
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
-- Natools.Tests is an abstract interface for objects holding the results   --
-- of a series of tests.                                                    --
--                                                                          --
-- Each test can have one of the following results:                         --
--   * Success, when everything goes well,                                  --
--   * Fail, when the test itself went fine the but the result is wrong,    --
--   * Error, when the test itself went wrong, which does not tell whether  --
--     the tested thing is fine or not,                                     --
--   * Skipped, when for any reason the test has not been performed         --
--     (e.g. missing dependency).                                           --
--                                                                          --
-- Tests are gathered into sections, which can be nested. What a section    --
-- exactly means is left to the implementation of this interface.           --
------------------------------------------------------------------------------

with Ada.Exceptions;

private with Ada.Finalization;
private with Ada.Strings.Unbounded;
private with Ada.Containers.Indefinite_Doubly_Linked_Lists;

package Natools.Tests is
   pragma Preelaborate (Tests);

   type Reporter is interface;
   type Result is (Success, Fail, Error, Skipped);
   type Result_Summary is array (Result) of Natural;

   procedure Section (Report : in out Reporter; Name : String) is abstract;
   procedure End_Section (Report : in out Reporter) is abstract;
      --  These procedures change the internal state of Report to respectively
      --    enter and leave a (sub)section.

   procedure Item
     (Report  : in out Reporter;
      Name    : in     String;
      Outcome : in     Result)
      is abstract;
      --  Append a new test item (with its outcome) to the current section
      --    of Report.

   procedure Info (Report : in out Reporter; Text : String) is abstract;
      --  Append free informational text related to the previous Item appended.

   function Current_Results (Report : Reporter) return Result_Summary
      is abstract;
      --  Return the number of each result type in the current section.

   function Total_Results (Report : Reporter) return Result_Summary
      is abstract;
      --  Return the total number of each result type in the current section.

   function To_Result (Succeeded : Boolean) return Result;
      --  Return Success or Fail depending on the Boolean input.

   Max_Result_String_Size : constant Positive := 7;
      --  Maximum length of any string returned by Result'Image.


   ------------------------
   -- Helper subprograms --
   ------------------------

   procedure Report_Exception
     (Report    : in out Reporter'Class;
      Test_Name : String;
      Ex        : Ada.Exceptions.Exception_Occurrence;
      Code      : Result := Error);
      --  Append to Report a new Item, whose result is Code, along with
      --    a description of the exception Ex as Info entries.


   -----------------
   -- Test Object --
   -----------------

   type Test (<>) is tagged limited private;
      --  An object of type Test hold information about a single test.
      --  It contains a reference to a Reporter object, which is filled with
      --  held info when the Test object is finalized.

   function Item
     (Report : access Reporter'Class;
      Name : String;
      Default_Outcome : Result := Success)
     return Test;
      --  Create a new Test object with the given Name

   procedure Set_Result (Object : in out Test; Outcome : in Result);
      --  Set the test result

   procedure Info (Object : in out Test; Text : in String);
      --  Append the given text as extra information related to the test

   procedure Report_Exception
     (Object : in out Test;
      Ex     : in Ada.Exceptions.Exception_Occurrence;
      Code   : in Result := Error);
      --  Append information about Ex to the test and set its result state

   procedure Fail (Object : in out Test; Text : in String := "");
   procedure Error (Object : in out Test; Text : in String := "");
   procedure Skip (Object : in out Test; Text : in String := "");
      --  Set the result state and append Text info in a single call

   generic
      type Result (<>) is limited private;
      with function "=" (Left, Right : Result) return Boolean is <>;
      with function Image (Object : Result) return String is <>;
      Multiline : Boolean := True;
   procedure Generic_Check
     (Object : in out Test;
      Expected : in Result;
      Found : in Result;
      Label : in String := "");

private

   package Info_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists
     (String);

   type Test (Report : access Reporter'Class) is
     new Ada.Finalization.Limited_Controlled with record
      Name : Ada.Strings.Unbounded.Unbounded_String;
      Info : Info_Lists.List;
      Outcome : Result;
      Finalized : Boolean := False;
   end record;

   overriding procedure Finalize (Object : in out Test);

end Natools.Tests;
