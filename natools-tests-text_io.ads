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
-- Natools.Tests.Text_IO is a simple implementation of Natools.Tests        --
-- interface. It immediately prints Item and Info to default output using   --
-- Ada.Text_IO facilities. Current and total result summaries are stored    --
-- in a stack using Doubly_Linked_Lists.                                    --
-- Sections are represented by a two-space indentation.                     --
------------------------------------------------------------------------------

private with Ada.Containers.Doubly_Linked_Lists;

package Natools.Tests.Text_IO is

   type Text_Reporter is new Reporter with private;

   procedure Section (Report : in out Text_Reporter; Name : String);
      --  Start a new (sub)section. This prints section header and increments
      --    indentation.

   procedure End_Section (Report : in out Text_Reporter);
      --  End the current (sub)section. This does not output anything, but
      --    decrements the current indentation.

   procedure Item
     (Report  : in out Text_Reporter;
      Name    : in     String;
      Outcome : in     Result);
      --  Output the Item with its outcome. If Line_Length is wide enough,
      --    the outcome is right-aligned on the same line as the test name,
      --    otherwise it is printed below with an additional indentation.

   procedure Info (Report : in out Text_Reporter; Text : String);
      --  Output the Text directly. Association with previous Item is visual.

   function Current_Results (Report : Text_Reporter) return Result_Summary;
      --  Return the number of each result type in the current subsection.

   function Total_Results (Report : Text_Reporter) return Result_Summary;
      --  Return the total number of each result type.


   procedure Print_Results (R : Result_Summary);
      --  Pretty-print the result summary into the default output.

private

   package Result_Lists is
      new Ada.Containers.Doubly_Linked_Lists (Result_Summary);

   type Text_Reporter is new Reporter with record
      Results : Result_Lists.List;
      Total : Result_Summary := (others => 0);
   end record;

end Natools.Tests.Text_IO;
