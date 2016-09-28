------------------------------------------------------------------------------
-- Copyright (c) 2016, Natacha Porté                                        --
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
-- Natools.Smaz.Tools helps building and generating dictionary for use      --
-- with its parent package. Note that the dictionary is intended to be      --
-- generated and hard-coded, so the final client shouldn't need this        --
-- package.                                                                 --
------------------------------------------------------------------------------

with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Natools.S_Expressions;

private with Ada.Containers.Indefinite_Ordered_Maps;
private with Ada.Containers.Indefinite_Ordered_Sets;

package Natools.Smaz.Tools is
   pragma Preelaborate;

   package String_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists
     (String);

   procedure Read_List
     (List : out String_Lists.List;
      Descriptor : in out S_Expressions.Descriptor'Class);
      --  Read atoms from Descriptor to fill List

   function To_Dictionary
     (List : in String_Lists.List;
      Variable_Length_Verbatim : in Boolean)
     return Dictionary
     with Pre => String_Lists.Length (List) in 1 ..
                 Ada.Containers.Count_Type (Ada.Streams.Stream_Element'Last);
      --  Build a Dictionary object from a string list
      --  Note that Hash is set to a placeholder which uncinditionnally
      --  raises Program_Error when called.

   generic
      with procedure Put_Line (Line : String);
   procedure Print_Dictionary_In_Ada
     (Dict : in Dictionary;
      Hash_Image : in String := "TODO";
      Max_Width : in Positive := 70;
      First_Prefix : in String := "     := (";
      Prefix : in String := "         ";
      Half_Indent : in String := "   ");
      --  Output Ada code corresponding to the value of the dictionary.
      --  Note that Prefix is the actual base indentation, while Half_Indent
      --  is added beyond Prefix before values continued on another line.
      --  Frist_Prefix is used instead of Prefix on the first line.
      --  All the defaults value are what was used to generate the constant
      --  in Natools.Smaz.Original.

   List_For_Linear_Search : String_Lists.List;
   function Linear_Search (Value : String) return Natural;
      --  Function and data source for inefficient but dynamic function
      --  that can be used with Dictionary.Hash.

   type String_Count is range 0 .. 2 ** 31 - 1;
      --  Type for a number of substring occurrences

   type Word_Counter is private;
      --  Accumulate frequency/occurrence counts for a set of strings

   procedure Add_Word
     (Counter : in out Word_Counter;
      Word : in String;
      Count : in String_Count := 1);
      --  Include Count number of occurrences of Word in Counter

   procedure Add_Substrings
     (Counter : in out Word_Counter;
      Phrase : in String;
      Min_Size : in Positive;
      Max_Size : in Positive);
      --  Include all the substrings of Phrase whose lengths are
      --  between Min_Size and Max_Size.

   function Simple_Dictionary
     (Counter : in Word_Counter;
      Word_Count : in Natural)
     return String_Lists.List;
      --  Return the Word_Count words in Counter that have the highest score,
      --  the score being count * length.

private

   package Word_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (String, String_Count);

   type Word_Counter is record
      Map : Word_Maps.Map;
   end record;


   type Score_Value is range 0 .. 2 ** 31 - 1;

   type Scored_Word (Size : Natural) is record
      Word : String (1 .. Size);
      Score : Score_Value;
   end record;

   function "<" (Left, Right : Scored_Word) return Boolean
     is (Left.Score > Right.Score
         or else (Left.Score = Right.Score and then Left.Word < Right.Word));

   function To_Scored_Word (Cursor : in Word_Maps.Cursor)
     return Scored_Word;

   package Scored_Word_Sets is new Ada.Containers.Indefinite_Ordered_Sets
     (Scored_Word);

end Natools.Smaz.Tools;
