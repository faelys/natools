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
-- Natools.Smaz_Tools provides dictionary-independant tools to deal with    --
-- word lists and prepare dictionary creation.                              --
-- Note that the dictionary is intended to be generated and hard-coded,     --
-- so the final client shouldn't need this package.                         --
------------------------------------------------------------------------------

with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Natools.S_Expressions;

private with Ada.Containers.Indefinite_Ordered_Maps;
private with Ada.Containers.Indefinite_Ordered_Sets;
private with Ada.Finalization;

package Natools.Smaz_Tools is
   pragma Preelaborate;

   package String_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists
     (String);

   procedure Read_List
     (List : out String_Lists.List;
      Descriptor : in out S_Expressions.Descriptor'Class);
      --  Read atoms from Descriptor to fill List


   List_For_Linear_Search : String_Lists.List;
   function Linear_Search (Value : String) return Natural;
      --  Function and data source for inefficient but dynamic function
      --  that can be used with Dictionary.Hash.

   procedure Set_Dictionary_For_Map_Search (List : in String_Lists.List);
   function Map_Search (Value : String) return Natural;
      --  Function and data source for logarithmic search using standard
      --  ordered map, that can be used with Dictionary.Hash.

   type Search_Trie is private;
   procedure Initialize (Trie : out Search_Trie; List : in String_Lists.List);
   function Search (Trie : in Search_Trie; Value : in String) return Natural;
      --  Trie-based search in a dynamic dictionary, for lookup whose
      --  speed-vs-memory is even more skewed towards speed.

   procedure Set_Dictionary_For_Trie_Search (List : in String_Lists.List);
   function Trie_Search (Value : String) return Natural;
      --  Function and data source for trie-based search that can be
      --  used with Dictionary.Hash.

   function Dummy_Hash (Value : String) return Natural;
      --  Placeholder for Hash dictionary member, always raises Program_Error


   type String_Count is range 0 .. 2 ** 31 - 1;
      --  Type for a number of substring occurrences

   package Methods is
      type Enum is (Encoded, Frequency, Gain);
   end Methods;
      --  Evaluation methods to select words to remove or include

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

   procedure Add_Words
     (Counter : in out Word_Counter;
      Phrase : in String;
      Min_Size : in Positive;
      Max_Size : in Positive);
      --  Add the "words" from Phrase into Counter, with a word being currently
      --  defined as anything between ASCII blanks or punctuation,
      --  or in other words [0-9A-Za-z\x80-\xFF]+

   procedure Filter_By_Count
     (Counter : in out Word_Counter;
      Threshold_Count : in String_Count);
      --  Remove from Counter all entries whose count is below the threshold

   function Simple_Dictionary
     (Counter : in Word_Counter;
      Word_Count : in Natural;
      Method : in Methods.Enum := Methods.Encoded)
     return String_Lists.List;
      --  Return the Word_Count words in Counter that have the highest score,
      --  the score being count * length.

   procedure Simple_Dictionary_And_Pending
     (Counter : in Word_Counter;
      Word_Count : in Natural;
      Selected : out String_Lists.List;
      Pending : out String_Lists.List;
      Method : in Methods.Enum := Methods.Encoded;
      Max_Pending_Count : in Ada.Containers.Count_Type
        := Ada.Containers.Count_Type'Last);
      --  Return in Selected the Word_Count words in Counter that have the
      --  highest score, and in Pending the remaining words,
      --  the score being count * length.


   type Score_Value is range 0 .. 2 ** 31 - 1;

   function Score_Encoded
     (Count : in String_Count; Length : in Positive) return Score_Value
     is (Score_Value (Count) * Score_Value (Length));
      --  Score value using the amount of encoded data by the element

   function Score_Frequency
     (Count : in String_Count; Length : in Positive) return Score_Value
     is (Score_Value (Count));
      --  Score value using the number of times the element was used

   function Score_Gain
     (Count : in String_Count; Length : in Positive) return Score_Value
     is (Score_Value (Count) * (Score_Value (Length) - 1));
      --  Score value using the number of bytes saved using the element

   function Score
     (Count : in String_Count;
      Length : in Positive;
      Method : in Methods.Enum)
     return Score_Value
     is (case Method is
         when Methods.Encoded => Score_Encoded (Count, Length),
         when Methods.Frequency => Score_Frequency (Count, Length),
         when Methods.Gain => Score_Gain (Count, Length));
      --  Scare value with dynamically chosen method

private

   package Word_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (String, String_Count);

   type Word_Counter is record
      Map : Word_Maps.Map;
   end record;


   type Scored_Word (Size : Natural) is record
      Word : String (1 .. Size);
      Score : Score_Value;
   end record;

   function "<" (Left, Right : Scored_Word) return Boolean
     is (Left.Score > Right.Score
         or else (Left.Score = Right.Score and then Left.Word < Right.Word));

   function To_Scored_Word
     (Cursor : in Word_Maps.Cursor;
      Method : in Methods.Enum)
     return Scored_Word;

   package Scored_Word_Sets is new Ada.Containers.Indefinite_Ordered_Sets
     (Scored_Word);

   package Dictionary_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (String, Natural);

   Search_Map : Dictionary_Maps.Map;

   type Trie_Node;
   type Trie_Node_Access is access Trie_Node;
   type Trie_Node_Array is array (Character) of Trie_Node_Access;

   type Trie_Node (Is_Leaf : Boolean) is new Ada.Finalization.Controlled
     with record
      Index : Natural;

      case Is_Leaf is
         when True  => null;
         when False => Children : Trie_Node_Array;
      end case;
   end record;

   overriding procedure Adjust (Node : in out Trie_Node);
   overriding procedure Finalize (Node : in out Trie_Node);

   type Search_Trie is record
      Not_Found : Natural;
      Root : Trie_Node (False);
   end record;

   Trie_For_Search : Search_Trie;

end Natools.Smaz_Tools;
