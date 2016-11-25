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
-- Natools.Smaz_Generic.Tools provides tools specific to the dictionary     --
-- implementation. These tools are useful for dictionary manipulation,      --
-- even though the intended use of this Smaz implementation is through a    --
-- global constant dictionary object.                                       --
------------------------------------------------------------------------------

with Ada.Containers;
with Natools.Smaz_Tools;

generic
package Natools.Smaz_Generic.Tools is
   pragma Preelaborate;

   package String_Lists renames Smaz_Tools.String_Lists;


   function To_Dictionary
     (List : in String_Lists.List;
      Variable_Length_Verbatim : in Boolean)
     return Dictionary
     with Pre => String_Lists.Length (List) in 1 ..
                 Ada.Containers.Count_Type (Ada.Streams.Stream_Element'Last);
      --  Build a Dictionary object from a string list
      --  Note that Hash is set to a placeholder which unconditionnally
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
      --  in Natools.Smaz_Original.

   function Remove_Element
     (Dict : in Dictionary;
      Index : in Dictionary_Code)
     return Dictionary
     with Pre => Index <= Dict.Last_Code,
         Post => Dict.Last_Code = Dictionary_Code'Succ
                                    (Remove_Element'Result.Last_Code)
               and then (Index = Dictionary_Code'First
                  or else (for all I in Dictionary_Code'First
                                     .. Dictionary_Code'Pred (Index)
                     => Dict_Entry (Dict, I)
                        = Dict_Entry (Remove_Element'Result, I)))
               and then (Index = Dict.Last_Code
                  or else (for all I in Index
                                     .. Dictionary_Code'Pred (Dict.Last_Code)
                    => Dict_Entry (Dict, Dictionary_Code'Succ (I))
                       = Dict_Entry (Remove_Element'Result, I)));
      --  Return a new dictionary equal to Dict without element for Index

   function Append_String
     (Dict : in Dictionary;
      Value : in String)
     return Dictionary
     with Pre => Dict.Last_Code < Dictionary_Code'Last
               and then Value'Length > 0,
         Post => Dict.Last_Code = Dictionary_Code'Pred
                                    (Append_String'Result.Last_Code)
               and then (for all I in Dictionary_Code'First .. Dict.Last_Code
                  => Dict_Entry (Dict, I)
                     = Dict_Entry (Append_String'Result, I))
               and then Dict_Entry (Append_String'Result,
                                    Append_String'Result.Last_Code)
                        = Value;
      --  Return a new dictionary with Value appended


   type Dictionary_Counts is
     array (Dictionary_Code) of Smaz_Tools.String_Count;

   function Score_Encoded
     (Dict : in Dictionary;
      Counts : in Dictionary_Counts;
      E : in Dictionary_Code)
     return Smaz_Tools.Score_Value
     is (Smaz_Tools.Score_Encoded (Counts (E), Dict_Entry_Length (Dict, E)));
      --  Score value using the amount of encoded data using E

   function Score_Frequency
     (Dict : in Dictionary;
      Counts : in Dictionary_Counts;
      E : in Dictionary_Code)
     return Smaz_Tools.Score_Value
     is (Smaz_Tools.Score_Frequency (Counts (E), Dict_Entry_Length (Dict, E)));
      --  Score value using the number of times E was used

   function Score_Gain
     (Dict : in Dictionary;
      Counts : in Dictionary_Counts;
      E : in Dictionary_Code)
     return Smaz_Tools.Score_Value
     is (Smaz_Tools.Score_Gain (Counts (E), Dict_Entry_Length (Dict, E)));
      --  Score value using the number of bytes saved using E

   function Score
     (Dict : in Dictionary;
      Counts : in Dictionary_Counts;
      E : in Dictionary_Code;
      Method : in Smaz_Tools.Methods.Enum)
     return Smaz_Tools.Score_Value
     is (Smaz_Tools.Score (Counts (E), Dict_Entry_Length (Dict, E), Method));
      --  Scare value with dynamically chosen method

end Natools.Smaz_Generic.Tools;
