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

with Ada.Unchecked_Deallocation;

package body Natools.Smaz.Tools is

   package Sx renames Natools.S_Expressions;

   function Build_Node
     (Map : Dictionary_Maps.Map;
      Empty_Value : Natural)
     return Trie_Node;

   function Dummy_Hash (Value : String) return Natural;
      --  Placeholder for Hash member, always raises Program_Error

   function Image (B : Boolean) return String;
      --  Return correctly-cased image of B

   procedure Free is new Ada.Unchecked_Deallocation
     (Trie_Node, Trie_Node_Access);


   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

   function Build_Node
     (Map : Dictionary_Maps.Map;
      Empty_Value : Natural)
     return Trie_Node
   is
      function First_Character (S : String) return Character
        is (S (S'First));
      function Is_Current (Cursor : Dictionary_Maps.Cursor; C : Character)
        return Boolean
        is (Dictionary_Maps.Has_Element (Cursor)
            and then First_Character (Dictionary_Maps.Key (Cursor)) = C);

      function Suffix (S : String) return String;

      function Suffix (S : String) return String is
      begin
         return S (S'First + 1 .. S'Last);
      end Suffix;

      use type Ada.Containers.Count_Type;
      Cursor : Dictionary_Maps.Cursor;
      Result : Trie_Node
        := (Ada.Finalization.Controlled with
            Is_Leaf => False,
            Index => Empty_Value,
            Children => (others => null));
   begin
      pragma Assert (Dictionary_Maps.Length (Map) >= 1);

      Cursor := Dictionary_Maps.Find (Map, "");

      if Dictionary_Maps.Has_Element (Cursor) then
         Result.Index := Natural (Dictionary_Maps.Element (Cursor));
      end if;

      for C in Character'Range loop
         Cursor := Dictionary_Maps.Ceiling (Map, (1 => C));

         if Is_Current (Cursor, C) then
            if not Is_Current (Dictionary_Maps.Next (Cursor), C)
              and then Dictionary_Maps.Key (Cursor) = (1 => C)
            then
               Result.Children (C)
                 := new Trie_Node'(Ada.Finalization.Controlled with
                     Is_Leaf => True,
                     Index => Natural (Dictionary_Maps.Element (Cursor)));
            else
               declare
                  New_Map : Dictionary_Maps.Map;
               begin
                  loop
                     Dictionary_Maps.Insert
                       (New_Map,
                        Suffix (Dictionary_Maps.Key (Cursor)),
                        Dictionary_Maps.Element (Cursor));
                     Dictionary_Maps.Next (Cursor);
                     exit when not Is_Current (Cursor, C);
                  end loop;

                  Result.Children (C)
                    := new Trie_Node'(Build_Node (New_Map, Empty_Value));
               end;
            end if;
         end if;
      end loop;

      return Result;
   end Build_Node;

   function Dummy_Hash (Value : String) return Natural is
      pragma Unreferenced (Value);
   begin
      raise Program_Error with "Dummy_Hash called";
      return 0;
   end Dummy_Hash;


   function Image (B : Boolean) return String is
   begin
      if B then
         return "True";
      else
         return "False";
      end if;
   end Image;



   ----------------------
   -- Public Interface --
   ----------------------

   function Append_String
     (Dict : in Dictionary;
      Value : in String)
     return Dictionary is
   begin
      return Dictionary'
        (Dict_Last => Dict.Dict_Last + 1,
         String_Size => Dict.String_Size + Value'Length,
         Variable_Length_Verbatim => Dict.Variable_Length_Verbatim,
         Max_Word_Length => Positive'Max (Dict.Max_Word_Length, Value'Length),
         Offsets => Dict.Offsets & (1 => Dict.String_Size + 1),
         Values => Dict.Values & Value,
         Hash => Dummy_Hash'Access);
   end Append_String;


   procedure Print_Dictionary_In_Ada
     (Dict : in Dictionary;
      Hash_Image : in String := "TODO";
      Max_Width : in Positive := 70;
      First_Prefix : in String := "     := (";
      Prefix : in String := "         ";
      Half_Indent : in String := "   ")
   is
      procedure Append_Entity
        (Buffer : in out String;
         Last : in out Natural;
         Entity : in String);
      function Double_Quote (S : String; Count : Natural) return String;
      function Offsets_Suffix (I : Ada.Streams.Stream_Element) return String;
      function Strip_Image (S : String) return String;
      function Values_Separator (I : Positive) return String;

      procedure Append_Entity
        (Buffer : in out String;
         Last : in out Natural;
         Entity : in String) is
      begin
         if Last + 1 + Entity'Length <= Buffer'Last then
            Buffer (Last + 1) := ' ';
            Buffer (Last + 2 .. Last + 1 + Entity'Length) := Entity;
            Last := Last + 1 + Entity'Length;
         else
            Put_Line (Buffer (Buffer'First .. Last));
            Last := Buffer'First + Prefix'Length - 1;
            Buffer (Last + 1 .. Last + Half_Indent'Length) := Half_Indent;
            Last := Last + Half_Indent'Length;
            Buffer (Last + 1 .. Last + Entity'Length) := Entity;
            Last := Last + Entity'Length;
         end if;
      end Append_Entity;

      function Double_Quote (S : String; Count : Natural) return String is
      begin
         if Count = 0 then
            return S;
         else
            return Quoted : String (1 .. S'Length + Count) do
               declare
                  O : Positive := Quoted'First;
               begin
                  for I in S'Range loop
                     Quoted (O) := S (I);
                     O := O + 1;

                     if S (I) = '"' then
                        Quoted (O) := S (I);
                        O := O + 1;
                     end if;
                  end loop;
               end;
            end return;
         end if;
      end Double_Quote;

      function Offsets_Suffix (I : Ada.Streams.Stream_Element) return String is
      begin
         if I < Dict.Offsets'Last then
            return ",";
         else
            return "),";
         end if;
      end Offsets_Suffix;

      function Strip_Image (S : String) return String is
      begin
         if S'Length > 0 and then S (S'First) = ' ' then
            return S (S'First + 1 .. S'Last);
         else
            return S;
         end if;
      end Strip_Image;

      function Values_Separator (I : Positive) return String is
      begin
         if I > Dict.Values'First then
            return "& ";
         else
            return "";
         end if;
      end Values_Separator;

      Line_Buffer : String (1 .. Max_Width + Prefix'Length);
      Buffer_Last : Natural;
   begin
      Put_Line (First_Prefix & "Dict_Last =>"
        & Ada.Streams.Stream_Element'Image (Dict.Dict_Last) & ',');
      Put_Line (Prefix & "String_Size =>"
        & Natural'Image (Dict.String_Size) & ',');
      Put_Line (Prefix & "Variable_Length_Verbatim => "
        & Image (Dict.Variable_Length_Verbatim) & ',');
      Put_Line (Prefix & "Max_Word_Length =>"
        & Natural'Image (Dict.Max_Word_Length) & ',');

      Line_Buffer (1 .. Prefix'Length) := Prefix;
      Line_Buffer (Prefix'Length + 1 .. Prefix'Length + 11) := "Offsets => ";
      Buffer_Last := Prefix'Length + 11;

      for I in Dict.Offsets'Range loop
         Append_Entity (Line_Buffer, Buffer_Last, Strip_Image
           (Positive'Image (Dict.Offsets (I)) & Offsets_Suffix (I)));

         if I = Dict.Offsets'First then
            Line_Buffer (Prefix'Length + 12) := '(';
         end if;
      end loop;

      Put_Line (Line_Buffer (Line_Buffer'First .. Buffer_Last));
      Line_Buffer (Prefix'Length + 1 .. Prefix'Length + 9) := "Values =>";
      Buffer_Last := Prefix'Length + 9;

      declare
         I : Positive := Dict.Values'First;
         First, Last : Positive;
         Quote_Count : Natural;
      begin
         Values_Loop :
         while I <= Dict.Values'Last loop
            Add_Unprintable :
            while Dict.Values (I) not in ' ' .. '~' loop
               Append_Entity
                 (Line_Buffer, Buffer_Last,
                  Values_Separator (I) & Character'Image (Dict.Values (I)));
               I := I + 1;
               exit Values_Loop when I > Dict.Values'Last;
            end loop Add_Unprintable;

            First := I;
            Quote_Count := 0;

            Find_Printable_Substring :
            loop
               if Dict.Values (I) = '"' then
                  Quote_Count := Quote_Count + 1;
               end if;

               I := I + 1;
               exit Find_Printable_Substring when I > Dict.Values'Last
                 or else Dict.Values (I) not in ' ' .. '~';
            end loop Find_Printable_Substring;

            Last := I - 1;

            Split_Lines :
            loop
               declare
                  Partial_Quote_Count : Natural := 0;
                  Partial_Width : Natural := 0;
                  Partial_Last : Natural := First - 1;
                  Sep : constant String := Values_Separator (First);
                  Available_Length : constant Natural
                    := (if Line_Buffer'Last > Buffer_Last + Sep'Length + 4
                        then Line_Buffer'Last - Buffer_Last - Sep'Length - 4
                        else Line_Buffer'Length - Prefix'Length
                          - Half_Indent'Length - Sep'Length - 3);
               begin
                  if 1 + Last - First + Quote_Count < Available_Length then
                     Append_Entity
                       (Line_Buffer, Buffer_Last,
                        Sep & '"' & Double_Quote
                          (Dict.Values (First .. Last), Quote_Count) & '"');
                     exit Split_Lines;
                  else
                     Count_Quotes :
                     loop
                        if Dict.Values (Partial_Last + 1) = '"' then
                           exit Count_Quotes
                             when Partial_Width + 2 > Available_Length;
                           Partial_Width := Partial_Width + 1;
                           Partial_Quote_Count := Partial_Quote_Count + 1;
                        else
                           exit Count_Quotes
                             when Partial_Width + 1 > Available_Length;
                        end if;

                        Partial_Width := Partial_Width + 1;
                        Partial_Last := Partial_Last + 1;
                     end loop Count_Quotes;

                     Append_Entity
                       (Line_Buffer, Buffer_Last, Sep & '"'
                        & Double_Quote
                          (Dict.Values (First .. Partial_Last),
                           Partial_Quote_Count)
                        & '"');
                     First := Partial_Last + 1;
                     Quote_Count := Quote_Count - Partial_Quote_Count;
                  end if;
               end;
            end loop Split_Lines;
         end loop Values_Loop;

         Put_Line (Line_Buffer (Line_Buffer'First .. Buffer_Last) & ',');
      end;

      Line_Buffer (Prefix'Length + 1 .. Prefix'Length + 7) := "Hash =>";
      Buffer_Last := Prefix'Length + 7;
      Append_Entity (Line_Buffer, Buffer_Last, Hash_Image & ");");
      Put_Line (Line_Buffer (Line_Buffer'First .. Buffer_Last));
   end Print_Dictionary_In_Ada;


   procedure Read_List
     (List : out String_Lists.List;
      Descriptor : in out S_Expressions.Descriptor'Class)
   is
      use type Sx.Events.Event;
      Event : Sx.Events.Event := Descriptor.Current_Event;
   begin
      String_Lists.Clear (List);

      if Event = Sx.Events.Open_List then
         Descriptor.Next (Event);
      end if;

      Read_Loop :
      loop
         case Event is
            when Sx.Events.Add_Atom =>
               String_Lists.Append
                 (List, Sx.To_String (Descriptor.Current_Atom));
            when Sx.Events.Open_List =>
               Descriptor.Close_Current_List;
            when Sx.Events.End_Of_Input | Sx.Events.Error
              | Sx.Events.Close_List =>
               exit Read_Loop;
         end case;

         Descriptor.Next (Event);
      end loop Read_Loop;
   end Read_List;


   function Remove_Element
     (Dict : in Dictionary;
      Index : in Ada.Streams.Stream_Element)
     return Dictionary
   is
      Removed_Length : constant Positive := Dict_Entry (Dict, Index)'Length;

      function New_Offsets return Offset_Array;
      function New_Values return String;

      function New_Offsets return Offset_Array is
         Result : Offset_Array (0 .. Dict.Dict_Last - 1);
      begin
         for I in Result'Range loop
            if I < Index then
               Result (I) := Dict.Offsets (I);
            else
               Result (I) := Dict.Offsets (I + 1) - Removed_Length;
            end if;
         end loop;

         return Result;
      end New_Offsets;

      function New_Values return String is
      begin
         if Index < Dict.Dict_Last then
            return Dict.Values (1 .. Dict.Offsets (Index) - 1)
              & Dict.Values (Dict.Offsets (Index + 1) .. Dict.Values'Last);
         else
            return Dict.Values (1 .. Dict.Offsets (Index) - 1);
         end if;
      end New_Values;

      New_Max_Word_Length : Positive := Dict.Max_Word_Length;
   begin
      if Removed_Length = Dict.Max_Word_Length then
         New_Max_Word_Length := 1;
         for I in Dict.Offsets'Range loop
            if I /= Index
              and then Dict_Entry (Dict, I)'Length > New_Max_Word_Length
            then
               New_Max_Word_Length := Dict_Entry (Dict, I)'Length;
            end if;
         end loop;
      end if;

      return Dictionary'
        (Dict_Last => Dict.Dict_Last - 1,
         String_Size => Dict.String_Size - Removed_Length,
         Variable_Length_Verbatim => Dict.Variable_Length_Verbatim,
         Max_Word_Length => New_Max_Word_Length,
         Offsets => New_Offsets,
         Values => New_Values,
         Hash => Dummy_Hash'Access);
   end Remove_Element;


   function To_Dictionary
     (List : in String_Lists.List;
      Variable_Length_Verbatim : in Boolean)
     return Dictionary
   is
      Dict_Last : constant Ada.Streams.Stream_Element
        := Ada.Streams.Stream_Element (String_Lists.Length (List)) - 1;
      String_Size : Natural := 0;
      Max_Word_Length : Positive := 1;
   begin
      for S of List loop
         String_Size := String_Size + S'Length;

         if S'Length > Max_Word_Length then
            Max_Word_Length := S'Length;
         end if;
      end loop;

      declare
         Offsets : Offset_Array (0 .. Dict_Last);
         Values : String (1 .. String_Size);
         Current_Offset : Positive := 1;
         Current_Index : Ada.Streams.Stream_Element := 0;
         Next_Offset : Positive;
      begin
         for S of List loop
            Offsets (Current_Index) := Current_Offset;
            Next_Offset := Current_Offset + S'Length;
            Values (Current_Offset .. Next_Offset - 1) := S;
            Current_Offset := Next_Offset;
            Current_Index := Current_Index + 1;
         end loop;

         pragma Assert (Current_Index = Dict_Last + 1);
         pragma Assert (Current_Offset = String_Size + 1);

         return
           (Dict_Last => Dict_Last,
            String_Size => String_Size,
            Variable_Length_Verbatim => Variable_Length_Verbatim,
            Max_Word_Length => Max_Word_Length,
            Offsets => Offsets,
            Values => Values,
            Hash => Dummy_Hash'Access);
      end;
   end To_Dictionary;



   ---------------------------------
   -- Dynamic Dictionary Searches --
   ---------------------------------

   overriding procedure Adjust (Node : in out Trie_Node) is
   begin
      if not Node.Is_Leaf then
         for C in Node.Children'Range loop
            if Node.Children (C) /= null then
               Node.Children (C) := new Trie_Node'(Node.Children (C).all);
            end if;
         end loop;
      end if;
   end Adjust;


   overriding procedure Finalize (Node : in out Trie_Node) is
   begin
      if not Node.Is_Leaf then
         for C in Node.Children'Range loop
            Free (Node.Children (C));
         end loop;
      end if;
   end Finalize;


   procedure Initialize (Trie : out Search_Trie; Dict : in Dictionary) is
      Map : Dictionary_Maps.Map;
   begin
      for I in Dict.Offsets'Range loop
         Dictionary_Maps.Insert (Map, Dict_Entry (Dict, I), I);
      end loop;

      Trie := (Not_Found => Natural (Dict.Dict_Last) + 1,
               Root => Build_Node (Map, Natural (Dict.Dict_Last) + 1));
   end Initialize;


   function Linear_Search (Value : String) return Natural is
      Result : Ada.Streams.Stream_Element := 0;
   begin
      for S of List_For_Linear_Search loop
         exit when S = Value;
         Result := Result + 1;
      end loop;

      return Natural (Result);
   end Linear_Search;


   function Map_Search (Value : String) return Natural is
      Cursor : constant Dictionary_Maps.Cursor
        := Dictionary_Maps.Find (Search_Map, Value);
   begin
      if Dictionary_Maps.Has_Element (Cursor) then
         return Natural (Dictionary_Maps.Element (Cursor));
      else
         return Natural (Ada.Streams.Stream_Element'Last);
      end if;
   end Map_Search;


   function Search (Trie : in Search_Trie; Value : in String) return Natural is
      Index : Positive := Value'First;
      Position : Trie_Node_Access;
   begin
      if Value'Length = 0 then
         return Trie.Not_Found;
      end if;

      Position := Trie.Root.Children (Value (Index));

      loop
         if Position = null then
            return Trie.Not_Found;
         end if;

         Index := Index + 1;

         if Index not in Value'Range then
            return Position.Index;
         elsif Position.Is_Leaf then
            return Trie.Not_Found;
         end if;

         Position := Position.Children (Value (Index));
      end loop;
   end Search;


   procedure Set_Dictionary_For_Map_Search (Dict : in Dictionary) is
   begin
      Dictionary_Maps.Clear (Search_Map);

      for I in Dict.Offsets'Range loop
         Dictionary_Maps.Insert (Search_Map, Dict_Entry (Dict, I), I);
      end loop;
   end Set_Dictionary_For_Map_Search;


   procedure Set_Dictionary_For_Trie_Search (Dict : in Dictionary) is
   begin
      Initialize (Trie_For_Search, Dict);
   end Set_Dictionary_For_Trie_Search;


   function Trie_Search (Value : String) return Natural is
   begin
      return Search (Trie_For_Search, Value);
   end Trie_Search;



   -------------------
   -- Word Counting --
   -------------------

   procedure Add_Substrings
     (Counter : in out Word_Counter;
      Phrase : in String;
      Min_Size : in Positive;
      Max_Size : in Positive) is
   begin
      for First in Phrase'First .. Phrase'Last - Min_Size + 1 loop
         for Last in First + Min_Size - 1
           .. Natural'Min (First + Max_Size - 1, Phrase'Last)
         loop
            Add_Word (Counter, Phrase (First .. Last));
         end loop;
      end loop;
   end Add_Substrings;


   procedure Add_Word
     (Counter : in out Word_Counter;
      Word : in String;
      Count : in String_Count := 1)
   is
      procedure Update
        (Key : in String; Element : in out String_Count);

      procedure Update
        (Key : in String; Element : in out String_Count)
      is
         pragma Unreferenced (Key);
      begin
         Element := Element + Count;
      end Update;

      Cursor : constant Word_Maps.Cursor := Word_Maps.Find (Counter.Map, Word);
   begin
      if Word_Maps.Has_Element (Cursor) then
         Word_Maps.Update_Element (Counter.Map, Cursor, Update'Access);
      else
         Word_Maps.Insert (Counter.Map, Word, Count);
      end if;
   end Add_Word;


   procedure Add_Words
     (Counter : in out Word_Counter;
      Phrase : in String;
      Min_Size : in Positive;
      Max_Size : in Positive)
   is
      subtype Word_Part is Character with Static_Predicate
        => Word_Part in '0' .. '9' | 'A' .. 'Z' | 'a' .. 'z'
                      | Character'Val (128) .. Character'Val (255);
      I, First, Next : Positive;
   begin
      if Max_Size < Min_Size then
         return;
      end if;

      I := Phrase'First;

      Main_Loop :
      while I in Phrase'Range loop
         Skip_Non_Word :
         while I in Phrase'Range and then Phrase (I) not in Word_Part loop
            I := I + 1;
         end loop Skip_Non_Word;

         exit Main_Loop when I not in Phrase'Range;
         First := I;

         Skip_Word :
         while I in Phrase'Range and then Phrase (I) in Word_Part loop
            I := I + 1;
         end loop Skip_Word;

         Next := I;

         if Next - First in Min_Size .. Max_Size then
            Add_Word (Counter, Phrase (First .. Next - 1));
         end if;
      end loop Main_Loop;
   end Add_Words;


   procedure Evaluate_Dictionary
     (Dict : in Dictionary;
      Corpus : in String_Lists.List;
      Compressed_Size : out Ada.Streams.Stream_Element_Count;
      Counts : out Dictionary_Counts) is
   begin
      Compressed_Size := 0;
      Counts := (others => 0);

      for S of Corpus loop
         Evaluate_Dictionary_Partial
           (Dict, S, Compressed_Size, Counts);
      end loop;
   end Evaluate_Dictionary;


   procedure Evaluate_Dictionary_Partial
     (Dict : in Dictionary;
      Corpus_Entry : in String;
      Compressed_Size : in out Ada.Streams.Stream_Element_Count;
      Counts : in out Dictionary_Counts)
   is
      use type Ada.Streams.Stream_Element_Offset;

      Verbatim_Code_Count : constant Ada.Streams.Stream_Element_Offset
        := Ada.Streams.Stream_Element_Offset
           (Ada.Streams.Stream_Element'Last - Dict.Dict_Last);

      Verbatim_Length : Ada.Streams.Stream_Element_Offset;
      Input_Byte : Ada.Streams.Stream_Element;
      Compressed : constant Ada.Streams.Stream_Element_Array
        := Compress (Dict, Corpus_Entry);
      Index : Ada.Streams.Stream_Element_Offset := Compressed'First;
   begin
      Compressed_Size := Compressed_Size + Compressed'Length;

      while Index in Compressed'Range loop
         Input_Byte := Compressed (Index);

         if Input_Byte in Dict.Offsets'Range then
            Counts (Input_Byte) := Counts (Input_Byte) + 1;
            Index := Index + 1;
         else
            if not Dict.Variable_Length_Verbatim then
               Verbatim_Length := Ada.Streams.Stream_Element_Offset
                 (Ada.Streams.Stream_Element'Last - Input_Byte) + 1;
            elsif Input_Byte < Ada.Streams.Stream_Element'Last then
               Verbatim_Length := Ada.Streams.Stream_Element_Offset
                 (Ada.Streams.Stream_Element'Last - Input_Byte);
            else
               Index := Index + 1;
               Verbatim_Length := Ada.Streams.Stream_Element_Offset
                 (Compressed (Index)) + Verbatim_Code_Count - 1;
            end if;

            Index := Index + Verbatim_Length + 1;
         end if;
      end loop;
   end Evaluate_Dictionary_Partial;


   procedure Filter_By_Count
     (Counter : in out Word_Counter;
      Threshold_Count : in String_Count)
   is
      Position, Next : Word_Maps.Cursor;
   begin
      Position := Word_Maps.First (Counter.Map);

      while Word_Maps.Has_Element (Position) loop
         Next := Word_Maps.Next (Position);

         if Word_Maps.Element (Position) < Threshold_Count then
            Word_Maps.Delete (Counter.Map, Position);
         end if;

         Position := Next;
      end loop;

      pragma Assert (for all Count of Counter.Map => Count >= Threshold_Count);
   end Filter_By_Count;


   function Simple_Dictionary
     (Counter : in Word_Counter;
      Word_Count : in Natural)
     return String_Lists.List
   is
      use type Ada.Containers.Count_Type;
      Target_Count : constant Ada.Containers.Count_Type
        := Ada.Containers.Count_Type (Word_Count);
      Set : Scored_Word_Sets.Set;
      Result : String_Lists.List;
   begin
      for Cursor in Word_Maps.Iterate (Counter.Map) loop
         Scored_Word_Sets.Include (Set, To_Scored_Word (Cursor));

         if Scored_Word_Sets.Length (Set) > Target_Count then
            Scored_Word_Sets.Delete_Last (Set);
         end if;
      end loop;

      for Cursor in Scored_Word_Sets.Iterate (Set) loop
         Result.Append (Scored_Word_Sets.Element (Cursor).Word);
      end loop;

      return Result;
   end Simple_Dictionary;


   procedure Simple_Dictionary_And_Pending
     (Counter : in Word_Counter;
      Word_Count : in Natural;
      Selected : out String_Lists.List;
      Pending : out String_Lists.List;
      Max_Pending_Count : in Ada.Containers.Count_Type
        := Ada.Containers.Count_Type'Last)
   is
      use type Ada.Containers.Count_Type;
      Target_Count : constant Ada.Containers.Count_Type
        := Ada.Containers.Count_Type (Word_Count);
      Set : Scored_Word_Sets.Set;
   begin
      for Cursor in Word_Maps.Iterate (Counter.Map) loop
         Scored_Word_Sets.Insert (Set, To_Scored_Word (Cursor));
      end loop;

      Selected := String_Lists.Empty_List;
      Pending := String_Lists.Empty_List;

      for Cursor in Scored_Word_Sets.Iterate (Set) loop
         if String_Lists.Length (Selected) < Target_Count then
            Selected.Append (Scored_Word_Sets.Element (Cursor).Word);
         else
            Pending.Append (Scored_Word_Sets.Element (Cursor).Word);
            exit when String_Lists.Length (Selected) >= Max_Pending_Count;
         end if;
      end loop;
   end Simple_Dictionary_And_Pending;


   function To_Scored_Word (Cursor : in Word_Maps.Cursor)
     return Scored_Word
   is
      Word : constant String := Word_Maps.Key (Cursor);
   begin
      return Scored_Word'
        (Size => Word'Length,
         Word => Word,
         Score => Score_Value (Word_Maps.Element (Cursor)) * Word'Length);
   end To_Scored_Word;


   function Worst_Index
     (Dict : in Dictionary;
      Counts : in Dictionary_Counts;
      Method : in Methods.Enum)
     return Ada.Streams.Stream_Element
   is
      Result : Ada.Streams.Stream_Element := 0;
      Worst_Score : Score_Value := Score_Encoded (Dict, Counts, 0);
      S : Score_Value;
   begin
      for I in 1 .. Dict.Dict_Last loop
         S := Score (Dict, Counts, I, Method);

         if S < Worst_Score then
            Result := I;
            Worst_Score := S;
         end if;
      end loop;

      return Result;
   end Worst_Index;

end Natools.Smaz.Tools;
