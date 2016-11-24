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

package body Natools.Smaz_Tools is

   package Sx renames Natools.S_Expressions;

   function Build_Node
     (Map : Dictionary_Maps.Map;
      Empty_Value : Natural)
     return Trie_Node;

   procedure Set_Map
     (Map : in out Dictionary_Maps.Map;
      List : in String_Lists.List);
      --  Set Map contents to match List by index number

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
         Result.Index := Dictionary_Maps.Element (Cursor);
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
                     Index => Dictionary_Maps.Element (Cursor));
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


   procedure Set_Map
     (Map : in out Dictionary_Maps.Map;
      List : in String_Lists.List)
   is
      I : Natural := 0;
   begin
      Dictionary_Maps.Clear (Map);

      for S of List loop
         Dictionary_Maps.Insert (Map, S, I);
         I := I + 1;
      end loop;
   end Set_Map;



   ----------------------
   -- Public Interface --
   ----------------------

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


   procedure Initialize
     (Trie : out Search_Trie;
      List : in String_Lists.List)
   is
      Map : Dictionary_Maps.Map;
      Not_Found : constant Natural := Natural (String_Lists.Length (List));
   begin
      Set_Map (Map, List);

      Trie := (Not_Found => Not_Found,
               Root => Build_Node (Map, Not_Found));
   end Initialize;


   function Linear_Search (Value : String) return Natural is
      Result : Natural := 0;
   begin
      for S of List_For_Linear_Search loop
         exit when S = Value;
         Result := Result + 1;
      end loop;

      return Result;
   end Linear_Search;


   function Map_Search (Value : String) return Natural is
      Cursor : constant Dictionary_Maps.Cursor
        := Dictionary_Maps.Find (Search_Map, Value);
   begin
      if Dictionary_Maps.Has_Element (Cursor) then
         return Natural (Dictionary_Maps.Element (Cursor));
      else
         return Natural (Dictionary_Maps.Length (Search_Map));
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


   procedure Set_Dictionary_For_Map_Search (List : in String_Lists.List) is
   begin
      Set_Map (Search_Map, List);
   end Set_Dictionary_For_Map_Search;


   procedure Set_Dictionary_For_Trie_Search (List : in String_Lists.List) is
   begin
      Initialize (Trie_For_Search, List);
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
      Word_Count : in Natural;
      Method : in Methods.Enum := Methods.Encoded)
     return String_Lists.List
   is
      use type Ada.Containers.Count_Type;
      Target_Count : constant Ada.Containers.Count_Type
        := Ada.Containers.Count_Type (Word_Count);
      Set : Scored_Word_Sets.Set;
      Result : String_Lists.List;
   begin
      for Cursor in Word_Maps.Iterate (Counter.Map) loop
         Scored_Word_Sets.Include (Set, To_Scored_Word (Cursor, Method));

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
      Method : in Methods.Enum := Methods.Encoded;
      Max_Pending_Count : in Ada.Containers.Count_Type
        := Ada.Containers.Count_Type'Last)
   is
      use type Ada.Containers.Count_Type;
      Target_Count : constant Ada.Containers.Count_Type
        := Ada.Containers.Count_Type (Word_Count);
      Set : Scored_Word_Sets.Set;
   begin
      for Cursor in Word_Maps.Iterate (Counter.Map) loop
         Scored_Word_Sets.Insert (Set, To_Scored_Word (Cursor, Method));
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


   function To_Scored_Word
     (Cursor : in Word_Maps.Cursor;
      Method : in Methods.Enum)
     return Scored_Word
   is
      Word : constant String := Word_Maps.Key (Cursor);
   begin
      return Scored_Word'
        (Size => Word'Length,
         Word => Word,
         Score => Score (Word_Maps.Element (Cursor), Word'Length, Method));
   end To_Scored_Word;

end Natools.Smaz_Tools;
