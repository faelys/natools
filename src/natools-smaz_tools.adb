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

package body Natools.Smaz_Tools is

   package Sx renames Natools.S_Expressions;


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
