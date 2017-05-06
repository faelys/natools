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

package body Natools.Smaz_Generic.Tools is

   function Image (B : Boolean) return String;
      --  Return correctly-cased image of B


   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

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
        (Last_Code => Dictionary_Code'Succ (Dict.Last_Code),
         Values_Last => Dict.Values_Last + Value'Length,
         Variable_Length_Verbatim => Dict.Variable_Length_Verbatim,
         Max_Word_Length => Positive'Max (Dict.Max_Word_Length, Value'Length),
         Offsets => Dict.Offsets
                      & (Dictionary_Code'First => Dict.Values_Last + 1),
         Values => Dict.Values & Value,
         Hash => Smaz_Tools.Dummy_Hash'Access);
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
      function Offsets_Suffix (I : Dictionary_Code) return String;
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

      function Offsets_Suffix (I : Dictionary_Code) return String is
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
      Put_Line (First_Prefix & "Last_Code =>"
        & Dictionary_Code'Image (Dict.Last_Code) & ',');
      Put_Line (Prefix & "Values_Last =>"
        & Natural'Image (Dict.Values_Last) & ',');
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


   function Remove_Element
     (Dict : in Dictionary;
      Index : in Dictionary_Code)
     return Dictionary
   is
      Removed_Length : constant Positive := Dict_Entry_Length (Dict, Index);

      function New_Offsets return Offset_Array;
      function New_Values return String;

      function New_Offsets return Offset_Array is
         Result : Offset_Array
           (Dict.Offsets'First .. Dictionary_Code'Pred (Dict.Last_Code));
      begin
         for I in Result'Range loop
            if I < Index then
               Result (I) := Dict.Offsets (I);
            else
               Result (I) := Dict.Offsets (Dictionary_Code'Succ (I))
                               - Removed_Length;
            end if;
         end loop;

         return Result;
      end New_Offsets;

      function New_Values return String is
      begin
         if Index = Dictionary_Code'First then
            return Dict.Values
              (Dict.Offsets (Dictionary_Code'Succ (Index))
               .. Dict.Values'Last);
         elsif Index < Dict.Last_Code then
            return Dict.Values (1 .. Dict.Offsets (Index) - 1)
              & Dict.Values (Dict.Offsets (Dictionary_Code'Succ (Index))
                             .. Dict.Values'Last);
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
        (Last_Code => Dictionary_Code'Pred (Dict.Last_Code),
         Values_Last => Dict.Values_Last - Removed_Length,
         Variable_Length_Verbatim => Dict.Variable_Length_Verbatim,
         Max_Word_Length => New_Max_Word_Length,
         Offsets => New_Offsets,
         Values => New_Values,
         Hash => Smaz_Tools.Dummy_Hash'Access);
   end Remove_Element;


   function Replace_Element
     (Dict : in Dictionary;
      Index : in Dictionary_Code;
      Value : in String)
     return Dictionary
   is
      Removed_Length : constant Positive := Dict_Entry_Length (Dict, Index);
      Length_Delta : constant Integer := Value'Length - Removed_Length;

      function New_Offsets return Offset_Array;
      function New_Values return String;

      function New_Offsets return Offset_Array is
         Result : Offset_Array (Dict.Offsets'First .. Dict.Last_Code);
      begin
         for I in Result'Range loop
            if I <= Index then
               Result (I) := Dict.Offsets (I);
            else
               Result (I) := Dict.Offsets (I) + Length_Delta;
            end if;
         end loop;

         return Result;
      end New_Offsets;

      function New_Values return String is
      begin
         if Index = Dictionary_Code'First then
            return Value & Dict.Values
              (Dict.Offsets (Dictionary_Code'Succ (Index))
               .. Dict.Values'Last);
         elsif Index < Dict.Last_Code then
            return Dict.Values (1 .. Dict.Offsets (Index) - 1)
              & Value
              & Dict.Values (Dict.Offsets (Dictionary_Code'Succ (Index))
                             .. Dict.Values'Last);
         else
            return Dict.Values (1 .. Dict.Offsets (Index) - 1) & Value;
         end if;
      end New_Values;

      New_Max_Word_Length : Positive := Dict.Max_Word_Length;
   begin
      if Removed_Length = Dict.Max_Word_Length then
         New_Max_Word_Length := 1;
         for I in Dict.Offsets'Range loop
            if I /= Index
              and then Dict_Entry_Length (Dict, I) > New_Max_Word_Length
            then
               New_Max_Word_Length := Dict_Entry_Length (Dict, I);
            end if;
         end loop;
      end if;

      if New_Max_Word_Length < Value'Length then
         New_Max_Word_Length := Value'Length;
      end if;

      return Dictionary'
        (Last_Code => Dict.Last_Code,
         Values_Last => Dict.Values_Last + Length_Delta,
         Variable_Length_Verbatim => Dict.Variable_Length_Verbatim,
         Max_Word_Length => New_Max_Word_Length,
         Offsets => New_Offsets,
         Values => New_Values,
         Hash => Smaz_Tools.Dummy_Hash'Access);
   end Replace_Element;


   function To_Dictionary
     (List : in String_Lists.List;
      Variable_Length_Verbatim : in Boolean)
     return Dictionary
   is
      Code_After_Last : Dictionary_Code := Dictionary_Code'First;
      String_Size : Natural := 0;
      Max_Word_Length : Positive := 1;
   begin
      for S of List loop
         Code_After_Last := Dictionary_Code'Succ (Code_After_Last);
         String_Size := String_Size + S'Length;

         if S'Length > Max_Word_Length then
            Max_Word_Length := S'Length;
         end if;
      end loop;

      declare
         Last_Code : constant Dictionary_Code
           := Dictionary_Code'Pred (Code_After_Last);
         Offsets : Offset_Array
           (Dictionary_Code'Succ (Dictionary_Code'First) .. Last_Code);
         Values : String (1 .. String_Size);
         Current_Offset : Positive := 1;
         Current_Index : Dictionary_Code := Dictionary_Code'First;
         Next_Offset : Positive;
      begin
         for S of List loop
            if Current_Index in Offsets'Range then
               Offsets (Current_Index) := Current_Offset;
            end if;

            Next_Offset := Current_Offset + S'Length;
            Values (Current_Offset .. Next_Offset - 1) := S;
            Current_Offset := Next_Offset;
            Current_Index := Dictionary_Code'Succ (Current_Index);
         end loop;

         pragma Assert (Current_Index = Code_After_Last);
         pragma Assert (Current_Offset = String_Size + 1);

         return
           (Last_Code => Last_Code,
            Values_Last => String_Size,
            Variable_Length_Verbatim => Variable_Length_Verbatim,
            Max_Word_Length => Max_Word_Length,
            Offsets => Offsets,
            Values => Values,
            Hash => Smaz_Tools.Dummy_Hash'Access);
      end;
   end To_Dictionary;

   function To_String_List (Dict : in Dictionary) return String_Lists.List is
      Result : String_Lists.List;
   begin
      for Code in Dictionary_Code'First .. Dict.Last_Code loop
         String_Lists.Append (Result, Dict_Entry (Dict, Code));
      end loop;

      return Result;
   end To_String_List;



   ---------------------------
   -- Dictionary Evaluation --
   ---------------------------

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
      use type Smaz_Tools.String_Count;

      Verbatim_Length : Natural;
      Code : Dictionary_Code;
      Compressed : constant Ada.Streams.Stream_Element_Array
        := Compress (Dict, Corpus_Entry);
      Index : Ada.Streams.Stream_Element_Offset := Compressed'First;
   begin
      Compressed_Size := Compressed_Size + Compressed'Length;

      while Index in Compressed'Range loop
         Read_Code
           (Compressed, Index,
            Code, Verbatim_Length,
            Dict.Last_Code, Dict.Variable_Length_Verbatim);

         if Verbatim_Length > 0 then
            Skip_Verbatim (Compressed, Index, Verbatim_Length);
         else
            Counts (Code) := Counts (Code) + 1;
         end if;
      end loop;
   end Evaluate_Dictionary_Partial;


   function Worst_Index
     (Dict : in Dictionary;
      Counts : in Dictionary_Counts;
      Method : in Smaz_Tools.Methods.Enum)
     return Dictionary_Code
   is
      use type Smaz_Tools.Score_Value;

      Result : Dictionary_Code := Dictionary_Code'First;
      Worst_Score : Smaz_Tools.Score_Value
        := Score (Dict, Counts, Result, Method);
      S : Smaz_Tools.Score_Value;
   begin
      for I in Dictionary_Code'Succ (Dictionary_Code'First)
               .. Dict.Last_Code
      loop
         S := Score (Dict, Counts, I, Method);

         if S < Worst_Score then
            Result := I;
            Worst_Score := S;
         end if;
      end loop;

      return Result;
   end Worst_Index;

end Natools.Smaz_Generic.Tools;
