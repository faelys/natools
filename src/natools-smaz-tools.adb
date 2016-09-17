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

package body Natools.Smaz.Tools is

   package Sx renames Natools.S_Expressions;

   function Dummy_Hash (Value : String) return Natural;
      --  Placeholder for Hash member, always raises Program_Error

   function Image (B : Boolean) return String;
      --  Return correctly-cased image of B


   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

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

end Natools.Smaz.Tools;