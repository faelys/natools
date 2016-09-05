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


   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

   function Dummy_Hash (Value : String) return Natural is
      pragma Unreferenced (Value);
   begin
      raise Program_Error with "Dummy_Hash called";
      return 0;
   end Dummy_Hash;



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
