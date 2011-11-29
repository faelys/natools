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

package body Natools.Accumulators.String_Accumulator_Linked_Lists is

   procedure Initialize_If_Needed
     (Object : in out String_Accumulator_Linked_List) is
   begin
      if Object.Stack.Is_Empty then
         Object.Stack.Append (Object.Build (1));
         Object.Position := Object.Stack.Last;
      end if;
   end Initialize_If_Needed;



   procedure Append (To   : in out String_Accumulator_Linked_List;
                     Text : String)
   is
      procedure Process (Element : in out String_Accumulator'Class);

      procedure Process (Element : in out String_Accumulator'Class) is
      begin
         Element.Append (Text);
      end Process;
   begin
      Initialize_If_Needed (To);
      To.Stack.Update_Element (To.Position, Process'Access);
   end Append;



   procedure Hard_Reset (Acc : in out String_Accumulator_Linked_List) is
   begin
      Acc.Stack.Clear;
      Acc.Position := Lists.No_Element;
   end Hard_Reset;



   function Length (Acc : String_Accumulator_Linked_List) return Natural is
      procedure Process (Element : String_Accumulator'Class);

      Result : Natural;

      procedure Process (Element : String_Accumulator'Class) is
      begin
         Result := Element.Length;
      end Process;
   begin
      if Acc.Stack.Is_Empty then
         return 0;
      else
         Lists.Query_Element (Acc.Position, Process'Access);
         return Result;
      end if;
   end Length;



   procedure Push (Acc : in out String_Accumulator_Linked_List) is
      procedure Process (Element : in out String_Accumulator'Class);

      use type Lists.Cursor;

      procedure Process (Element : in out String_Accumulator'Class) is
      begin
         Soft_Reset (Element);
      end Process;
   begin
      Initialize_If_Needed (Acc);
      Lists.Next (Acc.Position);
      if Acc.Position = Lists.No_Element then
         declare
            Level_Created : constant Positive
              := Natural (Acc.Stack.Length) + 1;
         begin
            Acc.Stack.Append (Acc.Build (Level_Created));
            Acc.Position := Acc.Stack.Last;
         end;
      else
         Acc.Stack.Update_Element (Acc.Position, Process'Access);
      end if;
   end Push;



   procedure Pop (Acc : in out String_Accumulator_Linked_List) is
      use type Lists.Cursor;
   begin
      if Acc.Stack.Is_Empty then
         raise Program_Error;
      end if;
      Lists.Previous (Acc.Position);
      if Acc.Position = Lists.No_Element then
         Acc.Position := Lists.First (Acc.Stack);
         raise Program_Error;
      end if;
   end Pop;



   procedure Soft_Reset (Acc : in out String_Accumulator_Linked_List) is
      procedure Process (Element : in out String_Accumulator'Class);

      procedure Process (Element : in out String_Accumulator'Class) is
      begin
         Element.Soft_Reset;
      end Process;
   begin
      Initialize_If_Needed (Acc);
      Acc.Position := Lists.First (Acc.Stack);
      Acc.Stack.Update_Element (Acc.Position, Process'Access);
   end Soft_Reset;



   function Tail (Acc : String_Accumulator_Linked_List; Size : Natural)
      return String
   is
      procedure Process (Element : String_Accumulator'Class);

      Result : String (1 .. Size);
      Actual_Size : Natural;

      procedure Process (Element : String_Accumulator'Class)
      is
         Output : constant String := Tail (Element, Size);
      begin
         Actual_Size := Output'Length;
         Result (1 .. Actual_Size) := Output;
      end Process;
   begin
      if Acc.Stack.Is_Empty then
         return "";
      else
         Lists.Query_Element (Acc.Position, Process'Access);
         return Result (1 .. Actual_Size);
      end if;
   end Tail;



   function To_String (Acc : String_Accumulator_Linked_List) return String is
   begin
      if Acc.Stack.Is_Empty then
         return "";
      end if;

      declare
         procedure Process (Element : String_Accumulator'Class);

         Result : String (1 .. Acc.Length);

         procedure Process (Element : String_Accumulator'Class) is
         begin
            Result := Element.To_String;
         end Process;
      begin
         Lists.Query_Element (Acc.Position, Process'Access);
         return Result;
      end;
   end To_String;



   procedure To_String (Acc : String_Accumulator_Linked_List;
                        Output : out String) is
   begin
      if Acc.Stack.Is_Empty then
         return;
      end if;

      declare
         procedure Process (Element : String_Accumulator'Class);

         procedure Process (Element : String_Accumulator'Class) is
         begin
            Element.To_String (Output);
         end Process;
      begin
         Lists.Query_Element (Acc.Position, Process'Access);
      end;
   end To_String;



   procedure Unappend (From : in out String_Accumulator_Linked_List;
                       Text : String)
   is
      procedure Process (Element : in out String_Accumulator'Class);

      procedure Process (Element : in out String_Accumulator'Class) is
      begin
         Element.Unappend (Text);
      end Process;
   begin
      if not From.Stack.Is_Empty then
         From.Stack.Update_Element (From.Position, Process'Access);
      end if;
   end Unappend;

end Natools.Accumulators.String_Accumulator_Linked_Lists;
