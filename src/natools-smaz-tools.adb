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

end Natools.Smaz.Tools;
