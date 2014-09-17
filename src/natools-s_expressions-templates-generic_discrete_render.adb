------------------------------------------------------------------------------
-- Copyright (c) 2014, Natacha Porté                                        --
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

procedure Natools.S_Expressions.Templates.Generic_Discrete_Render
  (Output : in out Ada.Streams.Root_Stream_Type'Class;
   Template : in out Lockable.Descriptor'Class;
   Value : in T)
is
   Current_Value : T := T'First;
   Event : Events.Event := Template.Current_Event;
begin
   loop
      case Event is
         when Events.Add_Atom =>
            if Current_Value = Value then
               Output.Write (Template.Current_Atom);
               return;
            end if;

         when Events.Open_List =>
            loop
               Template.Next (Event);

               case Event is
                  when Events.Add_Atom =>
                     Output.Write (Template.Current_Atom);
                  when others =>
                     return;
               end case;
            end loop;

         when Events.Close_List | Events.End_Of_Input | Events.Error =>
            exit;
      end case;

      Template.Next (Event);
      Current_Value := T'Succ (Current_Value);
   end loop;

   Output.Write (To_Atom (Default_Image (Value)));
end Natools.S_Expressions.Templates.Generic_Discrete_Render;
