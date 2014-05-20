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

procedure Natools.S_Expressions.Interpreter_Loop
  (Expression : in out Natools.S_Expressions.Lockable.Descriptor'Class;
   State : in out Shared_State;
   Context : in Shared_Context)
is
   Event : Events.Event := Expression.Current_Event;
   Lock : Lockable.Lock_State;
begin
   loop
      case Event is
         when Events.Add_Atom =>
            Dispatch_Without_Argument
              (State, Context, Expression.Current_Atom);

         when Events.Open_List =>
            Expression.Lock (Lock);
            begin
               Expression.Next (Event);
               if Event = Events.Add_Atom then
                  declare
                     Name : constant Atom := Expression.Current_Atom;
                  begin
                     Expression.Next;
                     Dispatch_With_Arguments
                       (State, Context, Name, Expression);
                  end;
               end if;
            exception
               when others =>
                  Expression.Unlock (Lock, False);
                  raise;
            end;
            Expression.Unlock (Lock);
            Event := Expression.Current_Event;
            exit when Event = Events.Error or Event = Events.End_Of_Input;

         when Events.Close_List | Events.End_Of_Input | Events.Error =>
            exit;
      end case;
      Expression.Next (Event);
   end loop;
end Natools.S_Expressions.Interpreter_Loop;
