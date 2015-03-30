------------------------------------------------------------------------------
-- Copyright (c) 2015, Natacha Porté                                        --
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

with Natools.S_Expressions.Interpreter_Loop;

function Natools.S_Expressions.Conditionals.Generic_Evaluate
  (Context : in Context_Type;
   Expression : in out Lockable.Descriptor'Class)
  return Boolean
is
   type State_Type is record
      Result : Boolean;
      Conjunction : Boolean;
   end record;


   procedure Evaluate_Element
     (State : in out State_Type;
      Context : in Context_Type;
      Name : in Atom);
      --  Evaluate a name as part of an "and" or "or" operation

   procedure Evaluate_Element
     (State : in out State_Type;
      Context : in Context_Type;
      Name : in Atom;
      Arguments : in out Lockable.Descriptor'Class);
      --  Evaluate a name as part of an "and" or "or" operation

   function Internal_Evaluate
     (Context : in Context_Type;
      Name : in Atom)
     return Boolean;
      --  Evaluate a boolean name or a context name

   function Internal_Evaluate
     (Context : in Context_Type;
      Name : in Atom;
      Arguments : in out Lockable.Descriptor'Class)
     return Boolean;
      --  Evaluate a boolean function or a context function


   procedure Run is new Natools.S_Expressions.Interpreter_Loop
     (State_Type, Context_Type, Evaluate_Element, Evaluate_Element);


   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

   procedure Evaluate_Element
     (State : in out State_Type;
      Context : in Context_Type;
      Name : in Atom) is
   begin
      if State.Result = State.Conjunction then
         State.Result := Internal_Evaluate (Context, Name);
      end if;
   end Evaluate_Element;


   procedure Evaluate_Element
     (State : in out State_Type;
      Context : in Context_Type;
      Name : in Atom;
      Arguments : in out Lockable.Descriptor'Class) is
   begin
      if State.Result = State.Conjunction then
         State.Result := Internal_Evaluate (Context, Name, Arguments);
      end if;
   end Evaluate_Element;


   function Internal_Evaluate
     (Context : in Context_Type;
      Name : in Atom)
     return Boolean
   is
      S_Name : constant String := To_String (Name);
   begin
      if S_Name = "true" then
         return True;
      elsif S_Name = "false" then
         return False;
      else
         return Simple_Evaluate (Context, Name);
      end if;
   end Internal_Evaluate;


   function Internal_Evaluate
     (Context : in Context_Type;
      Name : in Atom;
      Arguments : in out Lockable.Descriptor'Class)
     return Boolean
   is
      State : State_Type;
      S_Name : constant String := To_String (Name);
   begin
      if S_Name = "and" then
         State := (True, True);
         Run (Arguments, State, Context);
         return State.Result;

      elsif S_Name = "or" then
         State := (False, False);
         Run (Arguments, State, Context);
         return State.Result;

      elsif S_Name = "not" then
         return not Generic_Evaluate (Context, Arguments);

      else
         return Parametric_Evaluate (Context, Name, Arguments);
      end if;
   end Internal_Evaluate;


   -------------------
   -- Function Body --
   -------------------

   Event : Events.Event;
   Lock : Lockable.Lock_State;
   Result : Boolean;
begin
   case Expression.Current_Event is
      when Events.Add_Atom =>
         Result := Internal_Evaluate (Context, Expression.Current_Atom);

      when Events.Open_List =>
         Expression.Lock (Lock);
         begin
            Expression.Next (Event);
            if Event = Events.Add_Atom then
               declare
                  Name : constant Atom := Expression.Current_Atom;
               begin
                  Expression.Next (Event);
                  Result := Internal_Evaluate (Context, Name, Expression);
               end;
            end if;
         exception
            when others =>
               Expression.Unlock (Lock, False);
               raise;
         end;
         Expression.Unlock (Lock);

      when Events.Close_List | Events.Error | Events.End_Of_Input =>
         raise Constraint_Error with "Conditional on empty expression";
   end case;

   return Result;
end Natools.S_Expressions.Conditionals.Generic_Evaluate;
