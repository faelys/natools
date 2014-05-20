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

------------------------------------------------------------------------------
-- Natools.S_Expressions.Interpreter_Loop is a generic procedure that       --
-- implements the inner loop of S-expression interpreters, leaving command  --
-- dispatching to the formal procedures.                                    --
------------------------------------------------------------------------------

with Natools.S_Expressions.Lockable;

generic
   type Shared_State (<>) is limited private;
   type Shared_Context (<>) is limited private;

   with procedure Dispatch_With_Arguments
     (State : in out Shared_State;
      Context : in Shared_Context;
      Name : in Atom;
      Arguments : in out Natools.S_Expressions.Lockable.Descriptor'Class)
     is null;

   with procedure Dispatch_Without_Argument
     (State : in out Shared_State;
      Context : in Shared_Context;
      Name : in Atom)
     is null;

procedure Natools.S_Expressions.Interpreter_Loop
  (Expression : in out Natools.S_Expressions.Lockable.Descriptor'Class;
   State : in out Shared_State;
   Context : in Shared_Context);
pragma Pure (Natools.S_Expressions.Interpreter_Loop);
