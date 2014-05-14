------------------------------------------------------------------------------
-- Copyright (c) 2013-2014, Natacha Porté                                   --
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
-- Natools.S_Expressions.Interpreters provides an implemntation of a        --
-- dispatching command interpreter. The base list of a given S-expression   --
-- is considered as list of command, either argumentless (atoms) or with a  --
-- S-expression argument (sublist). Sublists that don't start with an atom  --
-- are silently ignored and can be used as comments.                        --
--                                                                          --
-- Formal types represent common objets for all the command, Shared_State   --
-- begin read/write while Shared_Context is read-only.                      --
------------------------------------------------------------------------------

with Natools.S_Expressions.Lockable;

private with Ada.Containers.Indefinite_Ordered_Maps;
private with Natools.S_Expressions.Atom_Refs;

generic
   type Shared_State (<>) is limited private;
   type Shared_Context (<>) is limited private;

package Natools.S_Expressions.Interpreters is
   pragma Preelaborate (Interpreters);

   Command_Not_Found : exception;


   type Command is interface;

   procedure Execute
     (Self : in Command;
      State : in out Shared_State;
      Context : in Shared_Context;
      Name : in Atom)
     is null;
      --  Execute a single argumentless command

   procedure Execute
     (Self : in Command;
      State : in out Shared_State;
      Context : in Shared_Context;
      Cmd : in out Lockable.Descriptor'Class)
     is null;
      --  Execute a single command with arguments


   type Null_Command is new Command with null record;

   Do_Nothing : Null_Command := Null_Command'(null record);


   type Interpreter is new Command with private;

   procedure Add_Command
     (Self : in out Interpreter;
      Name : in Atom;
      Cmd : in Command'Class);

   procedure Add
     (Self : in out Interpreter;
      Name : in String;
      Cmd : in Command'Class);

   function Has_Command (Self : Interpreter; Name : Atom) return Boolean;

   function Is_Empty (Self : Interpreter) return Boolean;

   procedure Set_Fallback
     (Self : in out Interpreter;
      Name : in Atom);

   procedure Reset_Fallback (Self : in out Interpreter);

   not overriding procedure Execute
     (Self : in Interpreter;
      Expression : in out Lockable.Descriptor'Class;
      State : in out Shared_State;
      Context : in Shared_Context);
      --  Execute an expression, raising Command_Not_Found on unknown commands

   not overriding procedure Execute
     (Self : in Interpreter;
      Fallback : in Command'Class;
      Expression : in out Lockable.Descriptor'Class;
      State : in out Shared_State;
      Context : in Shared_Context);
      --  Execute an expression with temporary fallback for unknown commands

   overriding procedure Execute
     (Self : in Interpreter;
      State : in out Shared_State;
      Context : in Shared_Context;
      Name : in Atom);
      --  Execute a single argumentless command

   overriding procedure Execute
     (Self : in Interpreter;
      State : in out Shared_State;
      Context : in Shared_Context;
      Cmd : in out Lockable.Descriptor'Class);
      --  Execute a single command with arguments


private

   type Exception_Command is new Command with null record;

   package Command_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Atom, Command'Class, Less_Than);

   type Interpreter is new Command with record
      Commands : Command_Maps.Map;
      Max_Length : Count := 0;
      Fallback_Name : Atom_Refs.Reference;
   end record;

end Natools.S_Expressions.Interpreters;
