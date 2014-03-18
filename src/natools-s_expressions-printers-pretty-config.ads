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
-- Natools.S_Expressions.Printers.Pretty.Config provides serialization and  --
-- deserialization of pretty printer parameters to and from S-expressions.  --
--                                                                          --
-- The interpreter uses a dummy Boolean context that is always ignored.     --
------------------------------------------------------------------------------

with Natools.S_Expressions.Interpreters;
with Natools.S_Expressions.Lockable;

package Natools.S_Expressions.Printers.Pretty.Config is
   pragma Preelaborate (Config);

   package Interpreters is
     new Natools.S_Expressions.Interpreters (Parameters, Boolean);


   --------------------------
   -- High-Level Interface --
   --------------------------

   function Config_Interpreter return Interpreters.Interpreter;
      --  Build a parameter interpreter

   procedure Update
     (Param : in out Parameters;
      Expression : in out Lockable.Descriptor'Class);
      --  Update parameters using a temporary interpreter

   procedure Update
     (Interpreter : in out Interpreters.Interpreter;
      Param : in out Parameters;
      Expression : in out Lockable.Descriptor'Class);
      --  Update parameters using Interpreter (wrapper around its Execute)


   ---------------------
   -- Building Blocks --
   ---------------------

   procedure Add_Char_Encoding_Commands
     (Interpreter : in out Interpreters.Interpreter);
   procedure Add_Hex_Casing_Commands
     (Interpreter : in out Interpreters.Interpreter);
   procedure Add_Quoted_Commands
     (Interpreter : in out Interpreters.Interpreter);
   procedure Add_Quoted_Escape_Commands
     (Interpreter : in out Interpreters.Interpreter);
   procedure Add_Newline_Encoding_Commands
     (Interpreter : in out Interpreters.Interpreter);
   procedure Add_Separator_Commands
     (Interpreter : in out Interpreters.Interpreter;
      Value : in Boolean;
      Newline : in Boolean);
      --  Inject commands into subinterpreter

   type Set_Width is new Interpreters.Command with null record;
   procedure Execute
     (Self : in out Set_Width;
      State : in out Parameters;
      Context : in Boolean;
      Name : in Atom);
   procedure Execute
     (Self : in out Set_Width;
      State : in out Parameters;
      Context : in Boolean;
      Cmd : in out Lockable.Descriptor'Class);

   type Set_Space_At is new Interpreters.Command with record
      Subinterpreter : Interpreters.Interpreter;
   end record;
   procedure Execute
     (Self : in out Set_Space_At;
      State : in out Parameters;
      Context : in Boolean;
      Cmd : in out Lockable.Descriptor'Class);

   type Set_Tab_Stop is new Interpreters.Command with null record;
   procedure Execute
     (Self : in out Set_Tab_Stop;
      State : in out Parameters;
      Context : in Boolean;
      Cmd : in out Lockable.Descriptor'Class);

   type Set_Indentation is new Interpreters.Command with null record;
   procedure Execute
     (Self : in out Set_Indentation;
      State : in out Parameters;
      Context : in Boolean;
      Name : in Atom);
   procedure Execute
     (Self : in out Set_Indentation;
      State : in out Parameters;
      Context : in Boolean;
      Cmd : in out Lockable.Descriptor'Class);

   type Set_Quoted is new Interpreters.Command with record
      Value : Quoted_Option;
   end record;
   procedure Execute
     (Self : in out Set_Quoted;
      State : in out Parameters;
      Context : in Boolean;
      Name : in Atom);

   type Set_Token is new Interpreters.Command with record
      Value : Token_Option;
   end record;
   procedure Execute
     (Self : in out Set_Token;
      State : in out Parameters;
      Context : in Boolean;
      Name : in Atom);
   procedure Execute
     (Self : in out Set_Token;
      State : in out Parameters;
      Context : in Boolean;
      Cmd : in out Lockable.Descriptor'Class);

   type Set_Hex_Casing is new Interpreters.Command with record
      Value : Encodings.Hex_Casing;
   end record;
   procedure Execute
     (Self : in out Set_Hex_Casing;
      State : in out Parameters;
      Context : in Boolean;
      Name : in Atom);

   type Set_Quoted_Escape is new Interpreters.Command with record
      Value : Quoted_Escape_Type;
   end record;
   procedure Execute
     (Self : in out Set_Quoted_Escape;
      State : in out Parameters;
      Context : in Boolean;
      Name : in Atom);

   type Set_Char_Encoding is new Interpreters.Command with record
      Value : Character_Encoding;
   end record;
   procedure Execute
     (Self : in out Set_Char_Encoding;
      State : in out Parameters;
      Context : in Boolean;
      Name : in Atom);

   type Set_Fallback is new Interpreters.Command with record
      Value : Atom_Encoding;
   end record;
   procedure Execute
     (Self : in out Set_Fallback;
      State : in out Parameters;
      Context : in Boolean;
      Name : in Atom);

   type Set_Newline is new Interpreters.Command with record
      Subinterpreter : Interpreters.Interpreter;
   end record;
   procedure Execute
     (Self : in out Set_Newline;
      State : in out Parameters;
      Context : in Boolean;
      Cmd : in out Lockable.Descriptor'Class);

   type Set_Newline_Encoding is new Interpreters.Command with record
      Value : Newline_Encoding;
   end record;
   procedure Execute
     (Self : in out Set_Newline_Encoding;
      State : in out Parameters;
      Context : in Boolean;
      Name : in Atom);

   type Set_Separator is new Interpreters.Command with record
      Before  : Entity;
      After   : Entity;
      Value   : Boolean;
      Newline : Boolean;
   end record;
   procedure Execute
     (Self : in out Set_Separator;
      State : in out Parameters;
      Context : in Boolean;
      Name : in Atom);

   type Set_All_Separators is new Interpreters.Command with record
      Value   : Boolean;
      Newline : Boolean;
   end record;
   procedure Execute
     (Self : in out Set_All_Separators;
      State : in out Parameters;
      Context : in Boolean;
      Name : in Atom);
   procedure Execute
     (Self : in out Set_All_Separators;
      State : in out Parameters;
      Context : in Boolean;
      Cmd : in out Lockable.Descriptor'Class);

   type Set_Quoted_String is new Interpreters.Command with record
      Subinterpreter : Interpreters.Interpreter;
   end record;
   procedure Execute
     (Self : in out Set_Quoted_String;
      State : in out Parameters;
      Context : in Boolean;
      Cmd : in out Lockable.Descriptor'Class);

end Natools.S_Expressions.Printers.Pretty.Config;
