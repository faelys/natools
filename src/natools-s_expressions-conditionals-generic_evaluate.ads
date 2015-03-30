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

------------------------------------------------------------------------------
-- Natools.S_Expressions.Conditionals.Generic_Evaluate provides an          --
-- evaluation function based on elementary names and functions, allowing    --
-- boolean combination of them.                                             --
-- For example Simple_Evaluate might evaluate the word "is_empty", while    --
-- Parameteric_Evaluate might check "contains", then Generic_Evaluate       --
-- handles expressions like:                                                --
-- (or is-empty (contains part_1) (and (not is-empty) (contains part_2)))   --
------------------------------------------------------------------------------

with Natools.S_Expressions.Lockable;

generic
   type Context_Type (<>) is limited private;

   with function Parametric_Evaluate
     (Context : in Context_Type;
      Name : in Natools.S_Expressions.Atom;
      Arguments : in out Natools.S_Expressions.Lockable.Descriptor'Class)
     return Boolean;

   with function Simple_Evaluate
     (Context : in Context_Type;
      Name : in Natools.S_Expressions.Atom)
     return Boolean;

function Natools.S_Expressions.Conditionals.Generic_Evaluate
  (Context : in Context_Type;
   Expression : in out Lockable.Descriptor'Class)
  return Boolean;
pragma Pure (Natools.S_Expressions.Conditionals.Generic_Evaluate);
