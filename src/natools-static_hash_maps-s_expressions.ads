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
-- Natools.Static_Hash_Maps.S_Expressions provides subprograms to read      --
-- S-expression descriptions of static hash maps and feed it to             --
-- Natools.Static_Hash_Maps.                                                --
------------------------------------------------------------------------------

with Natools.S_Expressions.Lockable;

package Natools.Static_Hash_Maps.S_Expressions is

   package Sx renames Natools.S_Expressions;

   procedure Generate_Packages
     (Input : in out Sx.Lockable.Descriptor'Class;
      Description : in String := "");
      --  Generate all hash map packages described in Input

   procedure Generate_Package
     (Input : in out Sx.Lockable.Descriptor'Class;
      Description : in String := "");
      --  Generate the package described by Input

private

   type Package_Command is
     (Private_Child,
      Public_Child,
      Extra_Declarations);

   type Map_Command is
     (Hash_Package,
      Nodes,
      Function_Name,
      Not_Found);

end Natools.Static_Hash_Maps.S_Expressions;
