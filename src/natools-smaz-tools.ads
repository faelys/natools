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

------------------------------------------------------------------------------
-- Natools.Smaz.Tools helps building and generating dictionary for use      --
-- with its parent package. Note that the dictionary is intended to be      --
-- generated and hard-coded, so the final client shouldn't need this        --
-- package.                                                                 --
------------------------------------------------------------------------------

with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Natools.S_Expressions;

package Natools.Smaz.Tools is
   pragma Preelaborate;

   package String_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists
     (String);

   procedure Read_List
     (List : out String_Lists.List;
      Descriptor : in out S_Expressions.Descriptor'Class);
      --  Read atoms from Descriptor to fill List

end Natools.Smaz.Tools;
