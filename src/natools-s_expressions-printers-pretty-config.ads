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
------------------------------------------------------------------------------

with Natools.S_Expressions.Lockable;

package Natools.S_Expressions.Printers.Pretty.Config is
   pragma Preelaborate (Config);


   procedure Update
     (Param : in out Parameters;
      Expression : in out Lockable.Descriptor'Class);
      --  Update parameters using a temporary interpreter

   procedure Print
     (Output : in out Printers.Printer'Class;
      Param : in Parameters);
      --  Output parameters to S-expression printer

end Natools.S_Expressions.Printers.Pretty.Config;
