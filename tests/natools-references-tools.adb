------------------------------------------------------------------------------
-- Copyright (c) 2013, Natacha Porté                                        --
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

package body Natools.References.Tools is

   function Is_Consistent (Left, Right : Reference) return Boolean is
   begin
      return (Left.Data = Right.Data) = (Left.Count = Right.Count);
   end Is_Consistent;


   function Is_Valid (Ref : Reference) return Boolean is
   begin
      return (Ref.Data = null) = (Ref.Count = null);
   end Is_Valid;


   function Count (Ref : Reference) return Natural is
   begin
      if Ref.Count /= null then
         return Natural (Ref.Count.all);
      else
         return 0;
      end if;
   end Count;

end Natools.References.Tools;
