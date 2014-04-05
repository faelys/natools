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

with Natools.S_Expressions.Encodings;

package body Natools.GNAT_HMAC is

   ----------------------------
   -- Generic Implementation --
   ----------------------------

   function Generic_Digest (C : Context)
     return Ada.Streams.Stream_Element_Array is
   begin
      return S_Expressions.Encodings.Decode_Hex
        (S_Expressions.To_Atom (Hex_Digest (C)));
   end Generic_Digest;


   ---------------------------
   -- Specialized Instances --
   ---------------------------

   function MD5_Digest is new Generic_Digest
     (GNAT.MD5.Context, GNAT.MD5.Digest);

   function Digest (C : GNAT.MD5.Context)
     return Ada.Streams.Stream_Element_Array
     renames MD5_Digest;

   function SHA1_Digest is new Generic_Digest
     (GNAT.SHA1.Context, GNAT.SHA1.Digest);

   function Digest (C : GNAT.SHA1.Context)
     return Ada.Streams.Stream_Element_Array
     renames SHA1_Digest;

   function SHA256_Digest is new Generic_Digest
     (GNAT.SHA256.Context, GNAT.SHA256.Digest);

   function Digest (C : GNAT.SHA256.Context)
     return Ada.Streams.Stream_Element_Array
     renames SHA256_Digest;

end Natools.GNAT_HMAC;
