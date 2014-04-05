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
-- Natools.GNAT_HMAC is a root for HMAC instances using hash functions      --
-- provided by GNAT. Is also provides glue functions.                       --
------------------------------------------------------------------------------

with Ada.Streams;

with GNAT.MD5;
with GNAT.SHA1;
with GNAT.SHA256;

package Natools.GNAT_HMAC is

   generic
      type Context is private;
      with function Hex_Digest (C : Context) return String;
   function Generic_Digest (C : Context)
     return Ada.Streams.Stream_Element_Array;

   function Digest (C : GNAT.MD5.Context)
     return Ada.Streams.Stream_Element_Array;

   function Digest (C : GNAT.SHA1.Context)
     return Ada.Streams.Stream_Element_Array;

   function Digest (C : GNAT.SHA256.Context)
     return Ada.Streams.Stream_Element_Array;

end Natools.GNAT_HMAC;
