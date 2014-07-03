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
-- HMAC.Pinentry encapsulates communication with `pinentry` utility from    --
-- GnuPG project.                                                           --
-- Depending on communication features available, the package might not be  --
-- functional on all targets. A client should use Is_Available function to  --
-- ensure the underlying implentation is indeed operational.                --
------------------------------------------------------------------------------

package HMAC.Pinentry is

   Backend_Error : exception;

   function Get_Key (Command : String) return String;
      --  Run the given Command and communicate with it using pinentry protocol
      --  and return a secret String or raise Backend_Error.

   function Is_Available return Boolean;
      --  Check whether Get_Key can actually work

end HMAC.Pinentry;
