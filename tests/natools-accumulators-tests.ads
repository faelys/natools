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

------------------------------------------------------------------------------
-- Natools.Accumulators.Tests is a test suite for any object implementing   --
-- the accumulator interface.                                               --
------------------------------------------------------------------------------

with Natools.Tests;

package Natools.Accumulators.Tests is
   pragma Preelaborate (Tests);

   procedure Test
     (Report : in out Natools.Tests.Reporter'Class;
      Accumulator : in out String_Accumulator'Class);
      --  Run a sequence of tests on Accumulator.
      --  This append a sequence of items to Report in the current section.

end Natools.Accumulators.Tests;
