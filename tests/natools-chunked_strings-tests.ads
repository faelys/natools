------------------------------------------------------------------------------
-- Copyright (c) 2011, Natacha Porté                                        --
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
-- Natools.Chunked_Strings.Tests is the test suite for Chunked_String.      --
--                                                                          --
-- It currently contains only black-box tests (i.e. without any assumption  --
-- on the internal implementaiton), taken from Unbounded_String tests in    --
-- ACATS.                                                                   --
--                                                                          --
-- It also provides private helper functions used in more specialized test  --
-- packages.                                                                --
------------------------------------------------------------------------------

with Natools.Tests;

generic package Natools.Chunked_Strings.Tests is
   pragma Preelaborate (Tests);

   procedure All_Blackbox_Tests (Report : in out Natools.Tests.Reporter'Class);

   procedure All_Tests (Report : in out Natools.Tests.Reporter'Class);

private

   procedure Dump (Report : in out Natools.Tests.Reporter'Class;
                   Dumped : in     Chunked_String);

   procedure Test (Report    : in out Natools.Tests.Reporter'Class;
                   Test_Name : in     String;
                   Computed  : in     Chunked_String;
                   Reference : in     String);

   procedure Test (Report    : in out Natools.Tests.Reporter'Class;
                   Test_Name : in     String;
                   Computed  : in     Chunked_String;
                   Reference : in     Chunked_String);

   procedure Test (Report    : in out Natools.Tests.Reporter'Class;
                   Test_Name : in     String;
                   Computed  : in     Natural;
                   Reference : in     Natural);

end Natools.Chunked_Strings.Tests;
