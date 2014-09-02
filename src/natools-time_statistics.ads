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
-- Natools.Time_Statistics is the root of various utilities to measure time --
-- and gather statistics.                                                   --
-- It also provides basic constant-space accumulator that keeps a summary   --
-- of seen values.                                                          --
------------------------------------------------------------------------------

package Natools.Time_Statistics is
   pragma Pure;

   type Accumulator is limited interface;

   procedure Add (Self : in out Accumulator; Measure : in Duration)
     is abstract;
      --  Add Measure to the accumulated statistics in Self



   type Summary is limited new Accumulator with private;
      --  Constant-space aggregator of duration values

   overriding procedure Add (Self : in out Summary; Measure : in Duration);
      --  Add Measure to the accumulated statistics

   not overriding function Minimum (Self : Summary) return Duration;
   not overriding function Maximum (Self : Summary) return Duration;
   not overriding function Mean (Self : Summary) return Duration;
   not overriding function Sample_Count (Self : Summary) return Natural;

private

   protected type Summary_Data is
      procedure Add (Measure : in Duration);
      function Minimum return Duration;
      function Maximum return Duration;
      function Mean return Duration;
      function Sample_Count return Natural;
   private
      Max : Duration := Duration'First;
      Min : Duration := Duration'Last;
      Current_Mean : Duration := 0.0;
      Count : Natural := 0;
   end Summary_Data;


   type Summary is limited new Accumulator with record
      Data : Summary_Data;
   end record;

   not overriding function Minimum (Self : Summary) return Duration
     is (Self.Data.Minimum);

   not overriding function Maximum (Self : Summary) return Duration
     is (Self.Data.Maximum);

   not overriding function Mean (Self : Summary) return Duration
     is (Self.Data.Mean);

   not overriding function Sample_Count (Self : Summary) return Natural
     is (Self.Data.Sample_Count);

end Natools.Time_Statistics;
