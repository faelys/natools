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

package body Natools.Time_Statistics.Tests is

   procedure Check is new NT.Generic_Check
     (Natural, "=", Natural'Image, False);

   procedure Check is new NT.Generic_Check
     (Duration, "=", Duration'Image, False);


   -------------------------
   -- Complete Test Suite --
   -------------------------

   procedure All_Tests (Report : in out NT.Reporter'Class) is
   begin
      Summary_Accumulator (Report);
   end All_Tests;



   -----------------------
   -- Inidividual Tests --
   -----------------------

   procedure Summary_Accumulator (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Summary accumulator");
   begin
      declare
         Stats : Summary;
      begin
         Stats.Add (10.1);
         Stats.Add (9.9);

         Check (Test, 2, Stats.Sample_Count, "Sample count");
         Check (Test, 10.1, Stats.Maximum, "Maximum");
         Check (Test, 9.9, Stats.Minimum, "Minimum");
         Check (Test, 10.0, Stats.Mean, "Mean");

         Stats.Add (10.3);

         Check (Test, 3, Stats.Sample_Count, "Sample count");
         Check (Test, 10.3, Stats.Maximum, "Maximum");
         Check (Test, 9.9, Stats.Minimum, "Minimum");
         Check (Test, 10.1, Stats.Mean, "Mean");
      end;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Summary_Accumulator;

end Natools.Time_Statistics.Tests;
