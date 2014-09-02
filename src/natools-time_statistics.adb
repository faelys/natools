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

package body Natools.Time_Statistics is

   ----------------------
   -- Public Interface --
   ----------------------

   overriding procedure Add (Self : in out Summary; Measure : in Duration) is
   begin
      Self.Data.Add (Measure);
   end Add;



   ------------------------------
   -- Summary Protected Object --
   ------------------------------

   protected body Summary_Data is

      procedure Add (Measure : in Duration) is
      begin
         if Measure > Max then
            Max := Measure;
         end if;

         if Measure < Min then
            Min := Measure;
         end if;

         Count := Count + 1;

         Current_Mean := Current_Mean
           + (Measure - Current_Mean) / Duration (Count);
      end Add;


      function Minimum return Duration is
      begin
         return Min;
      end Minimum;


      function Maximum return Duration is
      begin
         return Max;
      end Maximum;


      function Mean return Duration is
      begin
         return Current_Mean;
      end Mean;


      function Sample_Count return Natural is
      begin
         return Count;
      end Sample_Count;

   end Summary_Data;

end Natools.Time_Statistics;
