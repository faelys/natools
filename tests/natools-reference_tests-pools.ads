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
-- Natools.Reference_Tests.Pools expands reference test suite with          --
-- reference pools.                                                         --
------------------------------------------------------------------------------

private with Natools.References.Pools;

package Natools.Reference_Tests.Pools is

   procedure All_Tests (Report : in out NT.Reporter'Class);

   procedure Bounded_Pool (Report : in out NT.Reporter'Class);
   procedure Static_Pool (Report : in out NT.Reporter'Class);
   procedure Unbounded_Pool (Report : in out NT.Reporter'Class);

private

   package Ref_Pools is new Refs.Pools;


   task type Pseudo_Process is
      entry Start (Target : in Refs.Reference; Amount : in Duration);
   end Pseudo_Process;

   procedure Bounded_Start
     (Process : in out Pseudo_Process;
      Pool : in out Ref_Pools.Pool;
      Amount : in Duration;
      Test : in out NT.Test;
      Expected_Instance : in Natural);

   procedure Unbounded_Start
     (Process : in out Pseudo_Process;
      Pool : in out Ref_Pools.Pool;
      Amount : in Duration;
      Test : in out NT.Test;
      Expected_Instance : in Natural);

end Natools.Reference_Tests.Pools;
