------------------------------------------------------------------------------
-- Copyright (c) 2016, Natacha Porté                                        --
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
-- Natools.Parallelism provides generic procedures to help with simple      --
-- parallelisation needs.                                                   --
------------------------------------------------------------------------------

package Natools.Parallelism is
   pragma Pure;

   generic
      type Global_State (<>) is limited private;
         --  State common to all jobs, only accessed from protected subprograms

      type Job_State is limited private;
         --  State of a single job, each worker task having its own

      with procedure Initialize_Job
        (Global : in out Global_State;
         Job : out Job_State) is <>;
         --  Initialize Job and update Global as needed

      with procedure Do_Job (Job : in out Job_State) is <>;
         --  Perform the job in parallel

      with procedure Gather_Result
        (Global : in out Global_State;
         Job : in Job_State) is <>;
         --  Update Global with results stored in Job

      with function Is_Finished (Global : in Global_State)
        return Boolean is <>;
         --  Check whether there is still a job to do

   procedure Single_Accumulator_Run
     (Global : in out Global_State;
      Task_Count : in Positive);



   generic
      type Global_State (<>) is limited private;
         --  State common to all jobs, only accessed from protected subprograms

      type Task_Result is limited private;
         --  Accumulated result in a single task

      type Job_Description is limited private;
         --  Parameters for a given job

      with procedure Initialize (Result : in out Task_Result) is <>;
         --  Initialize Result for the current task

      with procedure Get_Next_Job
        (Global : in out Global_State;
         Job : out Job_Description;
         Terminated : out Boolean) is <>;
         --  If there is a next job available from Global, set Terminated
         --  to False and initialize Job, otherwise set Terminated to True.

      with procedure Do_Job
        (Result : in out Task_Result;
         Job : in Job_Description) is <>;
         --  Perform the job in parallel

      with procedure Gather_Result
        (Global : in out Global_State;
         Partial : in Task_Result) is <>;
         --  Update Global with results stored in Partial

   procedure Per_Task_Accumulator_Run
     (Global : in out Global_State;
      Task_Count : in Positive);

end Natools.Parallelism;
