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

package body Natools.Parallelism is

   procedure Single_Accumulator_Run
     (Global : in out Global_State;
      Task_Count : in Positive)
   is
      protected State is
         procedure Initialize (Job : out Job_State; Continue : out Boolean);
         procedure Next (Job : in out Job_State; Continue : out Boolean);
      end State;

      task type Worker is
      end Worker;

      protected body State is

         procedure Initialize (Job : out Job_State; Continue : out Boolean) is
         begin
            Continue := not Is_Finished (Global);

            if Continue then
               Initialize_Job (Global, Job);
            end if;
         end Initialize;

         procedure Next (Job : in out Job_State; Continue : out Boolean) is
         begin
            Gather_Result (Global, Job);
            Initialize (Job, Continue);
         end Next;

      end State;

      task body Worker is
         Job : Job_State;
         Continue : Boolean;
      begin
         State.Initialize (Job, Continue);

         while Continue loop
            Do_Job (Job);
            State.Next (Job, Continue);
         end loop;
      end Worker;

      Workers : array (1 .. Task_Count) of Worker;
      pragma Unreferenced (Workers);
   begin
      null;
   end Single_Accumulator_Run;



   procedure Per_Task_Accumulator_Run
     (Global : in out Global_State;
      Task_Count : in Positive)
   is
      protected State is
         procedure Get_Next_Job
           (Job : out Job_Description;
            Terminated : out Boolean);

         procedure Gather (Result : in Task_Result);
      end State;

      task type Worker is
      end Worker;

      protected body State is

         procedure Get_Next_Job
           (Job : out Job_Description;
            Terminated : out Boolean) is
         begin
            Get_Next_Job (Global, Job, Terminated);
         end Get_Next_Job;

         procedure Gather (Result : in Task_Result)  is
         begin
            Gather_Result (Global, Result);
         end Gather;

      end State;

      task body Worker is
         Job : Job_Description;
         Result : Task_Result;
         Terminated : Boolean;
      begin
         Initialize (Result);

         loop
            State.Get_Next_Job (Job, Terminated);
            exit when Terminated;
            Do_Job (Result, Job);
         end loop;

         State.Gather (Result);
      end Worker;

      Workers : array (1 .. Task_Count) of Worker;
      pragma Unreferenced (Workers);
   begin
      null;
   end Per_Task_Accumulator_Run;

end Natools.Parallelism;
