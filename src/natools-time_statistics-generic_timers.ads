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
------------------------------------------------------------------------------

private with Ada.Finalization;

generic
   type Time is private;
   with function Now return Time;
   with function "-" (Left, Right : Time) return Duration;

package Natools.Time_Statistics.Generic_Timers is

   type Manual_Timer (Backend : access Accumulator'Class)
     is tagged limited private;
      --  Timer that must be manually started and stopped

   not overriding function Is_Running (Timer : Manual_Timer) return Boolean;
      --  Return whether Timer is currently running

   not overriding procedure Start (Timer : in out Manual_Timer)
     with Pre => not Is_Running (Timer) or else raise Constraint_Error;
      --  Start measuring time

   not overriding procedure Stop (Timer : in out Manual_Timer)
     with Pre => Is_Running (Timer) or else raise Constraint_Error;
      --  Stop Timer and add the measured duration to the backend

   not overriding procedure Cancel (Timer : in out Manual_Timer);
      --  If Timer is running, stop it without reporting to backend



   type Auto_Timer (Backend : access Accumulator'Class)
     is tagged limited private;
      --  Measure time between object creation and finalization

   not overriding procedure Cancel (Timer : in out Auto_Timer);
      --  Prevent the Timer from reporting measured time on finalization

private

   type Manual_Timer (Backend : access Accumulator'Class)
     is tagged limited record
      Start_Time : Time;
      Running : Boolean := False;
   end record;

   not overriding function Is_Running (Timer : Manual_Timer) return Boolean
     is (Timer.Running);


   type Auto_Timer (Backend : access Accumulator'Class)
     is new Ada.Finalization.Limited_Controlled
   with record
      Start_Time : Time;
      Reported : Boolean := False;
   end record;

   overriding procedure Initialize (Object : in out Auto_Timer);
   overriding procedure Finalize (Object : in out Auto_Timer);

end Natools.Time_Statistics.Generic_Timers;
