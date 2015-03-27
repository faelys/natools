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

package body Natools.Time_Statistics.Generic_Timers is

   ------------------
   -- Manual Timer --
   ------------------

   not overriding procedure Start (Timer : in out Manual_Timer) is
   begin
      Timer.Start_Time := Now;
      Timer.Running := True;
   end Start;


   not overriding procedure Stop (Timer : in out Manual_Timer) is
   begin
      Timer.Backend.Add (Now - Timer.Start_Time);
      Timer.Running := False;
   end Stop;


   not overriding procedure Cancel (Timer : in out Manual_Timer) is
   begin
      Timer.Running := False;
   end Cancel;



   ---------------------
   -- Automatic Timer --
   ---------------------

   not overriding procedure Cancel (Timer : in out Auto_Timer) is
   begin
      Timer.Reported := True;
   end Cancel;


   overriding procedure Initialize (Object : in out Auto_Timer) is
   begin
      Object.Start_Time := Now;
      Object.Reported := False;
   end Initialize;


   overriding procedure Finalize (Object : in out Auto_Timer) is
   begin
      if not Object.Reported then
         Object.Backend.Add (Now - Object.Start_Time);
         Object.Reported := True;
      end if;
   end Finalize;

end Natools.Time_Statistics.Generic_Timers;
