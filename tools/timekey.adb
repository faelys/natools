------------------------------------------------------------------------------
-- Copyright (c) 2015-2017, Natacha Porté                                   --
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

with Ada.Calendar;
with Ada.Command_Line;
with Ada.Text_IO;
with Natools.Time_IO.RFC_3339;
with Natools.Time_Keys;

procedure Timekey is
   procedure Process (Line : in String);
      --  Guess the type of Line and convert it to or from type.

   procedure Process_Input;
      --  Read lines from current input and process them.


   Input_Processed : Boolean := False;
   Empty : Boolean := True;
   Verbose : Boolean := False;
   Subsecond_Digits : Natural := Duration'Aft;


   procedure Process (Line : in String) is
   begin
      if Verbose then
         Ada.Text_IO.Put (Line);
      end if;

      if Natools.Time_Keys.Is_Valid (Line) then
         if Verbose then
            Ada.Text_IO.Put (" => ");
         end if;

         Ada.Text_IO.Put_Line
           (Natools.Time_IO.RFC_3339.Image
              (Natools.Time_Keys.To_Time (Line), Subsecond_Digits, False));

      elsif Natools.Time_IO.RFC_3339.Is_Valid (Line) then
         if Verbose then
            Ada.Text_IO.Put (" => ");
         end if;

         Ada.Text_IO.Put_Line
           (Natools.Time_Keys.To_Key (Natools.Time_IO.RFC_3339.Value (Line)));
      end if;
   end Process;


   procedure Process_Input is
   begin
      if Input_Processed then
         return;
      else
         Input_Processed := True;
      end if;

      begin
         loop
            Process (Ada.Text_IO.Get_Line);
         end loop;
      exception
         when Ada.Text_IO.End_Error => null;
      end;
   end Process_Input;
begin
   for I in 1 .. Ada.Command_Line.Argument_Count loop
      declare
         Arg : constant String := Ada.Command_Line.Argument (I);
      begin
         if Arg = "-" then
            Empty := False;
            Process_Input;
         elsif Arg = "-v" then
            Verbose := True;
         elsif Arg'Length = 2
           and then Arg (Arg'First) = '-'
           and then Arg (Arg'Last) in '0' .. '9'
         then
            Subsecond_Digits := Character'Pos (Arg (Arg'Last))
              - Character'Pos ('0');
         else
            Empty := False;
            Process (Arg);
         end if;
      end;
   end loop;

   if Empty then
      declare
         Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      begin
         if Verbose then
            Ada.Text_IO.Put
              (Natools.Time_IO.RFC_3339.Image (Now, Subsecond_Digits, False)
               & " => ");
         end if;

         Ada.Text_IO.Put_Line (Natools.Time_Keys.To_Key (Now));
      end;
   end if;
end Timekey;
