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

with Ada.Environment_Variables;
with Ada.Streams;

with POSIX.IO;
with POSIX.Process_Identification;
with POSIX.Process_Primitives;

with Natools.S_Expressions;

package body HMAC.Pinentry is

   procedure Check_OK (Fd : POSIX.IO.File_Descriptor);
      --  Check that pinentry OK response is coming through Fd

   procedure Check_OK
     (Receive, Send : in POSIX.IO.File_Descriptor;
      Command : in String);
      --  Send Command and wait for OK response

   function Read_Line (Fd : POSIX.IO.File_Descriptor) return String;
      --  Read a single line from Fd, aussming it will be smaller than 1 kb

   procedure Send_Command
     (Send : in POSIX.IO.File_Descriptor;
      Command : in String);
      --  Send Command through the given file descriptor



   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

   procedure Check_OK (Fd : POSIX.IO.File_Descriptor) is
      Line : constant String := Read_Line (Fd);
   begin
      if Line'Length < 2
        or else Line (Line'First .. Line'First + 1) /= "OK"
      then
         raise Backend_Error with "Not-OK response """ & Line & '"';
      end if;
   end Check_OK;


   procedure Check_OK
     (Receive, Send : in POSIX.IO.File_Descriptor;
      Command : in String) is
   begin
      Send_Command (Send, Command);
      Check_OK (Receive);
   end Check_OK;


   function Read_Line (Fd : POSIX.IO.File_Descriptor) return String is
      Buffer : Ada.Streams.Stream_Element_Array (1 .. 1024);
      Next : Ada.Streams.Stream_Element_Offset := Buffer'First;
      Result : Ada.Streams.Stream_Element_Offset;

      use type Ada.Streams.Stream_Element;
      use type Ada.Streams.Stream_Element_Offset;
   begin
      loop
         POSIX.IO.Read (Fd, Buffer (Next .. Next), Result);
         exit when Result /= Next or else Buffer (Next) = 10;
         Next := Next + 1;
      end loop;

      return Natools.S_Expressions.To_String
        (Buffer (Buffer'First .. Next - 1));
   end Read_Line;


   procedure Send_Command
     (Send : in POSIX.IO.File_Descriptor;
      Command : in String)
   is
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      if Command'Length > 0 then
         POSIX.IO.Write
           (Send,
            Natools.S_Expressions.To_Atom (Command & Character'Val (10)),
            Last);
      end if;
   end Send_Command;



   ----------------------
   -- Public Interface --
   ----------------------

   function Get_Key (Command : String) return String is
      Local_Send, Local_Receive : POSIX.IO.File_Descriptor;
      Remote_Send, Remote_Receive : POSIX.IO.File_Descriptor;
      Template : POSIX.Process_Primitives.Process_Template;
      Pid : POSIX.Process_Identification.Process_ID;
      Args : POSIX.POSIX_String_List;
   begin
      POSIX.Append (Args, POSIX.To_POSIX_String (Command));
      if Ada.Environment_Variables.Exists ("DISPLAY") then
         POSIX.Append (Args, POSIX.To_POSIX_String (String'("--display")));
         POSIX.Append (Args, POSIX.To_POSIX_String
           (Ada.Environment_Variables.Value ("DISPLAY")));
      end if;

      POSIX.IO.Create_Pipe (Local_Receive, Remote_Send);
      POSIX.IO.Create_Pipe (Remote_Receive, Local_Send);

      POSIX.Process_Primitives.Open_Template (Template);
      POSIX.Process_Primitives.Set_File_Action_To_Duplicate
        (Template => Template,
         File => POSIX.IO.Standard_Input,
         From_File => Remote_Receive);
      POSIX.Process_Primitives.Set_File_Action_To_Duplicate
        (Template => Template,
         File => POSIX.IO.Standard_Output,
         From_File => Remote_Send);
      POSIX.Process_Primitives.Set_File_Action_To_Close
        (Template, Local_Send);
      POSIX.Process_Primitives.Set_File_Action_To_Close
        (Template, Local_Receive);


      POSIX.Process_Primitives.Start_Process_Search
        (Pid,
         POSIX.Value (Args, 1),
         Template,
         Args);
      POSIX.Process_Primitives.Close_Template (Template);
      POSIX.Make_Empty (Args);

      Check_OK (Local_Receive);

      if POSIX.IO.Is_A_Terminal (POSIX.IO.Standard_Input) then
         Check_OK (Local_Receive, Local_Send, "OPTION ttyname="
            & POSIX.To_String
              (POSIX.IO.Get_Terminal_Name (POSIX.IO.Standard_Input)));
      end if;

      if Ada.Environment_Variables.Exists ("TERM") then
         Check_OK (Local_Receive, Local_Send,
            "OPTION ttytype=" & Ada.Environment_Variables.Value ("TERM"));
      end if;

      if Ada.Environment_Variables.Exists ("LC_CTYPE") then
         Check_OK (Local_Receive, Local_Send,
            "OPTION lc-ctype=" & Ada.Environment_Variables.Value ("LC_CTYPE"));
      end if;

      Send_Command (Local_Send, "GETPIN");
      declare
         Response : constant String := Read_Line (Local_Receive);
      begin

         if Response'Length < 2
           or else Response (Response'First .. Response'First + 1) /= "D "
         then
            raise Backend_Error with "Unexpected response to GETPIN: """
              & Response & '"';
         end if;

         Check_OK (Local_Receive);

         POSIX.IO.Close (Local_Send);
         POSIX.IO.Close (Local_Receive);
         POSIX.IO.Close (Remote_Send);
         POSIX.IO.Close (Remote_Receive);

         return Response (Response'First + 2 .. Response'Last);
      end;
   exception
      when others =>
         POSIX.Process_Primitives.Close_Template (Template);
         POSIX.Make_Empty (Args);
         POSIX.IO.Close (Local_Send);
         POSIX.IO.Close (Local_Receive);
         POSIX.IO.Close (Remote_Send);
         POSIX.IO.Close (Remote_Receive);
         raise;
   end Get_Key;


   function Is_Available return Boolean is
   begin
      return True;
   end Is_Available;

end HMAC.Pinentry;
