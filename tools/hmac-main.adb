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

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Streams;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Text_IO.Text_Streams;

with Natools.Getopt_Long;
with Natools.S_Expressions;
with Natools.S_Expressions.Encodings;
with Natools.S_Expressions.File_Readers;

with HMAC.Pinentry;

procedure HMAC.Main is

   procedure Base64_Output (Digest : in Ada.Streams.Stream_Element_Array);
      --  Output the given binary Digest in base-64

   procedure Lower_Hex_Output (Digest : in Ada.Streams.Stream_Element_Array);
      --  Output the given binary Digest in lower-case hexadecimal

   procedure Raw_Output (Digest : in Ada.Streams.Stream_Element_Array);
      --  Output the given binary Direct directly

   procedure Upper_Hex_Output (Digest : in Ada.Streams.Stream_Element_Array);
      --  Output the given binary Digest in upper-case hexadecimal


   package Options is
      type Id is
        (Base64_Output,
         Key_File,
         Lower_Hex_Output,
         Pinentry,
         Raw_Output,
         Upper_Hex_Output);
   end Options;

   package Getopt is new Natools.Getopt_Long (Options.Id);

   type Encode_Output is not null access procedure
     (Digest : in Ada.Streams.Stream_Element_Array);

   type Callback is new Getopt.Handlers.Callback with record
      Output : Encode_Output := Lower_Hex_Output'Access;
      Key : Ada.Strings.Unbounded.Unbounded_String;
      Has_Key : Boolean := False;
      Done : Boolean := False;
   end record;

   overriding procedure Option
     (Handler : in out Callback;
      Id : in Options.Id;
      Argument : in String);

   overriding procedure Argument
     (Handler : in out Callback;
      Argument : in String);


   overriding procedure Option
     (Handler : in out Callback;
      Id : in Options.Id;
      Argument : in String) is
   begin
      case Id is
         when Options.Base64_Output =>
            Handler.Output := Base64_Output'Access;
         when Options.Key_File =>
            if Argument = "-" then
               Handler.Key := Ada.Strings.Unbounded.To_Unbounded_String
                 (Ada.Text_IO.Get_Line);
            else
               Handler.Key := Ada.Strings.Unbounded.To_Unbounded_String
                 (Natools.S_Expressions.To_String
                    (Natools.S_Expressions.File_Readers.Reader
                       (Argument).Read));
            end if;
            Handler.Has_Key := True;
         when Options.Lower_Hex_Output =>
            Handler.Output := Lower_Hex_Output'Access;
         when Options.Pinentry =>
            begin
               Handler.Key := Ada.Strings.Unbounded.To_Unbounded_String
                 (Pinentry.Get_Key (Argument));
               Handler.Has_Key := True;
            exception
               when Ex : others =>
                  Ada.Text_IO.Put_Line (Ada.Text_IO.Current_Error,
                     "Unable to get PIN from """ & Argument & '"');
                  Ada.Text_IO.Put_Line (Ada.Text_IO.Current_Error,
                     "exception " & Ada.Exceptions.Exception_Name (Ex)
                     & ": " & Ada.Exceptions.Exception_Message (Ex));
            end;
         when Options.Raw_Output =>
            Handler.Output := Raw_Output'Access;
         when Options.Upper_Hex_Output =>
            Handler.Output := Upper_Hex_Output'Access;
      end case;
   end Option;


   overriding procedure Argument
     (Handler : in out Callback;
      Argument : in String) is
   begin
      if Handler.Has_Key then
         Handler.Output (HMAC_Implementation.Digest
           (Ada.Strings.Unbounded.To_String (Handler.Key),
            Natools.S_Expressions.To_Atom (Argument)));
         Handler.Done := True;
      else
         Handler.Key := Ada.Strings.Unbounded.To_Unbounded_String (Argument);
         Handler.Has_Key := True;
      end if;
   end Argument;


   procedure Base64_Output (Digest : in Ada.Streams.Stream_Element_Array) is
   begin
      Ada.Text_IO.Put_Line (Natools.S_Expressions.To_String
        (Natools.S_Expressions.Encodings.Encode_Base64 (Digest)));
   end Base64_Output;

   procedure Lower_Hex_Output (Digest : in Ada.Streams.Stream_Element_Array) is
   begin
      Ada.Text_IO.Put_Line (Natools.S_Expressions.To_String
        (Natools.S_Expressions.Encodings.Encode_Hex
          (Digest, Natools.S_Expressions.Encodings.Lower)));
   end Lower_Hex_Output;

   procedure Raw_Output (Digest : in Ada.Streams.Stream_Element_Array) is
   begin
      Ada.Text_IO.Text_Streams.Stream (Ada.Text_IO.Current_Output).Write
        (Digest);
   end Raw_Output;

   procedure Upper_Hex_Output (Digest : in Ada.Streams.Stream_Element_Array) is
   begin
      Ada.Text_IO.Put_Line (Natools.S_Expressions.To_String
        (Natools.S_Expressions.Encodings.Encode_Hex
          (Digest, Natools.S_Expressions.Encodings.Upper)));
   end Upper_Hex_Output;

   Opt_Config : Getopt.Configuration;
   Handler : Callback;
begin
   Opt_Config.Add_Option
     ("base64", 'b', Getopt.No_Argument, Options.Base64_Output);
   Opt_Config.Add_Option
     ("key-file", 'f', Getopt.Required_Argument, Options.Key_File);
   Opt_Config.Add_Option
     ("lower-hex", 'h', Getopt.No_Argument, Options.Lower_Hex_Output);
   Opt_Config.Add_Option
     ("raw", 'r', Getopt.No_Argument, Options.Raw_Output);
   Opt_Config.Add_Option
     ("upper-hex", 'H', Getopt.No_Argument, Options.Upper_Hex_Output);

   if Pinentry.Is_Available then
      Opt_Config.Add_Option
        ("pinentry", 'p', Getopt.Required_Argument, Options.Pinentry);
   end if;

   Opt_Config.Process (Handler);

   if not Handler.Has_Key then
      Ada.Text_IO.Put_Line ("Usage:");
      Ada.Text_IO.Put_Line ("   "
        & Ada.Command_Line.Command_Name
        & " [-h | -H | -b | -r] key [message]");
      Ada.Text_IO.Put_Line ("   "
        & Ada.Command_Line.Command_Name
        & " [-h | -H | -b | -r] -f path/to/key/file [message]");

      if Pinentry.Is_Available then
         Ada.Text_IO.Put_Line ("   "
           & Ada.Command_Line.Command_Name
           & " [-h | -H | -b | -r] -p path/to/bin/pinentry [message]");
      end if;

   elsif not Handler.Done then
      declare
         Context : HMAC_Implementation.Context
           := HMAC_Implementation.Create
              (Ada.Strings.Unbounded.To_String (Handler.Key));
         Block : Ada.Streams.Stream_Element_Array (1 .. 64);
         Last : Ada.Streams.Stream_Element_Offset;
         Input : constant Ada.Text_IO.Text_Streams.Stream_Access
           := Ada.Text_IO.Text_Streams.Stream (Ada.Text_IO.Current_Input);
      begin
         loop
            Input.Read (Block, Last);
            exit when Last not in Block'Range;
            HMAC_Implementation.Update
              (Context, Block (Block'First .. Last));
         end loop;

         Handler.Output (HMAC_Implementation.Digest (Context));
      end;
   end if;
end HMAC.Main;
