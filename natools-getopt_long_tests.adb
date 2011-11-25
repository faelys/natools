------------------------------------------------------------------------------
-- Copyright (c) 2011, Natacha Porté                                        --
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

with Ada.Containers.Indefinite_Vectors;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Natools.Getopt_Long;

package body Natools.Getopt_Long_Tests is

   package US renames Ada.Strings.Unbounded;

   ----------------------------------------
   -- Dynamic command line argument list --
   ----------------------------------------

   package String_Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Positive, Element_Type => String);

   Command_Line : String_Vectors.Vector;


   function Argument_Count return Natural;
   function Argument (Number : Positive) return String;


   function Argument_Count return Natural is
   begin
      return Natural (Command_Line.Length);
   end Argument_Count;

   function Argument (Number : Positive) return String is
   begin
      return Command_Line.Element (Number);
   end Argument;



   --------------------------------
   -- Arguments used for testing --
   --------------------------------

   type Option_Id is
     (Short_No_Arg, Short_No_Arg_2, Short_Opt_Arg, Short_Arg,
      Long_No_Arg, Long_Opt_Arg, Long_Arg, Long_Ambiguous,
      Mixed_No_Arg, Mixed_Opt_Arg, Mixed_Arg,
      Command_Argument);

   type Flag_Seen_Array is array (Option_Id) of Boolean;

   type Flag_Argument_Array is array (Option_Id) of US.Unbounded_String;

   package Getopt is new Natools.Getopt_Long (Option_Id);

   Flag_Seen : Flag_Seen_Array;
   Flag_Argument : Flag_Argument_Array;
   Flag_Error : String_Vectors.Vector;
   Separator : constant Character := ';';

   procedure Callback (Id : Option_Id; Argument : String);
      --  Process the given argument, by recording it as seen in Flag_Seen
      --    and appending the argument to Flag_Argument.
   procedure Dump (Report : in out NT.Reporter'Class);
      --  Dump the current state (Flag_* variables) into the Report.
   function Option_Definitions return Getopt.Option_Definitions;
      --  Create the Option_Definitions object used for these tests.
   procedure Reset_Flags;
      --  Reset Flag_* variables for a new test.



   procedure Callback (Id : Option_Id; Argument : String) is
   begin
      Flag_Seen (Id) := True;
      US.Append (Flag_Argument (Id), Argument & Separator);
   end Callback;


   procedure Dump (Report : in out NT.Reporter'Class) is
      procedure Process (Position : String_Vectors.Cursor);
      function Seen_String (Seen : Boolean) return String;

      procedure Process (Position : String_Vectors.Cursor) is
      begin
         Report.Info ("Error """ & String_Vectors.Element (Position) & '"');
      end Process;

      function Seen_String (Seen : Boolean) return String is
      begin
         if Seen then
            return "Seen";
         else
            return "Not seen";
         end if;
      end Seen_String;
   begin
      Report.Info ("Flags:");
      for Id in Option_Id loop
         Report.Info ("  " & Option_Id'Image (Id) & ": "
                           & Seen_String (Flag_Seen (Id)) & ", """
                           & US.To_String (Flag_Argument (Id)) & '"');
      end loop;
      Flag_Error.Iterate (Process'Access);
   end Dump;


   function Option_Definitions return Getopt.Option_Definitions is
   begin
      return OD : Getopt.Option_Definitions do
         OD.Add_Option ('a', Getopt.No_Argument, Short_No_Arg);
         OD.Add_Option ('q', Getopt.No_Argument, Short_No_Arg_2);
         OD.Add_Option ('f', Getopt.Required_Argument, Short_Arg);
         OD.Add_Option ('v', Getopt.Optional_Argument, Short_Opt_Arg);
         OD.Add_Option ("aq", Getopt.No_Argument, Long_Ambiguous);
         OD.Add_Option ("aquatic", Getopt.No_Argument, Long_No_Arg);
         OD.Add_Option ("color", Getopt.Optional_Argument, Long_Opt_Arg);
         OD.Add_Option ("input", Getopt.Required_Argument, Long_Arg);
         OD.Add_Option ("execute", 'e', Getopt.Required_Argument, Mixed_Arg);
         OD.Add_Option ("ignore-case", 'i', Getopt.No_Argument, Mixed_No_Arg);
         OD.Add_Option ("write", 'w', Getopt.Optional_Argument, Mixed_Opt_Arg);
      end return;
   end Option_Definitions;


   procedure Reset_Flags is
   begin
      for Id in Option_Id loop
         Flag_Seen (Id) := False;
         Flag_Argument (Id) := US.Null_Unbounded_String;
      end loop;
      Flag_Error.Clear;
   end Reset_Flags;



   ----------------------------
   -- Generic test procedure --
   ----------------------------

   procedure Test
     (Report : in out NT.Reporter'Class;
      Name : String;
      Expected_Seen : Flag_Seen_Array;
      Expected_Argument : Flag_Argument_Array;
      Expected_Error : String_Vectors.Vector := String_Vectors.Empty_Vector;
      Posixly_Correct : Boolean := True;
      Long_Only : Boolean := False);


   procedure Test
     (Report : in out NT.Reporter'Class;
      Name : String;
      Expected_Seen : Flag_Seen_Array;
      Expected_Argument : Flag_Argument_Array;
      Expected_Error : String_Vectors.Vector := String_Vectors.Empty_Vector;
      Posixly_Correct : Boolean := True;
      Long_Only : Boolean := False)
   is
      use type String_Vectors.Vector;
      Options : constant Getopt.Option_Definitions := Option_Definitions;
   begin
      Reset_Flags;
      begin
         Options.Process
           (Top_Level_Argument => Command_Argument,
            Callback           => Callback'Access,
            Posixly_Correct    => Posixly_Correct,
            Long_Only          => Long_Only,
            Argument_Count     => Argument_Count'Access,
            Argument           => Argument'Access);
      exception
         when Error : Getopt.Option_Error =>
            Flag_Error.Append (Ada.Exceptions.Exception_Message (Error));
      end;

      if Flag_Seen = Expected_Seen and
         Flag_Argument = Expected_Argument and
         Flag_Error = Expected_Error
      then
         Report.Item (Name, NT.Success);
      else
         Report.Item (Name, NT.Fail);
         Dump (Report);
      end if;
   exception
      when Error : others =>
         Report.Report_Exception (Name, Error);
         Dump (Report);
   end Test;



   ---------------------------
   -- Public test functions --
   ---------------------------

   procedure All_Tests (Report : in out NT.Reporter'Class) is
   begin
      Test_Arguments (Report);
      Test_Empty (Report);
      Test_Error_Callbacks (Report);
      Test_Everything (Report);
      Test_Long (Report);
      Test_Long_Only (Report);
      Test_Long_Partial (Report);
      Test_Long_Partial_Ambiguous (Report);
      Test_Missing_Argument_Long (Report);
      Test_Missing_Argument_Short (Report);
      Test_Mixed_Arg (Report);
      Test_Mixed_No_Arg (Report);
      Test_Posixly_Correct (Report);
      Test_Short_Argument (Report);
      Test_Short_Compact (Report);
      Test_Short_Expanded (Report);
      Test_Unexpected_Argument (Report);
      Test_Unknown_Long (Report);
      Test_Unknown_Short (Report);
   end All_Tests;


   procedure Test_Arguments (Report : in out NT.Reporter'Class) is
   begin
      Command_Line.Clear;
      Command_Line.Append ("Argument 1");
      Command_Line.Append ("Argument 2");
      Command_Line.Append ("Argument 3");
      Test (Report, "Arguments without flag",
            (Command_Argument => True,
             others => False),
            (Command_Argument
               => US.To_Unbounded_String ("Argument 1;Argument 2;Argument 3;"),
             others => US.Null_Unbounded_String));
   end Test_Arguments;


   procedure Test_Empty (Report : in out NT.Reporter'Class) is
   begin
      Command_Line.Clear;
      Test (Report, "Empty command line",
            (others => False),
            (others => US.Null_Unbounded_String));
   end Test_Empty;


   procedure Test_Error_Callbacks (Report : in out NT.Reporter'Class) is
      procedure Local_Test
        (Name : String;
         Expected_Seen : Flag_Seen_Array;
         Expected_Argument : Flag_Argument_Array;
         Expected_Missing_Argument : Natural := 0;
         Expected_Unexpected_Argument : Natural := 0;
         Expected_Unknown_Long_Option : Natural := 0;
         Expected_Unknown_Short_Option : Natural := 0);
      procedure Missing_Argument (Id : Option_Id);
      procedure Unexpected_Argument (Id : Option_Id; Arg : String);
      procedure Unknown_Long_Option (Name : String);
      procedure Unknown_Short_Option (Name : Character);

      Missing_Argument_Nb, Unexpected_Argument_Nb,
      Unknown_Long_Option_Nb, Unknown_Short_Option_Nb : Natural;

      procedure Local_Test
        (Name : String;
         Expected_Seen : Flag_Seen_Array;
         Expected_Argument : Flag_Argument_Array;
         Expected_Missing_Argument : Natural := 0;
         Expected_Unexpected_Argument : Natural := 0;
         Expected_Unknown_Long_Option : Natural := 0;
         Expected_Unknown_Short_Option : Natural := 0)
      is
         Options : constant Getopt.Option_Definitions := Option_Definitions;
      begin
         Reset_Flags;
         Missing_Argument_Nb := 0;
         Unexpected_Argument_Nb := 0;
         Unknown_Long_Option_Nb := 0;
         Unknown_Short_Option_Nb := 0;
         Options.Process
           (Top_Level_Argument   => Command_Argument,
            Callback             => Callback'Access,
            Missing_Argument     => Missing_Argument'Access,
            Unexpected_Argument  => Unexpected_Argument'Access,
            Unknown_Long_Option  => Unknown_Long_Option'Access,
            Unknown_Short_Option => Unknown_Short_Option'Access,
            Argument_Count       => Argument_Count'Access,
            Argument             => Argument'Access);
         if Missing_Argument_Nb /= Expected_Missing_Argument or
           Unexpected_Argument_Nb /= Expected_Unexpected_Argument or
           Unknown_Long_Option_Nb /= Expected_Unknown_Long_Option or
           Unknown_Short_Option_Nb /= Expected_Unknown_Short_Option
         then
            Report.Item (Name, NT.Fail);
            if Missing_Argument_Nb /= Expected_Missing_Argument then
               Report.Info ("Missing argument callback called"
                            & Natural'Image (Missing_Argument_Nb)
                            & " times, expected"
                            & Natural'Image (Expected_Missing_Argument));
            end if;
            if Unexpected_Argument_Nb /= Expected_Unexpected_Argument then
               Report.Info ("Unexpected argument callback called"
                            & Natural'Image (Unexpected_Argument_Nb)
                            & " times, expected"
                            & Natural'Image (Expected_Unexpected_Argument));
            end if;
            if Unknown_Long_Option_Nb /= Expected_Unknown_Long_Option then
               Report.Info ("Unknown long option callback called"
                            & Natural'Image (Unknown_Long_Option_Nb)
                            & " times, expected"
                            & Natural'Image (Expected_Unknown_Long_Option));
            end if;
            if Unknown_Short_Option_Nb /= Expected_Unknown_Short_Option then
               Report.Info ("Unknown short option callback called"
                            & Natural'Image (Unknown_Short_Option_Nb)
                            & " times, expected"
                            & Natural'Image (Expected_Unknown_Short_Option));
            end if;
         elsif Flag_Seen /= Expected_Seen or
           Flag_Argument /= Expected_Argument
         then
            Report.Item (Name, NT.Fail);
            Dump (Report);
         else
            Report.Item (Name, NT.Success);
         end if;
      exception
         when Error : others =>
            Report.Report_Exception (Name, Error);
            Dump (Report);
      end Local_Test;

      procedure Missing_Argument (Id : Option_Id) is
         pragma Unreferenced (Id);
      begin
         Missing_Argument_Nb := Missing_Argument_Nb + 1;
      end Missing_Argument;

      procedure Unexpected_Argument (Id : Option_Id; Arg : String) is
         pragma Unreferenced (Id, Arg);
      begin
         Unexpected_Argument_Nb := Unexpected_Argument_Nb + 1;
      end Unexpected_Argument;

      procedure Unknown_Long_Option (Name : String) is
         pragma Unreferenced (Name);
      begin
         Unknown_Long_Option_Nb := Unknown_Long_Option_Nb + 1;
      end Unknown_Long_Option;

      procedure Unknown_Short_Option (Name : Character) is
         pragma Unreferenced (Name);
      begin
         Unknown_Short_Option_Nb := Unknown_Short_Option_Nb + 1;
      end Unknown_Short_Option;
   begin
      Report.Section ("Error-handling callbacks");

      Command_Line.Clear;
      Command_Line.Append ("-af");
      Local_Test ("Missing argument for short option",
                  (Short_No_Arg => True, others => False),
                  (Short_No_Arg => US.To_Unbounded_String (";"),
                   others => US.Null_Unbounded_String),
                  Expected_Missing_Argument => 1);

      Command_Line.Clear;
      Command_Line.Append ("--color");
      Command_Line.Append ("--input");
      Local_Test ("Missing argument for long option",
                  (Long_Opt_Arg => True, others => False),
                  (Long_Opt_Arg => US.To_Unbounded_String (";"),
                   others => US.Null_Unbounded_String),
                  Expected_Missing_Argument => 1);

      Command_Line.Clear;
      Command_Line.Append ("--aquatic=extra");
      Local_Test ("Unexpected argument",
                  (others => False),
                  (others => US.Null_Unbounded_String),
                  Expected_Unexpected_Argument => 1);

      Command_Line.Clear;
      Command_Line.Append ("-a");
      Command_Line.Append ("--ignore-case=true");
      Command_Line.Append ("--execute");
      Command_Line.Append ("command");
      Command_Line.Append ("file");
      Local_Test ("Process continues after caught unexpected argument",
                  (Short_No_Arg | Mixed_Arg | Command_Argument => True,
                   others => False),
                  (Short_No_Arg => US.To_Unbounded_String (";"),
                   Mixed_Arg => US.To_Unbounded_String ("command;"),
                   Command_Argument => US.To_Unbounded_String ("file;"),
                   others => US.Null_Unbounded_String),
                  Expected_Unexpected_Argument => 1);

      Command_Line.Clear;
      Command_Line.Append ("-abqffoo");
      Local_Test ("Unknown short option",
                  (Short_No_Arg | Short_No_Arg_2 | Short_Arg => True,
                   others => False),
                  (Short_No_Arg => US.To_Unbounded_String (";"),
                   Short_No_Arg_2 => US.To_Unbounded_String (";"),
                   Short_Arg => US.To_Unbounded_String ("foo;"),
                   others => US.Null_Unbounded_String),
                  Expected_Unknown_Short_Option => 1);

      Command_Line.Clear;
      Command_Line.Append ("--execute");
      Command_Line.Append ("command");
      Command_Line.Append ("--unknown=argument");
      Command_Line.Append ("file");
      Local_Test ("Unknown long option",
                  (Mixed_Arg | Command_Argument => True, others => False),
                  (Mixed_Arg => US.To_Unbounded_String ("command;"),
                   Command_Argument => US.To_Unbounded_String ("file;"),
                   others => US.Null_Unbounded_String),
                  Expected_Unknown_Long_Option => 1);

      Command_Line.Clear;
      Command_Line.Append ("--ignore-case");
      Command_Line.Append ("-bffoo");
      Command_Line.Append ("--aq=unexpected");
      Command_Line.Append ("-ecommand");
      Command_Line.Append ("--unknown");
      Command_Line.Append ("--input");
      Local_Test ("All errors simultaneously",
                  (Short_Arg | Mixed_No_Arg | Mixed_Arg => True,
                   others => False),
                  (Short_Arg => US.To_Unbounded_String ("foo;"),
                   Mixed_Arg => US.To_Unbounded_String ("command;"),
                   Mixed_No_Arg => US.To_Unbounded_String (";"),
                   others => US.Null_Unbounded_String),
                  Expected_Missing_Argument => 1,
                  Expected_Unexpected_Argument => 1,
                  Expected_Unknown_Long_Option => 1,
                  Expected_Unknown_Short_Option => 1);

      Report.End_Section;
   end Test_Error_Callbacks;


   procedure Test_Everything (Report : in out NT.Reporter'Class) is
   begin
      Command_Line.Clear;
      Command_Line.Append ("--write=arg 1");
      Command_Line.Append ("-awvfarg 2");
      Command_Line.Append ("--aq");
      Command_Line.Append ("-e");
      Command_Line.Append ("arg 3");
      Command_Line.Append ("--ignore-case");
      Command_Line.Append ("--color=arg 4");
      Command_Line.Append ("-iv");
      Command_Line.Append ("--execute=arg 5");
      Command_Line.Append ("--color");
      Command_Line.Append ("--input");
      Command_Line.Append ("arg 6");
      Command_Line.Append ("arg 7");
      Command_Line.Append ("arg 8");
      Test (Report, "Everything together",
            (Short_No_Arg_2 | Long_No_Arg => False, others => True),
            (Short_No_Arg     => US.To_Unbounded_String (";"),
             Short_No_Arg_2   => US.Null_Unbounded_String,
             Short_Arg        => US.To_Unbounded_String ("arg 2;"),
             Short_Opt_Arg    => US.To_Unbounded_String (";;"),
             Long_Ambiguous   => US.To_Unbounded_String (";"),
             Long_No_Arg      => US.Null_Unbounded_String,
             Long_Opt_Arg     => US.To_Unbounded_String ("arg 4;;"),
             Long_Arg         => US.To_Unbounded_String ("arg 6;"),
             Mixed_Arg        => US.To_Unbounded_String ("arg 3;arg 5;"),
             Mixed_No_Arg     => US.To_Unbounded_String (";;"),
             Mixed_Opt_Arg    => US.To_Unbounded_String ("arg 1;;"),
             Command_Argument => US.To_Unbounded_String ("arg 7;arg 8;")));
   end Test_Everything;


   procedure Test_Long (Report : in out NT.Reporter'Class) is
   begin
      Command_Line.Clear;
      Command_Line.Append ("--aquatic");
      Command_Line.Append ("--input=i equal");
      Command_Line.Append ("--color=c equal");
      Command_Line.Append ("--input");
      Command_Line.Append ("i space");
      Command_Line.Append ("--color");
      Command_Line.Append ("c space");
      Command_Line.Append ("top level");
      Test (Report, "Long flags",
            (Long_No_Arg | Long_Opt_Arg | Long_Arg | Command_Argument => True,
             others => False),
            (Long_No_Arg => US.To_Unbounded_String (";"),
             Long_Opt_Arg => US.To_Unbounded_String ("c equal;;"),
             Long_Arg => US.To_Unbounded_String ("i equal;i space;"),
             Command_Argument => US.To_Unbounded_String ("c space;top level;"),
             others => US.Null_Unbounded_String));
   end Test_Long;


   procedure Test_Long_Only (Report : in out NT.Reporter'Class) is
   begin
      Command_Line.Clear;
      Command_Line.Append ("-aq");
         --  Can be either 'a' and 'q' short flags or "aq" long flag, depending
         --    on Long_Only parameter

      --  Without Long_Only (default)
      Test (Report, "Long_Only disabled (default)",
            (Short_No_Arg | Short_No_Arg_2 => True, others => False),
            (Short_No_Arg | Short_No_Arg_2 => US.To_Unbounded_String (";"),
             others => US.Null_Unbounded_String),
            Long_Only => False);

      --  With Long_Only
      Test (Report, "Long_Only enabled",
            (Long_Ambiguous => True, others => False),
            (Long_Ambiguous => US.To_Unbounded_String (";"),
             others => US.Null_Unbounded_String),
            Long_Only => True);
   end Test_Long_Only;


   procedure Test_Long_Partial (Report : in out NT.Reporter'Class) is
   begin
      Command_Line.Clear;
      Command_Line.Append ("--aqu");
      Command_Line.Append ("--co=foo");
      Command_Line.Append ("--in");
      Command_Line.Append ("bar");
      Test (Report, "Partial matches for long flags",
            (Long_No_Arg | Long_Opt_Arg | Long_Arg => True, others => False),
            (Long_No_Arg => US.To_Unbounded_String (";"),
             Long_Opt_Arg => US.To_Unbounded_String ("foo;"),
             Long_Arg => US.To_Unbounded_String ("bar;"),
             others => US.Null_Unbounded_String));
   end Test_Long_Partial;


   procedure Test_Long_Partial_Ambiguous (Report : in out NT.Reporter'Class) is
   begin
      Command_Line.Clear;
      Command_Line.Append ("--i");
         --  partial match for both "input" and "ignore-case" long flags
      Test (Report, "Ambiguous partial match for long flags",
            (others => False),
            (others => US.Null_Unbounded_String),
            String_Vectors.To_Vector ("Unknown long option i", 1));

      Command_Line.Clear;
      Command_Line.Append ("--aq");
         --  partial match for both "aq" and "aquatic" long flags
         --  but exact match is preferred
      Test (Report, "Ambiguous exact match for long flags",
            (Long_Ambiguous => True, others => False),
            (Long_Ambiguous => US.To_Unbounded_String (";"),
             others => US.Null_Unbounded_String));
   end Test_Long_Partial_Ambiguous;


   procedure Test_Missing_Argument_Long (Report : in out NT.Reporter'Class) is
   begin
      Command_Line.Clear;
      Command_Line.Append ("--color");
      Command_Line.Append ("--input");
      Test (Report, "Missing argument for long option",
            (Long_Opt_Arg => True, others => False),
            (Long_Opt_Arg => US.To_Unbounded_String (";"),
             others => US.Null_Unbounded_String),
            String_Vectors.To_Vector ("Missing argument to option input", 1));
   end Test_Missing_Argument_Long;


   procedure Test_Missing_Argument_Short (Report : in out NT.Reporter'Class) is
   begin
      Command_Line.Clear;
      Command_Line.Append ("-v");
      Command_Line.Append ("-f");
      Test (Report, "Missing argument for long option",
            (Short_Opt_Arg => True, others => False),
            (Short_Opt_Arg => US.To_Unbounded_String (";"),
             others => US.Null_Unbounded_String),
            String_Vectors.To_Vector ("Missing argument to option f", 1));
   end Test_Missing_Argument_Short;


   procedure Test_Mixed_Arg (Report : in out NT.Reporter'Class) is
   begin
      Command_Line.Clear;
      Command_Line.Append ("-efoo");
      Command_Line.Append ("-qe");
      Command_Line.Append ("bar");
      Command_Line.Append ("-aebaz");
      Command_Line.Append ("--execute=long");
      Test (Report, "Short and long options with arguments",
            (Mixed_Arg | Short_No_Arg | Short_No_Arg_2  => True,
             others => False),
            (Mixed_Arg => US.To_Unbounded_String ("foo;bar;baz;long;"),
             Short_No_Arg | Short_No_Arg_2 => US.To_Unbounded_String (";"),
             others => US.Null_Unbounded_String));
   end Test_Mixed_Arg;


   procedure Test_Mixed_No_Arg (Report : in out NT.Reporter'Class) is
   begin
      Command_Line.Clear;
      Command_Line.Append ("-ai");
      Command_Line.Append ("--ignore-case");
      Test (Report, "Short and long options without arguments",
            (Mixed_No_Arg | Short_No_Arg => True, others => False),
            (Mixed_No_Arg => US.To_Unbounded_String (";;"),
             Short_No_Arg => US.To_Unbounded_String (";"),
             others => US.Null_Unbounded_String));
   end Test_Mixed_No_Arg;


   procedure Test_Posixly_Correct (Report : in out NT.Reporter'Class) is
   begin
      Command_Line.Clear;
      Command_Line.Append ("-a");
      Command_Line.Append ("top level");
      Command_Line.Append ("-q");
         --  Posixly_Correct defines whether this "-q" is a top-level argument
         --    or a short flag

      --  With the flag
      Test (Report, "Posixly correct behavior",
            (Short_No_Arg | Command_Argument => True,
             others => False),
            (Short_No_Arg => US.To_Unbounded_String (";"),
             Command_Argument => US.To_Unbounded_String ("top level;-q;"),
             others => US.Null_Unbounded_String),
            Posixly_Correct => True);

      --  Without the flag
      Test (Report, "GNU (posixly incorrect) behavior",
            (Short_No_Arg | Short_No_Arg_2 | Command_Argument => True,
             others => False),
            (Short_No_Arg | Short_No_Arg_2 => US.To_Unbounded_String (";"),
             Command_Argument => US.To_Unbounded_String ("top level;"),
             others => US.Null_Unbounded_String),
            Posixly_Correct => False);
   end Test_Posixly_Correct;


   procedure Test_Short_Argument (Report : in out NT.Reporter'Class) is
   begin
      Command_Line.Clear;
      Command_Line.Append ("-faq");
         --  "aq" is argument for 'f' short flag, not 'a' and 'q' short flags
      Command_Line.Append ("-f");
      Command_Line.Append ("-a");
         --  "-a" is argument for 'f' short flag, not 'a' short flag
      Command_Line.Append ("-v");
      Command_Line.Append ("bar");
         --  "bar" is top level argument, because optional argument for short
         --    flags are never set
      Test (Report, "Arguments to short flags",
            (Short_Arg | Short_Opt_Arg | Command_Argument => True,
             others => False),
            (Short_Arg => US.To_Unbounded_String ("aq;-a;"),
             Short_Opt_Arg => US.To_Unbounded_String (";"),
             Command_Argument => US.To_Unbounded_String ("bar;"),
             others => US.Null_Unbounded_String));
   end Test_Short_Argument;


   procedure Test_Short_Compact (Report : in out NT.Reporter'Class) is
   begin
      Command_Line.Clear;
      Command_Line.Append ("-avq");
         --  "q" is not argument to 'v' short flag, but a short flag itself
      Test (Report, "Argumentless compact short flags",
            (Short_No_Arg | Short_No_Arg_2 | Short_Opt_Arg => True,
             others => False),
            (Short_No_Arg | Short_No_Arg_2 | Short_Opt_Arg =>
               US.To_Unbounded_String (";"),
             others => US.Null_Unbounded_String));
   end Test_Short_Compact;


   procedure Test_Short_Expanded (Report : in out NT.Reporter'Class) is
   begin
      Command_Line.Clear;
      Command_Line.Append ("-a");
      Command_Line.Append ("-v");
      Command_Line.Append ("-q");
      Test (Report, "Argumentless expanded short flags",
            (Short_No_Arg | Short_No_Arg_2 | Short_Opt_Arg => True,
             others => False),
            (Short_No_Arg | Short_No_Arg_2 | Short_Opt_Arg =>
               US.To_Unbounded_String (";"),
             others => US.Null_Unbounded_String));
   end Test_Short_Expanded;


   procedure Test_Unexpected_Argument (Report : in out NT.Reporter'Class) is
   begin
      Command_Line.Clear;
      Command_Line.Append ("--color=foo");
      Command_Line.Append ("--aq=bar");
      Test (Report, "Unexpected argument to long option",
            (Long_Opt_Arg => True, others => False),
            (Long_Opt_Arg => US.To_Unbounded_String ("foo;"),
             others => US.Null_Unbounded_String),
            String_Vectors.To_Vector ("Unexpected argument ""bar"" to aq", 1));
   end Test_Unexpected_Argument;


   procedure Test_Unknown_Long (Report : in out NT.Reporter'Class) is
   begin
      Command_Line.Clear;
      Command_Line.Append ("--long-flag");
      Test (Report, "Unknown long flag",
            (others => False), (others => US.Null_Unbounded_String),
            String_Vectors.To_Vector ("Unknown long option long-flag", 1));
   end Test_Unknown_Long;


   procedure Test_Unknown_Short (Report : in out NT.Reporter'Class) is
   begin
      Command_Line.Clear;
      Command_Line.Append ("-g");
      Test (Report, "Unknown short flag",
            (others => False), (others => US.Null_Unbounded_String),
            String_Vectors.To_Vector ("Unknown short option g", 1));
   end Test_Unknown_Short;

end Natools.Getopt_Long_Tests;
