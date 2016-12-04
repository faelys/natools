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
-- Command Line Interface for primitives in Natools.Smaz.Tools.             --
------------------------------------------------------------------------------

with Ada.Characters.Latin_1;
with Ada.Command_Line;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Holders;
with Ada.Streams;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO.Text_Streams;
with Natools.Getopt_Long;
with Natools.Parallelism;
with Natools.S_Expressions.Parsers;
with Natools.S_Expressions.Printers;
with Natools.Smaz_256;
with Natools.Smaz_Generic.Tools;
with Natools.Smaz_Tools;
with Natools.Smaz_Tools.GNAT;
with Natools.String_Escapes;

procedure Smaz is
   function To_SEA (S : String) return Ada.Streams.Stream_Element_Array
     renames Natools.S_Expressions.To_Atom;

   package Holders is new Ada.Containers.Indefinite_Holders
     (Natools.Smaz_256.Dictionary, Natools.Smaz_256."=");

   package Tools_256 is new Natools.Smaz_256.Tools;

   package Methods renames Natools.Smaz_Tools.Methods;

   package Actions is
      type Enum is
        (Nothing,
         Decode,
         Encode,
         Evaluate);
   end Actions;

   package Dict_Sources is
      type Enum is
        (S_Expression,
         Text_List,
         Unoptimized_Text_List);
   end Dict_Sources;

   package Options is
      type Id is
        (Output_Ada_Dict,
         Dictionary_Input,
         Decode,
         Encode,
         Evaluate,
         Filter_Threshold,
         Output_Hash,
         Job_Count,
         Help,
         Sx_Dict_Output,
         Min_Sub_Size,
         Max_Sub_Size,
         Dict_Size,
         Max_Pending,
         Stat_Output,
         No_Stat_Output,
         Text_List_Input,
         Fast_Text_Input,
         Max_Word_Size,
         Sx_Output,
         No_Sx_Output,
         No_Vlen_Verbatim,
         Score_Method,
         Vlen_Verbatim);
   end Options;

   package Getopt is new Natools.Getopt_Long (Options.Id);

   type Callback is new Getopt.Handlers.Callback with record
      Display_Help : Boolean := False;
      Need_Dictionary : Boolean := False;
      Stat_Output : Boolean := False;
      Sx_Output : Boolean := False;
      Sx_Dict_Output : Boolean := False;
      Min_Sub_Size : Positive := 1;
      Max_Sub_Size : Positive := 3;
      Max_Word_Size : Positive := 10;
      Dict_Size : Positive := 254;
      Vlen_Verbatim : Boolean := True;
      Max_Pending : Ada.Containers.Count_Type
        := Ada.Containers.Count_Type'Last;
      Job_Count : Natural := 0;
      Filter_Threshold : Natools.Smaz_Tools.String_Count := 0;
      Score_Method : Methods.Enum := Methods.Encoded;
      Action : Actions.Enum := Actions.Nothing;
      Ada_Dictionary : Ada.Strings.Unbounded.Unbounded_String;
      Hash_Package : Ada.Strings.Unbounded.Unbounded_String;
      Dict_Source : Dict_Sources.Enum := Dict_Sources.S_Expression;
   end record;

   overriding procedure Option
     (Handler  : in out Callback;
      Id       : in Options.Id;
      Argument : in String);

   overriding procedure Argument
     (Handler  : in out Callback;
      Argument : in String)
     is null;


   function Getopt_Config return Getopt.Configuration;
      --  Build the configuration object

   function Last_Code (Dict : in Natools.Smaz_256.Dictionary)
     return Ada.Streams.Stream_Element
     is (Dict.Last_Code);
      --  Return the last valid entry

   procedure Print_Dictionary
     (Filename : in String;
      Dictionary : in Natools.Smaz_256.Dictionary;
      Hash_Package_Name : in String := "");
   procedure Print_Dictionary
     (Output : in Ada.Text_IO.File_Type;
      Dictionary : in Natools.Smaz_256.Dictionary;
      Hash_Package_Name : in String := "");
      --  print the given dictionary in the given file

   procedure Print_Help
     (Opt : in Getopt.Configuration;
      Output : in Ada.Text_IO.File_Type);
      --  Print the help text to the given file

   procedure Process
     (Handler : in Callback'Class;
      Word_List : in Natools.Smaz_Tools.String_Lists.List;
      Data_List : in Natools.Smaz_Tools.String_Lists.List);
      --  Perform the requested operations

   function To_Dictionary
     (Handler : in Callback'Class;
      Input : in Natools.Smaz_Tools.String_Lists.List)
     return Natools.Smaz_256.Dictionary;
      --  Convert the input into a dictionary given the option in Handler

   procedure Use_Dictionary (Dict : in out Natools.Smaz_256.Dictionary);
      --  Update Dictionary.Hash so that it can be actually used



   generic
      type Dictionary (<>) is private;
      type Dictionary_Entry is (<>);
      type Methods is (<>);
      type String_Count is range <>;

      type Dictionary_Counts is array (Dictionary_Entry) of String_Count;

      with package Holders
        is new Ada.Containers.Indefinite_Holders (Dictionary);
      with package String_Lists
        is new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

      with function Append_String
        (Dict : in Dictionary;
         Element : in String)
        return Dictionary;

      with function Dict_Entry
        (Dict : in Dictionary;
         Element : in Dictionary_Entry)
        return String;

      with procedure Evaluate_Dictionary
        (Dict : in Dictionary;
         Corpus : in String_Lists.List;
         Compressed_Size : out Ada.Streams.Stream_Element_Count;
         Counts : out Dictionary_Counts);

      with procedure Evaluate_Dictionary_Partial
        (Dict : in Dictionary;
         Corpus_Entry : in String;
         Compressed_Size : in out Ada.Streams.Stream_Element_Count;
         Counts : in out Dictionary_Counts);

      with function Last_Code (Dict : in Dictionary) return Dictionary_Entry;

      with function Remove_Element
        (Dict : in Dictionary;
         Element : in Dictionary_Entry)
        return Dictionary;

      with procedure Use_Dictionary (Dict : in out Dictionary) is <>;

      with function Worst_Element
        (Dict : in Dictionary;
         Counts : in Dictionary_Counts;
         Method : in Methods)
        return Dictionary_Entry;

   package Dictionary_Subprograms is

      procedure Evaluate_Dictionary
        (Job_Count : in Natural;
         Dict : in Dictionary;
         Corpus : in String_Lists.List;
         Compressed_Size : out Ada.Streams.Stream_Element_Count;
         Counts : out Dictionary_Counts);
         --  Dispatch to parallel or non-parallel version of
         --  Evaluate_Dictionary depending on Job_Count.

         procedure Optimization_Round
           (Dict : in out Holders.Holder;
            Score : in out Ada.Streams.Stream_Element_Count;
            Counts : in out Dictionary_Counts;
            Pending_Words : in out String_Lists.List;
            Input_Texts : in String_Lists.List;
            Job_Count : in Natural;
            Method : in Methods;
            Updated : out Boolean);
         --  Try to improve on Dict by replacing a single entry from it with
         --  one of the substring in Pending_Words.

         function Optimize_Dictionary
           (Base : in Dictionary;
            Pending_Words : in String_Lists.List;
            Input_Texts : in String_Lists.List;
            Job_Count : in Natural;
            Method : in Methods)
           return Dictionary;
         --  Optimize the dictionary on Input_Texts, starting with Base and
         --  adding substrings from Pending_Words.

      procedure Parallel_Evaluate_Dictionary
        (Job_Count : in Positive;
         Dict : in Dictionary;
         Corpus : in String_Lists.List;
         Compressed_Size : out Ada.Streams.Stream_Element_Count;
         Counts : out Dictionary_Counts);
         --  Return the same results as Natools.Smaz.Tools.Evaluate_Dictionary,
         --  but hopefully more quickly, using Job_Count tasks.

   end Dictionary_Subprograms;



   package body Dictionary_Subprograms is

      procedure Evaluate_Dictionary
        (Job_Count : in Natural;
         Dict : in Dictionary;
         Corpus : in String_Lists.List;
         Compressed_Size : out Ada.Streams.Stream_Element_Count;
         Counts : out Dictionary_Counts)
      is
         Actual_Dict : Dictionary := Dict;
      begin
         Use_Dictionary (Actual_Dict);

         if Job_Count > 0 then
            Parallel_Evaluate_Dictionary (Job_Count,
               Actual_Dict, Corpus, Compressed_Size, Counts);
         else
            Evaluate_Dictionary
              (Actual_Dict, Corpus, Compressed_Size, Counts);
         end if;
      end Evaluate_Dictionary;


      procedure Optimization_Round
        (Dict : in out Holders.Holder;
         Score : in out Ada.Streams.Stream_Element_Count;
         Counts : in out Dictionary_Counts;
         Pending_Words : in out String_Lists.List;
         Input_Texts : in String_Lists.List;
         Job_Count : in Natural;
         Method : in Methods;
         Updated : out Boolean)
      is
         use type Ada.Streams.Stream_Element_Offset;

         New_Value : Ada.Strings.Unbounded.Unbounded_String;
         New_Position : String_Lists.Cursor;
         Worst_Index : constant Dictionary_Entry
           := Worst_Element (Dict.Element, Counts, Method);
         Worst_Value : constant String
           := Dict_Entry (Dict.Element, Worst_Index);
         Worst_Count : constant String_Count := Counts (Worst_Index);
         Base : constant Dictionary
           := Remove_Element (Dict.Element, Worst_Index);
         Old_Score : constant Ada.Streams.Stream_Element_Count := Score;
      begin
         Updated := False;

         for Position in Pending_Words.Iterate loop
            declare
               Word : constant String := String_Lists.Element (Position);
               New_Dict : constant Dictionary := Append_String (Base, Word);
               New_Score : Ada.Streams.Stream_Element_Count;
               New_Counts : Dictionary_Counts;
            begin
               Evaluate_Dictionary
                 (Job_Count, New_Dict, Input_Texts, New_Score, New_Counts);

               if New_Score < Score then
                  Dict := Holders.To_Holder (New_Dict);
                  Score := New_Score;
                  Counts := New_Counts;
                  New_Value
                    := Ada.Strings.Unbounded.To_Unbounded_String (Word);
                  New_Position := Position;
                  Updated := True;
               end if;
            end;
         end loop;

         if Updated then
            Pending_Words.Delete (New_Position);
            Pending_Words.Append (Worst_Value);

            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Current_Error,
               "Removing"
               & Worst_Count'Img & "x "
               & Natools.String_Escapes.C_Escape_Hex (Worst_Value, True)
               & ", adding"
               & Counts (Last_Code (Dict.Element))'Img & "x "
               & Natools.String_Escapes.C_Escape_Hex
                  (Ada.Strings.Unbounded.To_String (New_Value), True)
               & ", size"
               & Score'Img
               & " ("
               & Ada.Streams.Stream_Element_Offset'Image (Score - Old_Score)
               & ')');
         end if;
      end Optimization_Round;


      function Optimize_Dictionary
        (Base : in Dictionary;
         Pending_Words : in String_Lists.List;
         Input_Texts : in String_Lists.List;
         Job_Count : in Natural;
         Method : in Methods)
        return Dictionary
      is
         Holder : Holders.Holder := Holders.To_Holder (Base);
         Pending : String_Lists.List := Pending_Words;
         Score : Ada.Streams.Stream_Element_Count;
         Counts : Dictionary_Counts;
         Running : Boolean := True;
      begin
         Evaluate_Dictionary
           (Job_Count, Base, Input_Texts, Score, Counts);

         while Running loop
            Optimization_Round
              (Holder,
               Score,
               Counts,
               Pending,
               Input_Texts,
               Job_Count,
               Method,
               Running);
         end loop;

         return Holder.Element;
      end Optimize_Dictionary;


      procedure Parallel_Evaluate_Dictionary
        (Job_Count : in Positive;
         Dict : in Dictionary;
         Corpus : in String_Lists.List;
         Compressed_Size : out Ada.Streams.Stream_Element_Count;
         Counts : out Dictionary_Counts)
      is
         type Result_Values is record
            Compressed_Size : Ada.Streams.Stream_Element_Count;
            Counts : Dictionary_Counts;
         end record;

         procedure Initialize (Result : in out Result_Values);

         procedure Get_Next_Job
           (Global : in out String_Lists.Cursor;
            Job : out String_Lists.Cursor;
            Terminated : out Boolean);

         procedure Do_Job
           (Result : in out Result_Values;
            Job : in String_Lists.Cursor);

         procedure Gather_Result
           (Global : in out String_Lists.Cursor;
            Partial : in Result_Values);


         procedure Initialize (Result : in out Result_Values) is
         begin
            Result := (Compressed_Size => 0,
                       Counts => (others => 0));
         end Initialize;


         procedure Get_Next_Job
           (Global : in out String_Lists.Cursor;
            Job : out String_Lists.Cursor;
            Terminated : out Boolean) is
         begin
            Job := Global;
            Terminated := not String_Lists.Has_Element (Global);
            if not Terminated then
               String_Lists.Next (Global);
            end if;
         end Get_Next_Job;


         procedure Do_Job
           (Result : in out Result_Values;
            Job : in String_Lists.Cursor) is
         begin
            Evaluate_Dictionary_Partial
              (Dict,
               String_Lists.Element (Job),
               Result.Compressed_Size,
               Result.Counts);
         end Do_Job;


         procedure Gather_Result
           (Global : in out String_Lists.Cursor;
            Partial : in Result_Values)
         is
            pragma Unreferenced (Global);
            use type Ada.Streams.Stream_Element_Count;
            use type Natools.Smaz_Tools.String_Count;
         begin
            Compressed_Size := Compressed_Size + Partial.Compressed_Size;

            for I in Counts'Range loop
               Counts (I) := Counts (I) + Partial.Counts (I);
            end loop;
         end Gather_Result;


         procedure Parallel_Run
           is new Natools.Parallelism.Per_Task_Accumulator_Run
              (String_Lists.Cursor, Result_Values, String_Lists.Cursor);

         Cursor : String_Lists.Cursor := String_Lists.First (Corpus);
      begin
         Compressed_Size := 0;
         Counts := (others => 0);
         Parallel_Run (Cursor, Job_Count);
      end Parallel_Evaluate_Dictionary;

   end Dictionary_Subprograms;



   package Dict_256 is new Dictionary_Subprograms
     (Dictionary => Natools.Smaz_256.Dictionary,
      Dictionary_Entry => Ada.Streams.Stream_Element,
      Methods => Natools.Smaz_Tools.Methods.Enum,
      String_Count => Natools.Smaz_Tools.String_Count,
      Dictionary_Counts => Tools_256.Dictionary_Counts,
      Holders => Holders,
      String_Lists => Natools.Smaz_Tools.String_Lists,
      Append_String => Tools_256.Append_String,
      Dict_Entry => Natools.Smaz_256.Dict_Entry,
      Evaluate_Dictionary => Tools_256.Evaluate_Dictionary,
      Evaluate_Dictionary_Partial => Tools_256.Evaluate_Dictionary_Partial,
      Last_Code => Last_Code,
      Remove_Element => Tools_256.Remove_Element,
      Worst_Element => Tools_256.Worst_Index);



   overriding procedure Option
     (Handler  : in out Callback;
      Id       : in Options.Id;
      Argument : in String) is
   begin
      case Id is
         when Options.Help =>
            Handler.Display_Help := True;

         when Options.Decode =>
            Handler.Need_Dictionary := True;
            Handler.Action := Actions.Decode;

         when Options.Encode =>
            Handler.Need_Dictionary := True;
            Handler.Action := Actions.Encode;

         when Options.Evaluate =>
            Handler.Need_Dictionary := True;
            Handler.Action := Actions.Evaluate;

         when Options.No_Stat_Output =>
            Handler.Stat_Output := False;

         when Options.No_Sx_Output =>
            Handler.Sx_Output := False;

         when Options.Output_Ada_Dict =>
            Handler.Need_Dictionary := True;

            if Argument'Length > 0 then
               Handler.Ada_Dictionary
                 := Ada.Strings.Unbounded.To_Unbounded_String (Argument);
            else
               Handler.Ada_Dictionary
                 := Ada.Strings.Unbounded.To_Unbounded_String ("-");
            end if;

         when Options.Output_Hash =>
            Handler.Need_Dictionary := True;
            Handler.Hash_Package
              := Ada.Strings.Unbounded.To_Unbounded_String (Argument);

         when Options.Stat_Output =>
            Handler.Stat_Output := True;

         when Options.Sx_Output =>
            Handler.Sx_Output := True;

         when Options.Dictionary_Input =>
            Handler.Dict_Source := Dict_Sources.S_Expression;

         when Options.Text_List_Input =>
            Handler.Dict_Source := Dict_Sources.Text_List;

         when Options.Fast_Text_Input =>
            Handler.Dict_Source := Dict_Sources.Unoptimized_Text_List;

         when Options.Sx_Dict_Output =>
            Handler.Need_Dictionary := True;
            Handler.Sx_Dict_Output := True;

         when Options.Min_Sub_Size =>
            Handler.Min_Sub_Size := Positive'Value (Argument);

         when Options.Max_Sub_Size =>
            Handler.Max_Sub_Size := Positive'Value (Argument);

         when Options.Max_Word_Size =>
            Handler.Max_Word_Size := Positive'Value (Argument);

         when Options.Job_Count =>
            Handler.Job_Count := Natural'Value (Argument);

         when Options.Filter_Threshold =>
            Handler.Filter_Threshold
              := Natools.Smaz_Tools.String_Count'Value (Argument);

         when Options.Score_Method =>
            Handler.Score_Method := Methods.Enum'Value (Argument);

         when Options.Max_Pending =>
            Handler.Max_Pending := Ada.Containers.Count_Type'Value (Argument);

         when Options.Dict_Size =>
            Handler.Dict_Size := Positive'Value (Argument);

         when Options.Vlen_Verbatim =>
            Handler.Vlen_Verbatim := True;

         when Options.No_Vlen_Verbatim =>
            Handler.Vlen_Verbatim := False;
      end case;
   end Option;


   function Getopt_Config return Getopt.Configuration is
      use Getopt;
      use Options;
      R : Getopt.Configuration;
   begin
      R.Add_Option ("ada-dict",      'A', Optional_Argument, Output_Ada_Dict);
      R.Add_Option ("decode",        'd', No_Argument,       Decode);
      R.Add_Option ("dict",          'D', No_Argument,       Dictionary_Input);
      R.Add_Option ("encode",        'e', No_Argument,       Encode);
      R.Add_Option ("evaluate",      'E', No_Argument,       Evaluate);
      R.Add_Option ("filter",        'F', Required_Argument, Filter_Threshold);
      R.Add_Option ("help",          'h', No_Argument,       Help);
      R.Add_Option ("hash-pkg",      'H', Required_Argument, Output_Hash);
      R.Add_Option ("jobs",          'j', Required_Argument, Job_Count);
      R.Add_Option ("sx-dict",       'L', No_Argument,       Sx_Dict_Output);
      R.Add_Option ("min-substring", 'm', Required_Argument, Min_Sub_Size);
      R.Add_Option ("max-substring", 'M', Required_Argument, Max_Sub_Size);
      R.Add_Option ("dict-size",     'n', Required_Argument, Dict_Size);
      R.Add_Option ("max-pending",   'N', Required_Argument, Max_Pending);
      R.Add_Option ("stats",         's', No_Argument,       Stat_Output);
      R.Add_Option ("no-stats",      'S', No_Argument,       No_Stat_Output);
      R.Add_Option ("text-list",     't', No_Argument,       Text_List_Input);
      R.Add_Option ("fast-text-list", 'T', No_Argument,       Fast_Text_Input);
      R.Add_Option ("max-word-len",  'W', Required_Argument, Max_Word_Size);
      R.Add_Option ("s-expr",        'x', No_Argument,       Sx_Output);
      R.Add_Option ("no-s-expr",     'X', No_Argument,       No_Sx_Output);
      R.Add_Option ("no-vlen-verbatim",   No_Argument,       No_Vlen_Verbatim);
      R.Add_Option ("score-method",       Required_Argument, Score_Method);
      R.Add_Option ("vlen-verbatim",      No_Argument,       Vlen_Verbatim);

      return R;
   end Getopt_Config;


   procedure Print_Dictionary
     (Filename : in String;
      Dictionary : in Natools.Smaz_256.Dictionary;
      Hash_Package_Name : in String := "") is
   begin
      if Filename = "-" then
         Print_Dictionary
           (Ada.Text_IO.Current_Output, Dictionary, Hash_Package_Name);
      elsif Filename'Length > 0 then
         declare
            File : Ada.Text_IO.File_Type;
         begin
            Ada.Text_IO.Create (File, Name => Filename);
            Print_Dictionary (File, Dictionary, Hash_Package_Name);
            Ada.Text_IO.Close (File);
         end;
      end if;
   end Print_Dictionary;


   procedure Print_Dictionary
     (Output : in Ada.Text_IO.File_Type;
      Dictionary : in Natools.Smaz_256.Dictionary;
      Hash_Package_Name : in String := "")
   is
      procedure Put_Line (Line : in String);

      procedure Put_Line (Line : in String) is
      begin
         Ada.Text_IO.Put_Line (Output, Line);
      end Put_Line;

      procedure Print_Dictionary_In_Ada is
        new Tools_256.Print_Dictionary_In_Ada (Put_Line);
   begin
      if Hash_Package_Name'Length > 0 then
         Print_Dictionary_In_Ada
           (Dictionary,
            Hash_Image => Hash_Package_Name & ".Hash'Access");
      else
         Print_Dictionary_In_Ada (Dictionary);
      end if;
   end Print_Dictionary;


   procedure Print_Help
     (Opt : in Getopt.Configuration;
      Output : in Ada.Text_IO.File_Type)
   is
      use Ada.Text_IO;
      Indent : constant String := "    ";
   begin
      Put_Line (Output, "Usage:");

      for Id in Options.Id loop
         Put (Output, Indent & Opt.Format_Names (Id));

         case Id is
            when Options.Help =>
               New_Line (Output);
               Put_Line (Output, Indent & Indent
                 & "Display this help text");

            when Options.Decode =>
               New_Line (Output);
               Put_Line (Output, Indent & Indent
                 & "Read a list of strings and decode them");

            when Options.Encode =>
               New_Line (Output);
               Put_Line (Output, Indent & Indent
                 & "Read a list of strings and encode them");

            when Options.No_Stat_Output =>
               New_Line (Output);
               Put_Line (Output, Indent & Indent
                 & "Do not output filter statistics");

            when Options.No_Sx_Output =>
               New_Line (Output);
               Put_Line (Output, Indent & Indent
                 & "Do not output filtered results in a S-expression");

            when Options.Output_Ada_Dict =>
               Put_Line (Output, " [filename]");
               Put_Line (Output, Indent & Indent
                 & "Output the current dictionary as Ada code in the given");
               Put_Line (Output, Indent & Indent
                 & "file, or standard output if filename is empty or ""-""");

            when Options.Output_Hash =>
               Put_Line (Output, " <Hash_Package_Name>");
               Put_Line (Output, Indent & Indent
                 & "Build a package with a perfect hash function for the");
               Put_Line (Output, Indent & Indent
                 & "current dictionary.");

            when Options.Stat_Output =>
               New_Line (Output);
               Put_Line (Output, Indent & Indent
                 & "Output filter statistics");

            when Options.Sx_Output =>
               New_Line (Output);
               Put_Line (Output, Indent & Indent
                 & "Output filtered results in a S-expression");

            when Options.Dictionary_Input =>
               New_Line (Output);
               Put_Line (Output, Indent & Indent
                 & "Read dictionary directly in input S-expression (default)");

            when Options.Text_List_Input =>
               New_Line (Output);
               Put_Line (Output, Indent & Indent
                 & "Compute dictionary from sample texts"
                 & " in input S-expression");

            when Options.Fast_Text_Input =>
               New_Line (Output);
               Put_Line (Output, Indent & Indent
                 & "Compute dictionary from sample texts"
                 & " in input S-expression, without optimization");

            when Options.Sx_Dict_Output =>
               New_Line (Output);
               Put_Line (Output, Indent & Indent
                 & "Output the dictionary as a S-expression");

            when Options.Min_Sub_Size =>
               Put_Line (Output, " <length>");
               Put_Line (Output, Indent & Indent
                 & "Minimum substring size when building a dictionary");

            when Options.Max_Sub_Size =>
               Put_Line (Output, " <length>");
               Put_Line (Output, Indent & Indent
                 & "Maximum substring size when building a dictionary");

            when Options.Max_Word_Size =>
               Put_Line (Output, " <length>");
               Put_Line (Output, Indent & Indent
                 & "Maximum word size when building a dictionary");

            when Options.Evaluate =>
               New_Line (Output);
               Put_Line (Output, Indent & Indent
                 & "Evaluate the dictionary on the input given corpus");

            when Options.Job_Count =>
               Put_Line (Output, " <number>");
               Put_Line (Output, Indent & Indent
                 & "Number of parallel jobs in long calculations");

            when Options.Filter_Threshold =>
               Put_Line (Output, " <threshold>");
               Put_Line (Output, Indent & Indent
                 & "Before building a dictionary from substrings, remove");
               Put_Line (Output, Indent & Indent
                 & "substrings whose count is below the threshold.");

            when Options.Score_Method =>
               Put_Line (Output, " <method>");
               Put_Line (Output, Indent & Indent
                 & "Select heuristic method to replace dictionary items"
                 & " during optimization");

            when Options.Max_Pending =>
               Put_Line (Output, " <count>");
               Put_Line (Output, Indent & Indent
                 & "Maximum size of candidate list"
                 & " when building a dictionary");

            when Options.Dict_Size =>
               Put_Line (Output, " <count>");
               Put_Line (Output, Indent & Indent
                 & "Number of words in the dictionary to build");

            when Options.Vlen_Verbatim =>
               New_Line (Output);
               Put_Line (Output, Indent & Indent
                 & "Enable variable-length verbatim in built dictionary");

            when Options.No_Vlen_Verbatim =>
               New_Line (Output);
               Put_Line (Output, Indent & Indent
                 & "Disable variable-length verbatim in built dictionary");
         end case;
      end loop;
   end Print_Help;


   procedure Process
     (Handler : in Callback'Class;
      Word_List : in Natools.Smaz_Tools.String_Lists.List;
      Data_List : in Natools.Smaz_Tools.String_Lists.List)
   is
      Dictionary : Natools.Smaz_256.Dictionary
        := To_Dictionary (Handler, Word_List);
      Sx_Output : Natools.S_Expressions.Printers.Canonical
        (Ada.Text_IO.Text_Streams.Stream (Ada.Text_IO.Current_Output));
      Ada_Dictionary : constant String
        := Ada.Strings.Unbounded.To_String (Handler.Ada_Dictionary);
      Hash_Package : constant String
        := Ada.Strings.Unbounded.To_String (Handler.Hash_Package);
   begin
      Natools.Smaz_Tools.Set_Dictionary_For_Trie_Search (Word_List);
      Dictionary.Hash := Natools.Smaz_Tools.Trie_Search'Access;

      if Ada_Dictionary'Length > 0 then
         Print_Dictionary (Ada_Dictionary, Dictionary, Hash_Package);
      end if;

      if Hash_Package'Length > 0 then
         Natools.Smaz_Tools.GNAT.Build_Perfect_Hash (Word_List, Hash_Package);
      end if;

      if Handler.Sx_Dict_Output then
         Sx_Output.Open_List;
         for I in Dictionary.Offsets'Range loop
            Sx_Output.Append_String
              (Natools.Smaz_256.Dict_Entry (Dictionary, I));
         end loop;
         Sx_Output.Close_List;
      end if;

      case Handler.Action is
         when Actions.Nothing => null;

         when Actions.Decode =>
            if Handler.Sx_Output then
               Sx_Output.Open_List;
               for S of Data_List loop
                  Sx_Output.Append_String
                    (Natools.Smaz_256.Decompress (Dictionary, To_SEA (S)));
               end loop;
               Sx_Output.Close_List;
            end if;

            if Handler.Stat_Output then
               declare
                  procedure Print_Line (Original, Output : Natural);

                  procedure Print_Line (Original, Output : Natural) is
                  begin
                     Ada.Text_IO.Put_Line
                       (Natural'Image (Original)
                        & Ada.Characters.Latin_1.HT
                        & Natural'Image (Output)
                        & Ada.Characters.Latin_1.HT
                        & Float'Image (Float (Original) / Float (Output)));
                  end Print_Line;
                  Original_Total : Natural := 0;
                  Output_Total : Natural := 0;
               begin
                  for S of Data_List loop
                     declare
                        Original_Size : constant Natural := S'Length;
                        Output_Size : constant Natural
                          := Natools.Smaz_256.Decompress
                             (Dictionary, To_SEA (S))'Length;
                     begin
                        Print_Line (Original_Size, Output_Size);
                        Original_Total := Original_Total + Original_Size;
                        Output_Total := Output_Total + Output_Size;
                     end;
                  end loop;

                  Print_Line (Original_Total, Output_Total);
               end;
            end if;

         when Actions.Encode =>
            if Handler.Sx_Output then
               Sx_Output.Open_List;
               for S of Data_List loop
                  Sx_Output.Append_Atom
                    (Natools.Smaz_256.Compress (Dictionary, S));
               end loop;
               Sx_Output.Close_List;
            end if;

            if Handler.Stat_Output then
               declare
                  procedure Print_Line (Original, Output, Base64 : Natural);

                  procedure Print_Line (Original, Output, Base64 : Natural) is
                  begin
                     Ada.Text_IO.Put_Line
                       (Natural'Image (Original)
                        & Ada.Characters.Latin_1.HT
                        & Natural'Image (Output)
                        & Ada.Characters.Latin_1.HT
                        & Natural'Image (Base64)
                        & Ada.Characters.Latin_1.HT
                        & Float'Image (Float (Output) / Float (Original))
                        & Ada.Characters.Latin_1.HT
                        & Float'Image (Float (Base64) / Float (Original)));
                  end Print_Line;
                  Original_Total : Natural := 0;
                  Output_Total : Natural := 0;
                  Base64_Total : Natural := 0;
               begin
                  for S of Data_List loop
                     declare
                        Original_Size : constant Natural := S'Length;
                        Output_Size : constant Natural
                          := Natools.Smaz_256.Compress (Dictionary, S)'Length;
                        Base64_Size : constant Natural
                          := ((Output_Size + 2) / 3) * 4;
                     begin
                        Print_Line (Original_Size, Output_Size, Base64_Size);
                        Original_Total := Original_Total + Original_Size;
                        Output_Total := Output_Total + Output_Size;
                        Base64_Total := Base64_Total + Base64_Size;
                     end;
                  end loop;

                  Print_Line (Original_Total, Output_Total, Base64_Total);
               end;
            end if;

         when Actions.Evaluate =>
            declare
               Total_Size : Ada.Streams.Stream_Element_Count;
               Counts : Tools_256.Dictionary_Counts;
            begin
               Dict_256.Evaluate_Dictionary (Handler.Job_Count,
                  Dictionary, Data_List, Total_Size, Counts);

               if Handler.Sx_Output then
                  Sx_Output.Open_List;
                  Sx_Output.Append_String (Ada.Strings.Fixed.Trim
                    (Ada.Streams.Stream_Element_Count'Image (Total_Size),
                     Ada.Strings.Both));

                  for E in Dictionary.Offsets'Range loop
                     Sx_Output.Open_List;
                     Sx_Output.Append_Atom ((0 => E));
                     Sx_Output.Append_String
                       (Natools.Smaz_256.Dict_Entry (Dictionary, E));
                     Sx_Output.Append_String (Ada.Strings.Fixed.Trim
                       (Natools.Smaz_Tools.String_Count'Image (Counts (E)),
                        Ada.Strings.Both));
                     Sx_Output.Close_List;
                  end loop;
                  Sx_Output.Close_List;
               end if;

               if Handler.Stat_Output then
                  declare
                     procedure Print
                       (Label : in String;
                        E : in Ada.Streams.Stream_Element;
                        Score : in Natools.Smaz_Tools.Score_Value);

                     procedure Print_Min_Max
                       (Label : in String;
                        Score : not null access function
                          (D : in Natools.Smaz_256.Dictionary;
                           C : in Tools_256.Dictionary_Counts;
                           E : in Ada.Streams.Stream_Element)
                          return Natools.Smaz_Tools.Score_Value);

                     procedure Print_Value
                       (Label : in String;
                        Score : not null access function
                          (D : in Natools.Smaz_256.Dictionary;
                           C : in Tools_256.Dictionary_Counts;
                           E : in Ada.Streams.Stream_Element)
                          return Natools.Smaz_Tools.Score_Value;
                        Ref : in Natools.Smaz_Tools.Score_Value);


                     procedure Print
                       (Label : in String;
                        E : in Ada.Streams.Stream_Element;
                        Score : in Natools.Smaz_Tools.Score_Value) is
                     begin
                        if Handler.Sx_Output then
                           Sx_Output.Open_List;
                           Sx_Output.Append_Atom ((0 => E));
                           Sx_Output.Append_String
                             (Natools.Smaz_256.Dict_Entry (Dictionary, E));
                           Sx_Output.Append_String (Ada.Strings.Fixed.Trim
                             (Score'Img, Ada.Strings.Both));
                           Sx_Output.Close_List;
                        else
                           Ada.Text_IO.Put_Line
                             (Label
                              & Ada.Characters.Latin_1.HT
                              & Ada.Streams.Stream_Element'Image (E)
                              & Ada.Characters.Latin_1.HT
                              & Natools.String_Escapes.C_Escape_Hex
                                (Natools.Smaz_256.Dict_Entry (Dictionary, E),
                                 True)
                              & Ada.Characters.Latin_1.HT
                              & Score'Img);
                        end if;
                     end Print;

                     procedure Print_Min_Max
                       (Label : in String;
                        Score : not null access function
                          (D : in Natools.Smaz_256.Dictionary;
                           C : in Tools_256.Dictionary_Counts;
                           E : in Ada.Streams.Stream_Element)
                          return Natools.Smaz_Tools.Score_Value)
                     is
                        use type Natools.Smaz_Tools.Score_Value;
                        Min_Score, Max_Score : Natools.Smaz_Tools.Score_Value
                          := Score (Dictionary, Counts, 0);
                        S : Natools.Smaz_Tools.Score_Value;
                     begin
                        for E in 1 .. Dictionary.Last_Code loop
                           S := Score (Dictionary, Counts, E);
                           if S < Min_Score then
                              Min_Score := S;
                           end if;
                           if S > Max_Score then
                              Max_Score := S;
                           end if;
                        end loop;

                        Print_Value ("best-" & Label, Score, Max_Score);
                        Print_Value ("worst-" & Label, Score, Min_Score);
                     end Print_Min_Max;

                     procedure Print_Value
                       (Label : in String;
                        Score : not null access function
                          (D : in Natools.Smaz_256.Dictionary;
                           C : in Tools_256.Dictionary_Counts;
                           E : in Ada.Streams.Stream_Element)
                          return Natools.Smaz_Tools.Score_Value;
                        Ref : in Natools.Smaz_Tools.Score_Value)
                     is
                        use type Natools.Smaz_Tools.Score_Value;
                     begin
                        if Handler.Sx_Output then
                           Sx_Output.Open_List;
                           Sx_Output.Append_String (Label);
                        end if;

                        for E in Dictionary.Offsets'Range loop
                           if Score (Dictionary, Counts, E) = Ref then
                              Print (Label, E, Ref);
                           end if;
                        end loop;

                        if Handler.Sx_Output then
                           Sx_Output.Close_List;
                        end if;
                     end Print_Value;
                  begin
                     Print_Min_Max ("encoded",
                       Tools_256.Score_Encoded'Access);
                     Print_Min_Max ("frequency",
                       Tools_256.Score_Frequency'Access);
                     Print_Min_Max ("gain",
                       Tools_256.Score_Gain'Access);
                  end;
               end if;
            end;
      end case;
   end Process;


   function To_Dictionary
     (Handler : in Callback'Class;
      Input : in Natools.Smaz_Tools.String_Lists.List)
     return Natools.Smaz_256.Dictionary
   is
      use type Natools.Smaz_Tools.String_Count;
      use type Dict_Sources.Enum;
   begin
      case Handler.Dict_Source is
         when Dict_Sources.S_Expression =>
            return Tools_256.To_Dictionary (Input, Handler.Vlen_Verbatim);

         when Dict_Sources.Text_List | Dict_Sources.Unoptimized_Text_List =>
            declare
               Counter : Natools.Smaz_Tools.Word_Counter;
            begin
               for S of Input loop
                  Natools.Smaz_Tools.Add_Substrings
                    (Counter, S, Handler.Min_Sub_Size, Handler.Max_Sub_Size);

                  if Handler.Max_Word_Size > Handler.Max_Sub_Size then
                     Natools.Smaz_Tools.Add_Words
                       (Counter, S,
                        Handler.Max_Sub_Size + 1, Handler.Max_Word_Size);
                  end if;
               end loop;

               if Handler.Filter_Threshold > 0 then
                  Natools.Smaz_Tools.Filter_By_Count
                    (Counter, Handler.Filter_Threshold);
               end if;

               if Handler.Dict_Source = Dict_Sources.Text_List then
                  declare
                     Selected, Pending : Natools.Smaz_Tools.String_Lists.List;
                  begin
                     Natools.Smaz_Tools.Simple_Dictionary_And_Pending
                       (Counter,
                        Handler.Dict_Size,
                        Selected,
                        Pending,
                        Handler.Score_Method,
                        Handler.Max_Pending);

                     return Dict_256.Optimize_Dictionary
                       (Tools_256.To_Dictionary
                          (Selected, Handler.Vlen_Verbatim),
                        Pending,
                        Input,
                        Handler.Job_Count,
                        Handler.Score_Method);
                  end;
               else
                  return Tools_256.To_Dictionary
                    (Natools.Smaz_Tools.Simple_Dictionary
                       (Counter, Handler.Dict_Size, Handler.Score_Method),
                     Handler.Vlen_Verbatim);
               end if;
            end;
      end case;
   end To_Dictionary;


   procedure Use_Dictionary (Dict : in out Natools.Smaz_256.Dictionary) is
   begin
      Natools.Smaz_Tools.Set_Dictionary_For_Trie_Search
        (Tools_256.To_String_List (Dict));
      Dict.Hash := Natools.Smaz_Tools.Trie_Search'Access;

      for I in Dict.Offsets'Range loop
         if Natools.Smaz_Tools.Trie_Search (Natools.Smaz_256.Dict_Entry
           (Dict, I)) /= Natural (I)
         then
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Current_Error,
               "Fail at" & Ada.Streams.Stream_Element'Image (I)
               & " -> " & Natools.String_Escapes.C_Escape_Hex
                  (Natools.Smaz_256.Dict_Entry (Dict, I), True)
               & " ->" & Natural'Image (Natools.Smaz_Tools.Trie_Search
                  (Natools.Smaz_256.Dict_Entry (Dict, I))));
         end if;
      end loop;
   end Use_Dictionary;


   Opt_Config : constant Getopt.Configuration := Getopt_Config;
   Handler : Callback;
   Input_List, Input_Data : Natools.Smaz_Tools.String_Lists.List;
begin
   Process_Command_Line :
   begin
      Opt_Config.Process (Handler);
   exception
      when Getopt.Option_Error =>
         Print_Help (Opt_Config, Ada.Text_IO.Current_Error);
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
         return;
   end Process_Command_Line;

   if Handler.Display_Help then
      Print_Help (Opt_Config, Ada.Text_IO.Current_Output);
   end if;

   if not Handler.Need_Dictionary then
      return;
   end if;

   if not (Handler.Stat_Output or Handler.Sx_Output) then
      Handler.Sx_Output := True;
   end if;

   Read_Input_List :
   declare
      use type Actions.Enum;

      Input : constant access Ada.Streams.Root_Stream_Type'Class
        := Ada.Text_IO.Text_Streams.Stream (Ada.Text_IO.Current_Input);
      Parser : Natools.S_Expressions.Parsers.Stream_Parser (Input);
   begin
      Parser.Next;
      Natools.Smaz_Tools.Read_List (Input_List, Parser);

      if Handler.Action /= Actions.Nothing then
         Parser.Next;
         Natools.Smaz_Tools.Read_List (Input_Data, Parser);
      end if;
   end Read_Input_List;

   Process (Handler, Input_List, Input_Data);

end Smaz;
