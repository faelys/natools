------------------------------------------------------------------------------
-- Copyright (c) 2016-2017, Natacha Porté                                   --
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
with Natools.Smaz;
with Natools.Smaz.Tools;
with Natools.Smaz_256;
with Natools.Smaz_4096;
with Natools.Smaz_64;
with Natools.Smaz_Generic.Tools;
with Natools.Smaz_Implementations.Base_4096;
with Natools.Smaz_Implementations.Base_64_Tools;
with Natools.Smaz_Tools;
with Natools.Smaz_Tools.GNAT;
with Natools.String_Escapes;

procedure Smaz is
   function To_SEA (S : String) return Ada.Streams.Stream_Element_Array
     renames Natools.S_Expressions.To_Atom;

   package Tools_256 is new Natools.Smaz_256.Tools;
   package Tools_4096 is new Natools.Smaz_4096.Tools;
   package Tools_64 is new Natools.Smaz_64.Tools;

   package Methods renames Natools.Smaz_Tools.Methods;

   package Actions is
      type Enum is
        (Nothing,
         Adjust_Dictionary,
         Decode,
         Encode,
         Evaluate);
   end Actions;

   package Algorithms is
      type Enum is
        (Base_256,
         Base_4096,
         Base_64,
         Base_256_Retired);
   end Algorithms;

   package Dict_Sources is
      type Enum is
        (S_Expression,
         Text_List,
         Unoptimized_Text_List);
   end Dict_Sources;

   package Options is
      type Id is
        (Base_256,
         Base_4096,
         Base_64,
         Output_Ada_Dict,
         Check_Roundtrip,
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
         Base_256_Retired,
         Stat_Output,
         No_Stat_Output,
         Text_List_Input,
         Fast_Text_Input,
         Max_Word_Size,
         Sx_Output,
         No_Sx_Output,
         Force_Word,
         Max_Dict_Size,
         Min_Dict_Size,
         No_Vlen_Verbatim,
         Score_Method,
         Vlen_Verbatim);
   end Options;

   package Getopt is new Natools.Getopt_Long (Options.Id);

   type Callback is new Getopt.Handlers.Callback with record
      Algorithm : Algorithms.Enum := Algorithms.Base_256;
      Display_Help : Boolean := False;
      Need_Dictionary : Boolean := False;
      Stat_Output : Boolean := False;
      Sx_Output : Boolean := False;
      Sx_Dict_Output : Boolean := False;
      Min_Sub_Size : Positive := 1;
      Max_Sub_Size : Positive := 3;
      Max_Word_Size : Positive := 10;
      Max_Dict_Size : Positive := 254;
      Min_Dict_Size : Positive := 254;
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
      Check_Roundtrip : Boolean := False;
      Forced_Words : Natools.Smaz_Tools.String_Lists.List;
   end record;

   overriding procedure Option
     (Handler  : in out Callback;
      Id       : in Options.Id;
      Argument : in String);

   overriding procedure Argument
     (Handler  : in out Callback;
      Argument : in String)
     is null;


   function Activate_Dictionary (Dict : in Natools.Smaz_256.Dictionary)
     return Natools.Smaz_256.Dictionary;
   function Activate_Dictionary (Dict : in Natools.Smaz_4096.Dictionary)
     return Natools.Smaz_4096.Dictionary;
   function Activate_Dictionary (Dict : in Natools.Smaz_64.Dictionary)
     return Natools.Smaz_64.Dictionary;
   function Activate_Dictionary (Dict : in Natools.Smaz.Dictionary)
     return Natools.Smaz.Dictionary;
      --  Update Dictionary.Hash so that it can be actually used

   procedure Build_Perfect_Hash
     (Word_List : in Natools.Smaz.Tools.String_Lists.List;
      Package_Name : in String);
      --  Adapter between Smaz_256 generator and retired Smaz types

   procedure Convert
     (Input : in Natools.Smaz_Tools.String_Lists.List;
      Output : out Natools.Smaz.Tools.String_Lists.List);
      --  Convert between old and new string lists

   function Getopt_Config return Getopt.Configuration;
      --  Build the configuration object

   function Last_Code (Dict : in Natools.Smaz_256.Dictionary)
     return Ada.Streams.Stream_Element
     is (Dict.Last_Code);
   function Last_Code (Dict : in Natools.Smaz_4096.Dictionary)
     return Natools.Smaz_Implementations.Base_4096.Base_4096_Digit
     is (Dict.Last_Code);
   function Last_Code (Dict : in Natools.Smaz_64.Dictionary)
     return Natools.Smaz_Implementations.Base_64_Tools.Base_64_Digit
     is (Dict.Last_Code);
   function Last_Code (Dict : in Natools.Smaz.Dictionary)
     return Ada.Streams.Stream_Element
     is (Dict.Dict_Last);
      --  Return the last valid entry

   function Length (Dict : in Natools.Smaz_256.Dictionary) return Positive
     is (Dict.Offsets'Length + 1);
   function Length (Dict : in Natools.Smaz_4096.Dictionary) return Positive
     is (Dict.Offsets'Length + 1);
   function Length (Dict : in Natools.Smaz_64.Dictionary) return Positive
     is (Dict.Offsets'Length + 1);
   function Length (Dict : in Natools.Smaz.Dictionary) return Positive
     is (Dict.Offsets'Length);
      --  Return the number of entries in Dict

   procedure Print_Dictionary
     (Output : in Ada.Text_IO.File_Type;
      Dictionary : in Natools.Smaz_256.Dictionary;
      Hash_Package_Name : in String := "");
   procedure Print_Dictionary
     (Output : in Ada.Text_IO.File_Type;
      Dictionary : in Natools.Smaz_4096.Dictionary;
      Hash_Package_Name : in String := "");
   procedure Print_Dictionary
     (Output : in Ada.Text_IO.File_Type;
      Dictionary : in Natools.Smaz_64.Dictionary;
      Hash_Package_Name : in String := "");
   procedure Print_Dictionary
     (Output : in Ada.Text_IO.File_Type;
      Dictionary : in Natools.Smaz.Dictionary;
      Hash_Package_Name : in String := "");
      --  print the given dictionary in the given file

   procedure Print_Help
     (Opt : in Getopt.Configuration;
      Output : in Ada.Text_IO.File_Type);
      --  Print the help text to the given file



   generic
      type Dictionary (<>) is private;
      type Dictionary_Entry is (<>);
      type Methods is (<>);
      type Score_Value is range <>;
      type String_Count is range <>;
      type Word_Counter is private;

      type Dictionary_Counts is array (Dictionary_Entry) of String_Count;

      with package String_Lists
        is new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

      with function Activate_Dictionary (Dict : in Dictionary)
        return Dictionary is <>;

      with procedure Add_Substrings
        (Counter : in out Word_Counter;
         Phrase : in String;
         Min_Size : in Positive;
         Max_Size : in Positive);

      with procedure Add_Words
        (Counter : in out Word_Counter;
         Phrase : in String;
         Min_Size : in Positive;
         Max_Size : in Positive);

      with function Append_String
        (Dict : in Dictionary;
         Element : in String)
        return Dictionary;

      with procedure Build_Perfect_Hash
        (Word_List : in String_Lists.List;
         Package_Name : in String);

      with function Compress
        (Dict : in Dictionary;
         Input : in String)
        return Ada.Streams.Stream_Element_Array;

      with function Decompress
        (Dict : in Dictionary;
         Input : in Ada.Streams.Stream_Element_Array)
        return String;

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

      with procedure Filter_By_Count
        (Counter : in out Word_Counter;
         Threshold_Count : in String_Count);

      with function Last_Code (Dict : in Dictionary) return Dictionary_Entry;

      with function Length (Dict : in Dictionary) return Positive is <>;

      with procedure Print_Dictionary
        (Output : in Ada.Text_IO.File_Type;
         Dict : in Dictionary;
         Hash_Package_Name : in String := "")
        is <>;

      with function Remove_Element
        (Dict : in Dictionary;
         Element : in Dictionary_Entry)
        return Dictionary;

      with function Replace_Element
        (Dict : in Dictionary;
         Element : in Dictionary_Entry;
         Value : in String)
        return Dictionary;

      Score_Encoded, Score_Frequency, Score_Gain : in access function
        (D : in Dictionary;
         C : in Dictionary_Counts;
         E : in Dictionary_Entry)
        return Score_Value;

      with function Simple_Dictionary
        (Counter : in Word_Counter;
         Word_Count : in Natural;
         Method : in Methods)
        return String_Lists.List;

      with procedure Simple_Dictionary_And_Pending
        (Counter : in Word_Counter;
         Word_Count : in Natural;
         Selected : out String_Lists.List;
         Pending : out String_Lists.List;
         Method : in Methods;
         Max_Pending_Count : in Ada.Containers.Count_Type);

      with function To_Dictionary
        (List : in String_Lists.List;
         Variable_Length_Verbatim : in Boolean)
        return Dictionary;

      with function Worst_Element
        (Dict : in Dictionary;
         Counts : in Dictionary_Counts;
         Method : in Methods;
         First, Last : in Dictionary_Entry)
        return Dictionary_Entry;

   package Dictionary_Subprograms is

      package Holders is new Ada.Containers.Indefinite_Holders (Dictionary);

      function Adjust_Dictionary
        (Handler : in Callback'Class;
         Dict : in Dictionary;
         Corpus : in String_Lists.List;
         Method : in Methods)
        return Dictionary;
         --  Adjust the given dictionary according to info in Handle

      procedure Evaluate_Dictionary
        (Job_Count : in Natural;
         Dict : in Dictionary;
         Corpus : in String_Lists.List;
         Compressed_Size : out Ada.Streams.Stream_Element_Count;
         Counts : out Dictionary_Counts);
         --  Dispatch to parallel or non-parallel version of
         --  Evaluate_Dictionary depending on Job_Count.

      function Image
        (Dict : in Dictionary;
         Code : in Dictionary_Entry)
        return Natools.S_Expressions.Atom;
         --  S-expression image of Code

      function Is_In_Dict (Dict : Dictionary; Word : String) return Boolean;
         --  Return whether Word is in Dict (inefficient)

      function Make_Word_Counter
        (Handler : in Callback'Class;
         Input : in String_Lists.List)
        return Word_Counter;
         --  Make a word counter from an input word list

      procedure Optimization_Round
        (Dict : in out Holders.Holder;
         Score : in out Ada.Streams.Stream_Element_Count;
         Counts : in out Dictionary_Counts;
         First : in Dictionary_Entry;
         Pending_Words : in out String_Lists.List;
         Input_Texts : in String_Lists.List;
         Job_Count : in Natural;
         Method : in Methods;
         Min_Dict_Size : in Positive;
         Max_Dict_Size : in Positive;
         Updated : out Boolean);
      --  Try to improve on Dict by replacing a single entry from it with
      --  one of the substring in Pending_Words.

      function Optimize_Dictionary
        (Base : in Dictionary;
         First : in Dictionary_Entry;
         Pending_Words : in String_Lists.List;
         Input_Texts : in String_Lists.List;
         Job_Count : in Natural;
         Method : in Methods;
         Min_Dict_Size : in Positive;
         Max_Dict_Size : in Positive)
        return Dictionary;
      --  Optimize the dictionary on Input_Texts, starting with Base and
      --  adding substrings from Pending_Words. Operates only on words
      --  at First and beyond.

      procedure Parallel_Evaluate_Dictionary
        (Job_Count : in Positive;
         Dict : in Dictionary;
         Corpus : in String_Lists.List;
         Compressed_Size : out Ada.Streams.Stream_Element_Count;
         Counts : out Dictionary_Counts);
         --  Return the same results as Natools.Smaz.Tools.Evaluate_Dictionary,
         --  but hopefully more quickly, using Job_Count tasks.

      procedure Print_Dictionary
        (Filename : in String;
         Dict : in Dictionary;
         Hash_Package_Name : in String := "");
         --  print the given dictionary in the given file

      procedure Process
        (Handler : in Callback'Class;
         Word_List : in String_Lists.List;
         Data_List : in String_Lists.List;
         Method : in Methods);
         --  Perform the requested operations

      function To_Dictionary
        (Handler : in Callback'Class;
         Input : in String_Lists.List;
         Data_List : in String_Lists.List;
         Method : in Methods)
        return Dictionary;
         --  Convert the input into a dictionary given the option in Handler

   end Dictionary_Subprograms;



   package body Dictionary_Subprograms is

      function Adjust_Dictionary
        (Handler : in Callback'Class;
         Dict : in Dictionary;
         Corpus : in String_Lists.List;
         Method : in Methods)
        return Dictionary is
      begin
         if Handler.Forced_Words.Is_Empty or else Corpus.Is_Empty then
            return Dict;
         end if;

         Add_Forced_Words :
         declare
            Actual_Dict : constant Dictionary := Activate_Dictionary (Dict);
            Counts : Dictionary_Counts;
            Discarded_Size : Ada.Streams.Stream_Element_Count;
            Replacement_Count : String_Count;
            Current : Holders.Holder := Holders.To_Holder (Actual_Dict);
         begin
            Evaluate_Dictionary
              (Handler.Job_Count, Actual_Dict, Corpus, Discarded_Size, Counts);

            Replacement_Count := Counts (Counts'First);
            for I in Counts'Range loop
               if Replacement_Count < Counts (I) then
                  Replacement_Count := Counts (I);
               end if;
            end loop;

            for Word of Handler.Forced_Words loop
               if not Is_In_Dict (Actual_Dict, Word) then
                  declare
                     Worst_Index : constant Dictionary_Entry
                       := Worst_Element
                          (Actual_Dict, Counts, Method,
                           Dictionary_Entry'First, Last_Code (Actual_Dict));
                     New_Dict : constant Dictionary
                       := Replace_Element (Current.Element, Worst_Index, Word);
                  begin
                     Ada.Text_IO.Put_Line
                       (Ada.Text_IO.Current_Error,
                        "Removing"
                        & Counts (Worst_Index)'Img & "x "
                        & Natools.String_Escapes.C_Escape_Hex
                          (Dict_Entry (Actual_Dict, Worst_Index), True)
                        & " at"
                        & Worst_Index'Img
                        & ", replaced by "
                        & Natools.String_Escapes.C_Escape_Hex (Word, True));

                     Current := Holders.To_Holder (New_Dict);
                     Counts (Worst_Index) := Replacement_Count;
                  end;
               end if;
            end loop;

            return Current.Element;
         end Add_Forced_Words;
      end Adjust_Dictionary;


      procedure Evaluate_Dictionary
        (Job_Count : in Natural;
         Dict : in Dictionary;
         Corpus : in String_Lists.List;
         Compressed_Size : out Ada.Streams.Stream_Element_Count;
         Counts : out Dictionary_Counts)
      is
         Actual_Dict : constant Dictionary := Activate_Dictionary (Dict);
      begin
         if Job_Count > 0 then
            Parallel_Evaluate_Dictionary (Job_Count,
               Actual_Dict, Corpus, Compressed_Size, Counts);
         else
            Evaluate_Dictionary
              (Actual_Dict, Corpus, Compressed_Size, Counts);
         end if;
      end Evaluate_Dictionary;


      function Image
        (Dict : in Dictionary;
         Code : in Dictionary_Entry)
        return Natools.S_Expressions.Atom is
      begin
         return Compress (Dict, Dict_Entry (Dict, Code));
      end Image;


      function Is_In_Dict (Dict : Dictionary; Word : String) return Boolean is
      begin
         for Code in Dictionary_Entry'First .. Last_Code (Dict) loop
            if Dict_Entry (Dict, Code) = Word then
               return True;
            end if;
         end loop;

         return False;
      end Is_In_Dict;


      function Make_Word_Counter
        (Handler : in Callback'Class;
         Input : in String_Lists.List)
        return Word_Counter
      is
         use type Natools.Smaz_Tools.String_Count;
         Counter : Word_Counter;
      begin
         for S of Input loop
            Add_Substrings
              (Counter, S,
               Handler.Min_Sub_Size, Handler.Max_Sub_Size);

            if Handler.Max_Word_Size > Handler.Max_Sub_Size then
               Add_Words
                 (Counter, S,
                  Handler.Max_Sub_Size + 1, Handler.Max_Word_Size);
            end if;
         end loop;

         if Handler.Filter_Threshold > 0 then
            Filter_By_Count (Counter, String_Count (Handler.Filter_Threshold));
         end if;

         return Counter;
      end Make_Word_Counter;


      procedure Optimization_Round
        (Dict : in out Holders.Holder;
         Score : in out Ada.Streams.Stream_Element_Count;
         Counts : in out Dictionary_Counts;
         First : in Dictionary_Entry;
         Pending_Words : in out String_Lists.List;
         Input_Texts : in String_Lists.List;
         Job_Count : in Natural;
         Method : in Methods;
         Min_Dict_Size : in Positive;
         Max_Dict_Size : in Positive;
         Updated : out Boolean)
      is
         pragma Unreferenced (Max_Dict_Size);
         use type Ada.Streams.Stream_Element_Offset;

         No_Longer_Pending : String_Lists.Cursor;
         Log_Message : Ada.Strings.Unbounded.Unbounded_String;
         Original : constant Dictionary := Dict.Element;
         Worst_Index : constant Dictionary_Entry
           := Worst_Element
              (Original, Counts, Method, First, Last_Code (Original));
         Worst_Value : constant String
           := Dict_Entry (Original, Worst_Index);
         Worst_Count : constant String_Count := Counts (Worst_Index);
         Worst_Removed : Boolean := False;
         Base : constant Dictionary
           := Remove_Element (Original, Worst_Index);
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
                  No_Longer_Pending := Position;
                  Worst_Removed := True;
                  Updated := True;
                  Log_Message := Ada.Strings.Unbounded.To_Unbounded_String
                    ("Removing"
                     & Worst_Count'Img & "x "
                     & Natools.String_Escapes.C_Escape_Hex (Worst_Value, True)
                     & ", adding"
                     & Counts (Last_Code (New_Dict))'Img & "x "
                     & Natools.String_Escapes.C_Escape_Hex (Word, True)
                     & ", size"
                     & Score'Img
                     & " ("
                     & Ada.Streams.Stream_Element_Offset'Image
                        (Score - Old_Score)
                     & ')');
               end if;
            end;
         end loop;

         if Length (Base) >= Min_Dict_Size then
            declare
               New_Score : Ada.Streams.Stream_Element_Count;
               New_Counts : Dictionary_Counts;
            begin
               Evaluate_Dictionary
                 (Job_Count, Base, Input_Texts, New_Score, New_Counts);

               if New_Score <= Score then
                  Dict := Holders.To_Holder (Base);
                  Score := New_Score;
                  Counts := New_Counts;
                  No_Longer_Pending := String_Lists.No_Element;
                  Worst_Removed := True;
                  Updated := True;
                  Log_Message := Ada.Strings.Unbounded.To_Unbounded_String
                    ("Removing"
                     & Worst_Count'Img & "x "
                     & Natools.String_Escapes.C_Escape_Hex (Worst_Value, True)
                     & ", size"
                     & Score'Img
                     & " ("
                     & Ada.Streams.Stream_Element_Offset'Image
                        (Score - Old_Score)
                     & ')');
               end if;
            end;
         end if;

         if Updated then
            if String_Lists.Has_Element (No_Longer_Pending) then
               Pending_Words.Delete (No_Longer_Pending);
            end if;

            if Worst_Removed then
               Pending_Words.Append (Worst_Value);
            end if;

            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Current_Error,
               Ada.Strings.Unbounded.To_String (Log_Message));
         end if;
      end Optimization_Round;


      function Optimize_Dictionary
        (Base : in Dictionary;
         First : in Dictionary_Entry;
         Pending_Words : in String_Lists.List;
         Input_Texts : in String_Lists.List;
         Job_Count : in Natural;
         Method : in Methods;
         Min_Dict_Size : in Positive;
         Max_Dict_Size : in Positive)
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
               First,
               Pending,
               Input_Texts,
               Job_Count,
               Method,
               Min_Dict_Size,
               Max_Dict_Size,
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


      procedure Print_Dictionary
        (Filename : in String;
         Dict : in Dictionary;
         Hash_Package_Name : in String := "") is
      begin
         if Filename = "-" then
            Print_Dictionary
              (Ada.Text_IO.Current_Output, Dict, Hash_Package_Name);
         elsif Filename'Length > 0 then
            declare
               File : Ada.Text_IO.File_Type;
            begin
               Ada.Text_IO.Create (File, Name => Filename);
               Print_Dictionary (File, Dict, Hash_Package_Name);
               Ada.Text_IO.Close (File);
            end;
         end if;
      end Print_Dictionary;


      procedure Process
        (Handler : in Callback'Class;
         Word_List : in String_Lists.List;
         Data_List : in String_Lists.List;
         Method : in Methods)
      is
         Dict : constant Dictionary := Activate_Dictionary
           (To_Dictionary (Handler, Word_List, Data_List, Method));
         Sx_Output : Natools.S_Expressions.Printers.Canonical
           (Ada.Text_IO.Text_Streams.Stream (Ada.Text_IO.Current_Output));
         Ada_Dictionary : constant String
           := Ada.Strings.Unbounded.To_String (Handler.Ada_Dictionary);
         Hash_Package : constant String
           := Ada.Strings.Unbounded.To_String (Handler.Hash_Package);
      begin
         if Ada_Dictionary'Length > 0 then
            Print_Dictionary (Ada_Dictionary, Dict, Hash_Package);
         end if;

         if Hash_Package'Length > 0 then
            Build_Perfect_Hash (Word_List, Hash_Package);
         end if;

         if Handler.Sx_Dict_Output then
            Sx_Output.Open_List;
            for I in Dictionary_Entry'First .. Last_Code (Dict) loop
               Sx_Output.Append_String (Dict_Entry (Dict, I));
            end loop;
            Sx_Output.Close_List;
         end if;

         case Handler.Action is
            when Actions.Nothing | Actions.Adjust_Dictionary => null;

            when Actions.Decode =>
               if Handler.Sx_Output then
                  Sx_Output.Open_List;
                  for S of Data_List loop
                     Sx_Output.Append_String (Decompress (Dict, To_SEA (S)));
                  end loop;
                  Sx_Output.Close_List;
               end if;

               if Handler.Check_Roundtrip then
                  for S of Data_List loop
                     declare
                        use type Ada.Streams.Stream_Element_Array;
                        Input : constant Ada.Streams.Stream_Element_Array
                          := To_SEA (S);
                        Processed : constant String
                          := Decompress (Dict, Input);
                        Roundtrip : constant Ada.Streams.Stream_Element_Array
                          := Compress (Dict, Processed);
                     begin
                        if Input /= Roundtrip then
                           Sx_Output.Open_List;
                           Sx_Output.Append_String
                             ("decompress-roundtrip-failed");
                           Sx_Output.Append_Atom (Input);
                           Sx_Output.Append_String (Processed);
                           Sx_Output.Append_Atom (Roundtrip);
                           Sx_Output.Close_List;
                        end if;
                     end;
                  end loop;
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
                             := Decompress (Dict, To_SEA (S))'Length;
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
                     Sx_Output.Append_Atom (Compress (Dict, S));
                  end loop;
                  Sx_Output.Close_List;
               end if;

               if Handler.Check_Roundtrip then
                  for S of Data_List loop
                     declare
                        Processed : constant Ada.Streams.Stream_Element_Array
                          := Compress (Dict, S);
                        Roundtrip : constant String
                          := Decompress (Dict, Processed);
                     begin
                        if S /= Roundtrip then
                           Sx_Output.Open_List;
                           Sx_Output.Append_String
                             ("compress-roundtrip-failed");
                           Sx_Output.Append_String (S);
                           Sx_Output.Append_Atom (Processed);
                           Sx_Output.Append_String (Roundtrip);
                           Sx_Output.Close_List;
                        end if;
                     end;
                  end loop;
               end if;

               if Handler.Stat_Output then
                  declare
                     procedure Print_Line (Original, Output, Base64 : Natural);

                     procedure Print_Line
                       (Original, Output, Base64 : in Natural) is
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
                             := Compress (Dict, S)'Length;
                           Base64_Size : constant Natural
                             := ((Output_Size + 2) / 3) * 4;
                        begin
                           Print_Line
                             (Original_Size, Output_Size, Base64_Size);
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
                  Counts : Dictionary_Counts;
               begin
                  Evaluate_Dictionary (Handler.Job_Count,
                     Dict, Data_List, Total_Size, Counts);

                  if Handler.Sx_Output then
                     Sx_Output.Open_List;
                     Sx_Output.Append_String (Ada.Strings.Fixed.Trim
                       (Ada.Streams.Stream_Element_Count'Image (Total_Size),
                        Ada.Strings.Both));

                     for E in Dictionary_Entry'First .. Last_Code (Dict) loop
                        Sx_Output.Open_List;
                        Sx_Output.Append_Atom (Image (Dict, E));
                        Sx_Output.Append_String (Dict_Entry (Dict, E));
                        Sx_Output.Append_String (Ada.Strings.Fixed.Trim
                          (String_Count'Image (Counts (E)),
                           Ada.Strings.Both));
                        Sx_Output.Close_List;
                     end loop;
                     Sx_Output.Close_List;
                  end if;

                  if Handler.Stat_Output then
                     declare
                        procedure Print
                          (Label : in String;
                           E : in Dictionary_Entry;
                           Score : in Score_Value);

                        procedure Print_Min_Max
                          (Label : in String;
                           Score : not null access function
                             (D : in Dictionary;
                              C : in Dictionary_Counts;
                              E : in Dictionary_Entry)
                             return Score_Value);

                        procedure Print_Value
                          (Label : in String;
                           Score : not null access function
                             (D : in Dictionary;
                              C : in Dictionary_Counts;
                              E : in Dictionary_Entry)
                             return Score_Value;
                           Ref : in Score_Value);


                        procedure Print
                          (Label : in String;
                           E : in Dictionary_Entry;
                           Score : in Score_Value) is
                        begin
                           if Handler.Sx_Output then
                              Sx_Output.Open_List;
                              Sx_Output.Append_Atom (Image (Dict, E));
                              Sx_Output.Append_String (Dict_Entry (Dict, E));
                              Sx_Output.Append_String (Ada.Strings.Fixed.Trim
                                (Score'Img, Ada.Strings.Both));
                              Sx_Output.Close_List;
                           else
                              Ada.Text_IO.Put_Line
                                (Label
                                 & Ada.Characters.Latin_1.HT
                                 & Dictionary_Entry'Image (E)
                                 & Ada.Characters.Latin_1.HT
                                 & Natools.String_Escapes.C_Escape_Hex
                                   (Dict_Entry (Dict, E), True)
                                 & Ada.Characters.Latin_1.HT
                                 & Score'Img);
                           end if;
                        end Print;

                        procedure Print_Min_Max
                          (Label : in String;
                           Score : not null access function
                             (D : in Dictionary;
                              C : in Dictionary_Counts;
                              E : in Dictionary_Entry)
                             return Score_Value)
                        is
                           Min_Score, Max_Score : Score_Value
                             := Score (Dict, Counts, Dictionary_Entry'First);
                           S : Score_Value;
                        begin
                           for E in Dictionary_Entry'Succ
                                      (Dictionary_Entry'First)
                                 .. Last_Code (Dict)
                           loop
                              S := Score (Dict, Counts, E);
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
                             (D : in Dictionary;
                              C : in Dictionary_Counts;
                              E : in Dictionary_Entry)
                             return Score_Value;
                           Ref : in Score_Value) is
                        begin
                           if Handler.Sx_Output then
                              Sx_Output.Open_List;
                              Sx_Output.Append_String (Label);
                           end if;

                           for E in Dictionary_Entry'First .. Last_Code (Dict)
                           loop
                              if Score (Dict, Counts, E) = Ref then
                                 Print (Label, E, Ref);
                              end if;
                           end loop;

                           if Handler.Sx_Output then
                              Sx_Output.Close_List;
                           end if;
                        end Print_Value;
                     begin
                        Print_Min_Max ("encoded", Score_Encoded);
                        Print_Min_Max ("frequency", Score_Frequency);
                        Print_Min_Max ("gain", Score_Gain);
                     end;
                  end if;
               end;
         end case;
      end Process;


      function To_Dictionary
        (Handler : in Callback'Class;
         Input : in String_Lists.List;
         Data_List : in String_Lists.List;
         Method : in Methods)
        return Dictionary is
      begin
         case Handler.Dict_Source is
            when Dict_Sources.S_Expression =>
               return Adjust_Dictionary
                 (Handler,
                  To_Dictionary (Input, Handler.Vlen_Verbatim),
                  Data_List,
                  Method);

            when Dict_Sources.Text_List =>
               declare
                  Needed : constant Integer
                    := Handler.Max_Dict_Size
                     - Natural (Handler.Forced_Words.Length);
                  Selected, Pending : String_Lists.List;
                  First : Dictionary_Entry := Dictionary_Entry'First;
               begin
                  if Needed <= 0 then
                     for Word of reverse Handler.Forced_Words loop
                        Selected.Prepend (Word);
                        exit when Positive (Selected.Length)
                          = Handler.Max_Dict_Size;
                     end loop;
                     return To_Dictionary (Selected, Handler.Vlen_Verbatim);
                  end if;

                  Simple_Dictionary_And_Pending
                    (Make_Word_Counter (Handler, Input),
                     Needed,
                     Selected,
                     Pending,
                     Method,
                     Handler.Max_Pending);

                  for Word of reverse Handler.Forced_Words loop
                     Selected.Prepend (Word);
                     First := Dictionary_Entry'Succ (First);
                  end loop;

                  return Optimize_Dictionary
                    (To_Dictionary (Selected, Handler.Vlen_Verbatim),
                     First,
                     Pending,
                     Input,
                     Handler.Job_Count,
                     Method,
                     Handler.Min_Dict_Size,
                     Handler.Max_Dict_Size);
               end;

            when Dict_Sources.Unoptimized_Text_List =>
               declare
                  Needed : constant Integer
                    := Handler.Max_Dict_Size
                     - Natural (Handler.Forced_Words.Length);
                  All_Words : String_Lists.List;
               begin
                  if Needed > 0 then
                     All_Words := Simple_Dictionary
                       (Make_Word_Counter (Handler, Input), Needed, Method);

                     for Word of reverse Handler.Forced_Words loop
                        All_Words.Prepend (Word);
                     end loop;
                  else
                     for Word of reverse Handler.Forced_Words loop
                        All_Words.Prepend (Word);
                        exit when Positive (All_Words.Length)
                          >= Handler.Max_Dict_Size;
                     end loop;
                  end if;

                  return To_Dictionary (All_Words, Handler.Vlen_Verbatim);
               end;
         end case;
      end To_Dictionary;

   end Dictionary_Subprograms;



   package Dict_256 is new Dictionary_Subprograms
     (Dictionary => Natools.Smaz_256.Dictionary,
      Dictionary_Entry => Ada.Streams.Stream_Element,
      Methods => Natools.Smaz_Tools.Methods.Enum,
      Score_Value => Natools.Smaz_Tools.Score_Value,
      String_Count => Natools.Smaz_Tools.String_Count,
      Word_Counter => Natools.Smaz_Tools.Word_Counter,
      Dictionary_Counts => Tools_256.Dictionary_Counts,
      String_Lists => Natools.Smaz_Tools.String_Lists,
      Add_Substrings => Natools.Smaz_Tools.Add_Substrings,
      Add_Words => Natools.Smaz_Tools.Add_Words,
      Append_String => Tools_256.Append_String,
      Build_Perfect_Hash => Natools.Smaz_Tools.GNAT.Build_Perfect_Hash,
      Compress => Natools.Smaz_256.Compress,
      Decompress => Natools.Smaz_256.Decompress,
      Dict_Entry => Natools.Smaz_256.Dict_Entry,
      Evaluate_Dictionary => Tools_256.Evaluate_Dictionary,
      Evaluate_Dictionary_Partial => Tools_256.Evaluate_Dictionary_Partial,
      Filter_By_Count => Natools.Smaz_Tools.Filter_By_Count,
      Last_Code => Last_Code,
      Remove_Element => Tools_256.Remove_Element,
      Replace_Element => Tools_256.Replace_Element,
      Score_Encoded => Tools_256.Score_Encoded'Access,
      Score_Frequency => Tools_256.Score_Frequency'Access,
      Score_Gain => Tools_256.Score_Gain'Access,
      Simple_Dictionary => Natools.Smaz_Tools.Simple_Dictionary,
      Simple_Dictionary_And_Pending
        => Natools.Smaz_Tools.Simple_Dictionary_And_Pending,
      To_Dictionary => Tools_256.To_Dictionary,
      Worst_Element => Tools_256.Worst_Index);

   package Dict_4096 is new Dictionary_Subprograms
     (Dictionary => Natools.Smaz_4096.Dictionary,
      Dictionary_Entry
        => Natools.Smaz_Implementations.Base_4096.Base_4096_Digit,
      Methods => Natools.Smaz_Tools.Methods.Enum,
      Score_Value => Natools.Smaz_Tools.Score_Value,
      String_Count => Natools.Smaz_Tools.String_Count,
      Word_Counter => Natools.Smaz_Tools.Word_Counter,
      Dictionary_Counts => Tools_4096.Dictionary_Counts,
      String_Lists => Natools.Smaz_Tools.String_Lists,
      Add_Substrings => Natools.Smaz_Tools.Add_Substrings,
      Add_Words => Natools.Smaz_Tools.Add_Words,
      Append_String => Tools_4096.Append_String,
      Build_Perfect_Hash => Natools.Smaz_Tools.GNAT.Build_Perfect_Hash,
      Compress => Natools.Smaz_4096.Compress,
      Decompress => Natools.Smaz_4096.Decompress,
      Dict_Entry => Natools.Smaz_4096.Dict_Entry,
      Evaluate_Dictionary => Tools_4096.Evaluate_Dictionary,
      Evaluate_Dictionary_Partial => Tools_4096.Evaluate_Dictionary_Partial,
      Filter_By_Count => Natools.Smaz_Tools.Filter_By_Count,
      Last_Code => Last_Code,
      Remove_Element => Tools_4096.Remove_Element,
      Replace_Element => Tools_4096.Replace_Element,
      Score_Encoded => Tools_4096.Score_Encoded'Access,
      Score_Frequency => Tools_4096.Score_Frequency'Access,
      Score_Gain => Tools_4096.Score_Gain'Access,
      Simple_Dictionary => Natools.Smaz_Tools.Simple_Dictionary,
      Simple_Dictionary_And_Pending
        => Natools.Smaz_Tools.Simple_Dictionary_And_Pending,
      To_Dictionary => Tools_4096.To_Dictionary,
      Worst_Element => Tools_4096.Worst_Index);

   package Dict_64 is new Dictionary_Subprograms
     (Dictionary => Natools.Smaz_64.Dictionary,
      Dictionary_Entry
        => Natools.Smaz_Implementations.Base_64_Tools.Base_64_Digit,
      Methods => Natools.Smaz_Tools.Methods.Enum,
      Score_Value => Natools.Smaz_Tools.Score_Value,
      String_Count => Natools.Smaz_Tools.String_Count,
      Word_Counter => Natools.Smaz_Tools.Word_Counter,
      Dictionary_Counts => Tools_64.Dictionary_Counts,
      String_Lists => Natools.Smaz_Tools.String_Lists,
      Add_Substrings => Natools.Smaz_Tools.Add_Substrings,
      Add_Words => Natools.Smaz_Tools.Add_Words,
      Append_String => Tools_64.Append_String,
      Build_Perfect_Hash => Natools.Smaz_Tools.GNAT.Build_Perfect_Hash,
      Compress => Natools.Smaz_64.Compress,
      Decompress => Natools.Smaz_64.Decompress,
      Dict_Entry => Natools.Smaz_64.Dict_Entry,
      Evaluate_Dictionary => Tools_64.Evaluate_Dictionary,
      Evaluate_Dictionary_Partial => Tools_64.Evaluate_Dictionary_Partial,
      Filter_By_Count => Natools.Smaz_Tools.Filter_By_Count,
      Last_Code => Last_Code,
      Remove_Element => Tools_64.Remove_Element,
      Replace_Element => Tools_64.Replace_Element,
      Score_Encoded => Tools_64.Score_Encoded'Access,
      Score_Frequency => Tools_64.Score_Frequency'Access,
      Score_Gain => Tools_64.Score_Gain'Access,
      Simple_Dictionary => Natools.Smaz_Tools.Simple_Dictionary,
      Simple_Dictionary_And_Pending
        => Natools.Smaz_Tools.Simple_Dictionary_And_Pending,
      To_Dictionary => Tools_64.To_Dictionary,
      Worst_Element => Tools_64.Worst_Index);

   package Dict_Retired is new Dictionary_Subprograms
     (Dictionary => Natools.Smaz.Dictionary,
      Dictionary_Entry => Ada.Streams.Stream_Element,
      Methods => Natools.Smaz.Tools.Methods.Enum,
      Score_Value => Natools.Smaz.Tools.Score_Value,
      String_Count => Natools.Smaz.Tools.String_Count,
      Word_Counter => Natools.Smaz.Tools.Word_Counter,
      Dictionary_Counts => Natools.Smaz.Tools.Dictionary_Counts,
      String_Lists => Natools.Smaz.Tools.String_Lists,
      Add_Substrings => Natools.Smaz.Tools.Add_Substrings,
      Add_Words => Natools.Smaz.Tools.Add_Words,
      Append_String => Natools.Smaz.Tools.Append_String,
      Build_Perfect_Hash => Build_Perfect_Hash,
      Compress => Natools.Smaz.Compress,
      Decompress => Natools.Smaz.Decompress,
      Dict_Entry => Natools.Smaz.Dict_Entry,
      Evaluate_Dictionary => Natools.Smaz.Tools.Evaluate_Dictionary,
      Evaluate_Dictionary_Partial
        => Natools.Smaz.Tools.Evaluate_Dictionary_Partial,
      Filter_By_Count => Natools.Smaz.Tools.Filter_By_Count,
      Last_Code => Last_Code,
      Remove_Element => Natools.Smaz.Tools.Remove_Element,
      Replace_Element => Natools.Smaz.Tools.Replace_Element,
      Score_Encoded => Natools.Smaz.Tools.Score_Encoded'Access,
      Score_Frequency => Natools.Smaz.Tools.Score_Frequency'Access,
      Score_Gain => Natools.Smaz.Tools.Score_Gain'Access,
      Simple_Dictionary => Natools.Smaz.Tools.Simple_Dictionary,
      Simple_Dictionary_And_Pending
        => Natools.Smaz.Tools.Simple_Dictionary_And_Pending,
      To_Dictionary => Natools.Smaz.Tools.To_Dictionary,
      Worst_Element => Natools.Smaz.Tools.Worst_Index);



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
            Handler.Min_Dict_Size := Positive'Value (Argument);
            Handler.Max_Dict_Size := Positive'Value (Argument);

         when Options.Vlen_Verbatim =>
            Handler.Vlen_Verbatim := True;

         when Options.No_Vlen_Verbatim =>
            Handler.Vlen_Verbatim := False;

         when Options.Base_256 =>
            Handler.Algorithm := Algorithms.Base_256;

         when Options.Base_256_Retired =>
            Handler.Algorithm := Algorithms.Base_256_Retired;

         when Options.Base_64 =>
            Handler.Algorithm := Algorithms.Base_64;

         when Options.Base_4096 =>
            Handler.Algorithm := Algorithms.Base_4096;

         when Options.Check_Roundtrip =>
            Handler.Check_Roundtrip := True;

         when Options.Force_Word =>
            if Argument'Length > 0 then
               Handler.Need_Dictionary := True;
               Handler.Forced_Words.Append (Argument);

               if Handler.Action in Actions.Nothing then
                  Handler.Action := Actions.Adjust_Dictionary;
               end if;
            end if;

         when Options.Max_Dict_Size =>
            Handler.Max_Dict_Size := Positive'Value (Argument);

         when Options.Min_Dict_Size =>
            Handler.Min_Dict_Size := Positive'Value (Argument);
      end case;
   end Option;


   function Activate_Dictionary (Dict : in Natools.Smaz_256.Dictionary)
     return Natools.Smaz_256.Dictionary
   is
      Result : Natools.Smaz_256.Dictionary := Dict;
   begin
      Natools.Smaz_Tools.Set_Dictionary_For_Trie_Search
        (Tools_256.To_String_List (Result));
      Result.Hash := Natools.Smaz_Tools.Trie_Search'Access;

      pragma Assert (Natools.Smaz_256.Is_Valid (Result));

      return Result;
   end Activate_Dictionary;


   function Activate_Dictionary (Dict : in Natools.Smaz_4096.Dictionary)
     return Natools.Smaz_4096.Dictionary
   is
      Result : Natools.Smaz_4096.Dictionary := Dict;
   begin
      Natools.Smaz_Tools.Set_Dictionary_For_Trie_Search
        (Tools_4096.To_String_List (Result));
      Result.Hash := Natools.Smaz_Tools.Trie_Search'Access;

      pragma Assert (Natools.Smaz_4096.Is_Valid (Result));

      return Result;
   end Activate_Dictionary;


   function Activate_Dictionary (Dict : in Natools.Smaz_64.Dictionary)
     return Natools.Smaz_64.Dictionary
   is
      Result : Natools.Smaz_64.Dictionary := Dict;
   begin
      Natools.Smaz_Tools.Set_Dictionary_For_Trie_Search
        (Tools_64.To_String_List (Result));
      Result.Hash := Natools.Smaz_Tools.Trie_Search'Access;

      pragma Assert (Natools.Smaz_64.Is_Valid (Result));

      return Result;
   end Activate_Dictionary;


   function Activate_Dictionary (Dict : in Natools.Smaz.Dictionary)
     return Natools.Smaz.Dictionary
   is
      Result : Natools.Smaz.Dictionary := Dict;
   begin
      Natools.Smaz.Tools.Set_Dictionary_For_Trie_Search (Result);
      Result.Hash := Natools.Smaz.Tools.Trie_Search'Access;

      for I in Result.Offsets'Range loop
         if Natools.Smaz.Tools.Trie_Search (Natools.Smaz.Dict_Entry
           (Result, I)) /= Natural (I)
         then
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Current_Error,
               "Fail at" & Ada.Streams.Stream_Element'Image (I)
               & " -> " & Natools.String_Escapes.C_Escape_Hex
                  (Natools.Smaz.Dict_Entry (Result, I), True)
               & " ->" & Natural'Image (Natools.Smaz.Tools.Trie_Search
                  (Natools.Smaz.Dict_Entry (Result, I))));
         end if;
      end loop;

      return Result;
   end Activate_Dictionary;


   procedure Build_Perfect_Hash
     (Word_List : in Natools.Smaz.Tools.String_Lists.List;
      Package_Name : in String)
   is
      Other_Word_List : Natools.Smaz_Tools.String_Lists.List;
   begin
      for S of Word_List loop
         Natools.Smaz_Tools.String_Lists.Append (Other_Word_List, S);
      end loop;

      Natools.Smaz_Tools.GNAT.Build_Perfect_Hash
        (Other_Word_List, Package_Name);
   end Build_Perfect_Hash;


   procedure Convert
     (Input : in Natools.Smaz_Tools.String_Lists.List;
      Output : out Natools.Smaz.Tools.String_Lists.List) is
   begin
      Natools.Smaz.Tools.String_Lists.Clear (Output);

      for S of Input loop
         Natools.Smaz.Tools.String_Lists.Append (Output, S);
      end loop;
   end Convert;


   function Getopt_Config return Getopt.Configuration is
      use Getopt;
      use Options;
      R : Getopt.Configuration;
   begin
      R.Add_Option ("base-256",      '2', No_Argument,       Base_256);
      R.Add_Option ("base-4096",     '4', No_Argument,       Base_4096);
      R.Add_Option ("base-64",       '6', No_Argument,       Base_64);
      R.Add_Option ("ada-dict",      'A', Optional_Argument, Output_Ada_Dict);
      R.Add_Option ("check",         'C', No_Argument,       Check_Roundtrip);
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
      R.Add_Option ("retired",       'R', No_Argument,       Base_256_Retired);
      R.Add_Option ("stats",         's', No_Argument,       Stat_Output);
      R.Add_Option ("no-stats",      'S', No_Argument,       No_Stat_Output);
      R.Add_Option ("text-list",     't', No_Argument,       Text_List_Input);
      R.Add_Option ("fast-text-list", 'T', No_Argument,       Fast_Text_Input);
      R.Add_Option ("max-word-len",  'W', Required_Argument, Max_Word_Size);
      R.Add_Option ("s-expr",        'x', No_Argument,       Sx_Output);
      R.Add_Option ("no-s-expr",     'X', No_Argument,       No_Sx_Output);
      R.Add_Option ("force-word",         Required_Argument, Force_Word);
      R.Add_Option ("max-dict-size",      Required_Argument, Max_Dict_Size);
      R.Add_Option ("min-dict-size",      Required_Argument, Min_Dict_Size);
      R.Add_Option ("no-vlen-verbatim",   No_Argument,       No_Vlen_Verbatim);
      R.Add_Option ("score-method",       Required_Argument, Score_Method);
      R.Add_Option ("vlen-verbatim",      No_Argument,       Vlen_Verbatim);

      return R;
   end Getopt_Config;


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


   procedure Print_Dictionary
     (Output : in Ada.Text_IO.File_Type;
      Dictionary : in Natools.Smaz_4096.Dictionary;
      Hash_Package_Name : in String := "")
   is
      procedure Put_Line (Line : in String);

      procedure Put_Line (Line : in String) is
      begin
         Ada.Text_IO.Put_Line (Output, Line);
      end Put_Line;

      procedure Print_Dictionary_In_Ada is
        new Tools_4096.Print_Dictionary_In_Ada (Put_Line);
   begin
      if Hash_Package_Name'Length > 0 then
         Print_Dictionary_In_Ada
           (Dictionary,
            Hash_Image => Hash_Package_Name & ".Hash'Access");
      else
         Print_Dictionary_In_Ada (Dictionary);
      end if;
   end Print_Dictionary;


   procedure Print_Dictionary
     (Output : in Ada.Text_IO.File_Type;
      Dictionary : in Natools.Smaz_64.Dictionary;
      Hash_Package_Name : in String := "")
   is
      procedure Put_Line (Line : in String);

      procedure Put_Line (Line : in String) is
      begin
         Ada.Text_IO.Put_Line (Output, Line);
      end Put_Line;

      procedure Print_Dictionary_In_Ada is
        new Tools_64.Print_Dictionary_In_Ada (Put_Line);
   begin
      if Hash_Package_Name'Length > 0 then
         Print_Dictionary_In_Ada
           (Dictionary,
            Hash_Image => Hash_Package_Name & ".Hash'Access");
      else
         Print_Dictionary_In_Ada (Dictionary);
      end if;
   end Print_Dictionary;


   procedure Print_Dictionary
     (Output : in Ada.Text_IO.File_Type;
      Dictionary : in Natools.Smaz.Dictionary;
      Hash_Package_Name : in String := "")
   is
      procedure Put_Line (Line : in String);

      procedure Put_Line (Line : in String) is
      begin
         Ada.Text_IO.Put_Line (Output, Line);
      end Put_Line;

      procedure Print_Dictionary_In_Ada is
        new Natools.Smaz.Tools.Print_Dictionary_In_Ada (Put_Line);
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

            when Options.Base_256 =>
               New_Line (Output);
               Put_Line (Output, Indent & Indent
                 & "Use base-256 implementation (default)");

            when Options.Base_256_Retired =>
               New_Line (Output);
               Put_Line (Output, Indent & Indent
                 & "Use retired base-256 implementation");

            when Options.Base_64 =>
               New_Line (Output);
               Put_Line (Output, Indent & Indent
                 & "Use base-64 implementation");

            when Options.Base_4096 =>
               New_Line (Output);
               Put_Line (Output, Indent & Indent
                 & "Use base-4096 implementation");

            when Options.Check_Roundtrip =>
               New_Line (Output);
               Put_Line (Output, Indent & Indent
                 & "Check roundtrip of compression or decompression");

            when Options.Force_Word =>
               Put_Line (Output, " <word>");
               Put_Line (Output, Indent & Indent
                 & "Force <word> into the dictionary,"
                 & " replacing the worst entry");
               Put_Line (Output, Indent & Indent
                 & "Can be specified multiple times to force many words.");

            when Options.Max_Dict_Size =>
               Put_Line (Output, " <count>");
               Put_Line (Output, Indent & Indent
                 & "Maximum number of words in the dictionary to build");

            when Options.Min_Dict_Size =>
               Put_Line (Output, " <count>");
               Put_Line (Output, Indent & Indent
                 & "Minimum number of words in the dictionary to build");
         end case;
      end loop;
   end Print_Help;


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

   if not (Handler.Stat_Output or Handler.Sx_Output or Handler.Check_Roundtrip)
   then
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

   case Handler.Algorithm is
      when Algorithms.Base_256 =>
         Dict_256.Process
           (Handler, Input_List, Input_Data, Handler.Score_Method);
      when Algorithms.Base_64 =>
         Dict_64.Process
           (Handler, Input_List, Input_Data, Handler.Score_Method);
      when Algorithms.Base_4096 =>
         Dict_4096.Process
           (Handler, Input_List, Input_Data, Handler.Score_Method);
      when Algorithms.Base_256_Retired =>
         declare
            Converted_Input_List : Natools.Smaz.Tools.String_Lists.List;
            Converted_Input_Data : Natools.Smaz.Tools.String_Lists.List;
         begin
            Convert (Input_List, Converted_Input_List);
            Convert (Input_Data, Converted_Input_Data);
            Dict_Retired.Process
              (Handler, Converted_Input_List, Converted_Input_Data,
               Natools.Smaz.Tools.Methods.Enum'Val
                 (Natools.Smaz_Tools.Methods.Enum'Pos (Handler.Score_Method)));
         end;
   end case;

end Smaz;
