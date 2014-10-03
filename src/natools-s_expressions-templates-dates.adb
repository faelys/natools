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

with Natools.S_Expressions.Interpreter_Loop;
with Natools.S_Expressions.Templates.Generic_Discrete_Render;
with Natools.S_Expressions.Templates.Integers;
with Natools.Static_Maps.S_Expressions.Templates.Dates;
with Natools.Time_IO.RFC_3339;

package body Natools.S_Expressions.Templates.Dates is

   package Commands renames Natools.Static_Maps.S_Expressions.Templates.Dates;

   procedure Render_Day_Of_Week
     is new Natools.S_Expressions.Templates.Generic_Discrete_Render
     (Ada.Calendar.Formatting.Day_Name,
      Ada.Calendar.Formatting.Day_Name'Image,
      Ada.Calendar.Formatting."=");

   procedure Append
     (Output : in out Ada.Streams.Root_Stream_Type'Class;
      Value : in Split_Time;
      Data : in Atom);

   procedure Execute
     (Output : in out Ada.Streams.Root_Stream_Type'Class;
      Value : in Split_Time;
      Name : in Atom;
      Arguments : in out Lockable.Descriptor'Class);

   function Two_Digit_Image (Value : Integer) return Atom
     is ((1 => Character'Pos ('0') + Octet (Value / 10),
          2 => Character'Pos ('0') + Octet (Value mod 10)))
     with Pre => Value in 0 .. 99;

   function Four_Digit_Image (Value : Integer) return Atom
     is ((1 => Character'Pos ('0') + Octet (Value / 1000),
          2 => Character'Pos ('0') + Octet ((Value / 100) mod 10),
          3 => Character'Pos ('0') + Octet ((Value / 10) mod 10),
          4 => Character'Pos ('0') + Octet (Value mod 10)))
     with Pre => Value in 0 .. 9999;

   procedure Render_Triplet
     (Output : in out Ada.Streams.Root_Stream_Type'Class;
      Part_1, Part_2, Part_3 : in Atom;
      Template : in out Lockable.Descriptor'Class);


   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

   procedure Render_Triplet
     (Output : in out Ada.Streams.Root_Stream_Type'Class;
      Part_1, Part_2, Part_3 : in Atom;
      Template : in out Lockable.Descriptor'Class) is
   begin
      Output.Write (Part_1);

      if Template.Current_Event = Events.Add_Atom then
         declare
            Separator : constant Atom := Template.Current_Atom;
            Event : Events.Event;
         begin
            Template.Next (Event);

            Output.Write (Separator);
            Output.Write (Part_2);

            if Event = Events.Add_Atom then
               Output.Write (Template.Current_Atom);
            else
               Output.Write (Separator);
            end if;
         end;
      else
         Output.Write (Part_2);
      end if;

      Output.Write (Part_3);
   end Render_Triplet;



   ----------------------------
   -- Interpreter Components --
   ----------------------------

   procedure Append
     (Output : in out Ada.Streams.Root_Stream_Type'Class;
      Value : in Split_Time;
      Data : in Atom)
   is
      pragma Unreferenced (Value);
   begin
      Output.Write (Data);
   end Append;


   procedure Execute
     (Output : in out Ada.Streams.Root_Stream_Type'Class;
      Value : in Split_Time;
      Name : in Atom;
      Arguments : in out Lockable.Descriptor'Class)
   is
      Format : Integers.Format;
   begin
      case Commands.Main (To_String (Name)) is
         when Commands.Error =>
            null;

         when Commands.Big_Endian_Date =>
            Render_Triplet
              (Output,
               Four_Digit_Image (Value.Year),
               Two_Digit_Image (Value.Month),
               Two_Digit_Image (Value.Day),
               Arguments);

         when Commands.Big_Endian_Time =>
            Render_Triplet
              (Output,
               Two_Digit_Image (Value.Hour),
               Two_Digit_Image (Value.Minute),
               Two_Digit_Image (Value.Second),
               Arguments);

         when Commands.Day =>
            Format.Set_Image (0, Null_Atom);
            Integers.Render (Output, Format, Arguments, Value.Day);

         when Commands.Day_Of_Week =>
            Render_Day_Of_Week (Output, Arguments, Value.Day_Of_Week);

         when Commands.Hour =>
            Format.Set_Image (-1, Null_Atom);
            Integers.Render (Output, Format, Arguments, Value.Hour);

         when Commands.Little_Endian_Date =>
            Render_Triplet
              (Output,
               Two_Digit_Image (Value.Day),
               Two_Digit_Image (Value.Month),
               Four_Digit_Image (Value.Year),
               Arguments);

         when Commands.Little_Endian_Time =>
            Render_Triplet
              (Output,
               Two_Digit_Image (Value.Second),
               Two_Digit_Image (Value.Minute),
               Two_Digit_Image (Value.Hour),
               Arguments);

         when Commands.Minute =>
            Format.Set_Image (-1, Null_Atom);
            Integers.Render (Output, Format, Arguments, Value.Minute);

         when Commands.Month =>
            Format.Set_Image (0, Null_Atom);
            Integers.Render (Output, Format, Arguments, Value.Month);

         when Commands.Padded_Day =>
            Format.Set_Image (0, Null_Atom);
            Format.Set_Minimum_Width (2);
            Format.Set_Left_Padding ((1 => Character'Pos ('0')));
            Format.Set_Align (Integers.Right_Aligned);
            Integers.Render (Output, Format, Arguments, Value.Day);

         when Commands.Padded_Hour =>
            Format.Set_Image (-1, Null_Atom);
            Format.Set_Minimum_Width (2);
            Format.Set_Left_Padding ((1 => Character'Pos ('0')));
            Format.Set_Align (Integers.Right_Aligned);
            Integers.Render (Output, Format, Arguments, Value.Hour);

         when Commands.Padded_Minute =>
            Format.Set_Image (-1, Null_Atom);
            Format.Set_Minimum_Width (2);
            Format.Set_Left_Padding ((1 => Character'Pos ('0')));
            Format.Set_Align (Integers.Right_Aligned);
            Integers.Render (Output, Format, Arguments, Value.Minute);

         when Commands.Padded_Month =>
            Format.Set_Image (0, Null_Atom);
            Format.Set_Minimum_Width (2);
            Format.Set_Left_Padding ((1 => Character'Pos ('0')));
            Format.Set_Align (Integers.Right_Aligned);
            Integers.Render (Output, Format, Arguments, Value.Month);

         when Commands.Padded_Second =>
            Format.Set_Image (-1, Null_Atom);
            Format.Set_Minimum_Width (2);
            Format.Set_Left_Padding ((1 => Character'Pos ('0')));
            Format.Set_Align (Integers.Right_Aligned);
            Integers.Render (Output, Format, Arguments, Value.Second);

         when Commands.RFC_3339 =>
            Output.Write (To_Atom
              (Time_IO.RFC_3339.Image (Value.Source, Value.Time_Zone)));

         when Commands.Second =>
            Format.Set_Image (-1, Null_Atom);
            Integers.Render (Output, Format, Arguments, Value.Second);

         when Commands.Year =>
            Integers.Render (Output, Arguments, Value.Year);
      end case;
   end Execute;


   procedure Interpreter is new Interpreter_Loop
     (Ada.Streams.Root_Stream_Type'Class, Split_Time, Execute, Append);



   ----------------------
   -- Public Interface --
   ----------------------

   function Split
     (Value : Ada.Calendar.Time;
      Time_Zone : Ada.Calendar.Time_Zones.Time_Offset)
     return Split_Time
   is
      Year : Ada.Calendar.Year_Number;
      Month : Ada.Calendar.Month_Number;
      Day : Ada.Calendar.Day_Number;
      Hour : Ada.Calendar.Formatting.Hour_Number;
      Minute : Ada.Calendar.Formatting.Minute_Number;
      Second : Ada.Calendar.Formatting.Second_Number;
      Sub_Second : Ada.Calendar.Formatting.Second_Duration;
   begin
      Ada.Calendar.Formatting.Split
        (Value,
         Year, Month, Day,
         Hour, Minute, Second,
         Sub_Second,
         Time_Zone);

      return Split_Time'
        (Source => Value,
         Time_Zone => Time_Zone,
         Year => Year,
         Month => Month,
         Day => Day,
         Day_Of_Week => Ada.Calendar.Formatting.Day_Of_Week (Value),
         Hour => Hour,
         Minute => Minute,
         Second => Second,
         Sub_Second => Sub_Second);
   end Split;


   procedure Render
     (Output : in out Ada.Streams.Root_Stream_Type'Class;
      Template : in out Lockable.Descriptor'Class;
      Value : in Ada.Calendar.Time) is
   begin
      Render
        (Output,
         Template,
         Value,
         Ada.Calendar.Time_Zones.UTC_Time_Offset (Value));
   end Render;


   procedure Render
     (Output : in out Ada.Streams.Root_Stream_Type'Class;
      Template : in out Lockable.Descriptor'Class;
      Value : in Ada.Calendar.Time;
      Time_Zone : Ada.Calendar.Time_Zones.Time_Offset) is
   begin
      Render (Output, Template, Split (Value, Time_Zone));
   end Render;


   procedure Render
     (Output : in out Ada.Streams.Root_Stream_Type'Class;
      Template : in out Lockable.Descriptor'Class;
      Value : in Split_Time) is
   begin
      Interpreter (Template, Output, Value);
   end Render;

end Natools.S_Expressions.Templates.Dates;

