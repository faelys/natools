------------------------------------------------------------------------------
-- Copyright (c) 2013-2014, Natacha Porté                                   --
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
with Ada.Exceptions;

with Natools.References.Tools;

package body Natools.Reference_Tests is

   package Tools is new Refs.Tools;

   procedure Check_Ref
     (Report : in out NT.Reporter'Class;
      Name : in String;
      Ref : in Refs.Reference;
      Expected_Count : in Natural;
      Continue : in out Boolean);
      --  Check the given reference and report failure if any
      --  Do nothing if Continue is False

   procedure Check_Consistency
     (Report : in out NT.Reporter'Class;
      Name : in String;
      Left, Right : in Refs.Reference;
      Continue : in out Boolean);
      --  Check consistency between two reference and report if failed
      --  Do nothing if Continue is False

   procedure Check_Count
     (Report : in out NT.Reporter'Class;
      Name : in String;
      Expected_Count : in Integer;
      Continue : in out Boolean);
      --  Check instance count and report failure if any
      --  Do nothing if Continue is False


   --------------------
   -- Object counter --
   --------------------

   function Factory return Counter is
   begin
      Instance_Count := Instance_Count + 1;
      return Counter'(Ada.Finalization.Limited_Controlled with
         Instance_Number => Instance_Count);
   end Factory;


   overriding procedure Initialize (Object : in out Counter) is
   begin
      Instance_Count := Instance_Count + 1;
      Object.Instance_Number := Instance_Count;
   end Initialize;


   overriding procedure Finalize (Object : in out Counter) is
      pragma Unreferenced (Object);
   begin
      Instance_Count := Instance_Count - 1;
   end Finalize;



   ------------------------
   -- Helper subprograms --
   ------------------------

   procedure Check_Consistency
     (Report : in out NT.Reporter'Class;
      Name : in String;
      Left, Right : in Refs.Reference;
      Continue : in out Boolean) is
   begin
      if Continue and then not Tools.Is_Consistent (Left, Right) then
         NT.Item (Report, Name, NT.Fail);
         NT.Info (Report, "Inconsistent references found");
         Continue := False;
      end if;
   end Check_Consistency;


   procedure Check_Count
     (Report : in out NT.Reporter'Class;
      Name : in String;
      Expected_Count : in Integer;
      Continue : in out Boolean) is
   begin
      if not Continue then
         return;
      end if;

      if Instance_Count < 0 then
         NT.Item (Report, Name, NT.Fail);
         NT.Info
           (Report,
            "Invalid Instance_Count " & Integer'Image (Instance_Count));
         Continue := False;

      elsif Instance_Count /= Expected_Count then
         NT.Item (Report, Name, NT.Fail);
         NT.Info (Report, "Unexpected Instance_Count"
           & Integer'Image (Instance_Count)
           & " instead of"
           & Integer'Image (Expected_Count));
         Continue := False;
      end if;
   end Check_Count;


   procedure Check_Ref
     (Report : in out NT.Reporter'Class;
      Name : in String;
      Ref : in Refs.Reference;
      Expected_Count : in Natural;
      Continue : in out Boolean)
   is
      Actual_Count : constant Integer := Tools.Count (Ref);
   begin
      if not Continue then
         return;
      end if;

      if not Tools.Is_Valid (Ref) then
         NT.Item (Report, Name, NT.Fail);
         NT.Info (Report, "Invalid internal state for reference");
         Continue := False;

      elsif Actual_Count /= Expected_Count then
         NT.Item (Report, Name, NT.Fail);
         NT.Info (Report,
           "Unexpected reference count"
           & Natural'Image (Actual_Count)
           & " instead of"
           & Natural'Image (Expected_Count));
         Continue := False;

      elsif not Ref.Is_Empty and then Ref.Is_Last /= (Actual_Count = 1) then
         NT.Item (Report, Name, NT.Fail);
         NT.Info (Report,
           "Unexpected result of Is_Last ("
           & Boolean'Image (Ref.Is_Last)
           & ") while counter is"
           & Natural'Image (Actual_Count));
         Continue := False;
      end if;
   end Check_Ref;



   --------------------
   -- Invidual tests --
   --------------------

   procedure Test_Data_Access (Report : in out NT.Reporter'Class) is
      Argument_Count : Natural;
      Result : Boolean;

      procedure Check (Self : in Counter);
      procedure Set (Self : in out Counter);

      procedure Check (Self : in Counter) is
      begin
         Result := Argument_Count = Self.Instance_Number;
         Argument_Count := Self.Instance_Number;
      end Check;

      procedure Set (Self : in out Counter) is
      begin
         Self.Instance_Number := Argument_Count;
      end Set;

      Name : constant String := "Data access";
   begin
      declare
         Ref_1 : Refs.Reference := Refs.Create (Factory'Access);
         Ref_2 : Refs.Reference;
      begin
         Ref_2.Replace (Factory'Access);
         Argument_Count := 42;
         Ref_2.Update (Set'Access);
         Ref_1.Update.Data.Instance_Number := 18;

         Argument_Count := 18;
         Ref_1.Query (Check'Access);
         if not Result then
            NT.Item (Report, Name, NT.Fail);
            NT.Info (Report, "Stored 18, retrieved"
              & Integer'Image (Argument_Count));
            return;
         end if;

         Ref_1.Reset;

         Argument_Count := Ref_2.Query.Data.Instance_Number;
         if Argument_Count /= 42 then
            NT.Item (Report, Name, NT.Fail);
            NT.Info (Report, "Stored 42, retrieved"
              & Integer'Image (Argument_Count));
            return;
         end if;
      end;

      NT.Item (Report, Name, NT.Success);
   exception
      when Error : others => NT.Report_Exception (Report, Name, Error);
   end Test_Data_Access;


   procedure Test_Double_Finalize (Report : in out NT.Reporter'Class) is
      Name : constant String := "Double finalize";
      Initial_Count : constant Integer := Instance_Count;
      Continue : Boolean := True;
   begin
      declare
         Ref : Refs.Reference := Refs.Create (Factory'Access);
      begin
         Ref.Finalize;
      end;

      Check_Count (Report, Name, Initial_Count, Continue);

      if Continue then
         NT.Item (Report, Name, NT.Success);
      end if;
   exception
      when Error : others => NT.Report_Exception (Report, Name, Error);
   end Test_Double_Finalize;


   procedure Test_Instance_Counts (Report : in out NT.Reporter'Class) is
      Name : constant String := "Instance counts";
      Initial_Count : constant Integer := Instance_Count;
      Continue : Boolean := True;
   begin
      declare
         procedure Check (Count_0, Count_1, Count_2, Delta_I : in Integer);

         Ref_0 : Refs.Reference := Refs.Create (Factory'Access);
         Ref_1 : Refs.Reference := Refs.Create (Refs.Data_Access'(null));
         Ref_2 : Refs.Reference;

         procedure Check (Count_0, Count_1, Count_2, Delta_I : in Integer) is
         begin
            Check_Ref (Report, Name, Ref_0, Count_0, Continue);
            Check_Ref (Report, Name, Ref_1, Count_1, Continue);
            Check_Ref (Report, Name, Ref_2, Count_2, Continue);
            Check_Consistency (Report, Name, Ref_0, Ref_1, Continue);
            Check_Consistency (Report, Name, Ref_1, Ref_2, Continue);
            Check_Consistency (Report, Name, Ref_2, Ref_0, Continue);
            Check_Count (Report, Name, Initial_Count + Delta_I, Continue);
         end Check;
      begin
         Check (1, 0, 0, 1);

         if Continue then
            Ref_1 := Refs.Create (new Counter);
         end if;

         Check (1, 1, 0, 2);

         if Continue then
            Ref_2 := Ref_0;
         end if;

         Check (2, 1, 2, 2);

         if Continue then
            Ref_1 := Ref_0;
         end if;

         Check (3, 3, 3, 1);

         if Continue then
            Ref_2.Replace (new Counter);
         end if;

         Check (2, 2, 1, 2);

         if Continue then
            Ref_1.Reset;
            Ref_0 := Ref_1;
         end if;

         Check (0, 0, 1, 1);
      end;

      Check_Count (Report, Name, Initial_Count, Continue);

      if Continue then
         NT.Item (Report, Name, NT.Success);
      end if;
   exception
      when Error : others => NT.Report_Exception (Report, Name, Error);
   end Test_Instance_Counts;


   procedure Test_Reference_Counts (Report : in out NT.Reporter'Class) is
      Name : constant String := "Reference counts";
      Initial_Count : constant Integer := Instance_Count;
      Continue : Boolean := True;
   begin
      declare
         procedure Check (Count_0, Count_1, Count_2 : in Integer);

         Ref_0 : constant Refs.Reference := Refs.Create (Factory'Access);
         Ref_1 : Refs.Reference := Ref_0;
         Ref_2 : Refs.Reference;

         procedure Check (Count_0, Count_1, Count_2 : in Integer) is
         begin
            Check_Ref (Report, Name, Ref_0, Count_0, Continue);
            Check_Ref (Report, Name, Ref_1, Count_1, Continue);
            Check_Ref (Report, Name, Ref_2, Count_2, Continue);
            Check_Consistency (Report, Name, Ref_0, Ref_1, Continue);
            Check_Consistency (Report, Name, Ref_1, Ref_2, Continue);
            Check_Consistency (Report, Name, Ref_2, Ref_0, Continue);
         end Check;
      begin
         Check (2, 2, 0);

         if Continue then
            Ref_2 := Ref_0;
         end if;

         Check (3, 3, 3);

         if Continue then
            Refs.Reset (Ref_1);
         end if;

         Check (2, 0, 2);
      end;

      Check_Count (Report, Name, Initial_Count, Continue);

      if Continue then
         NT.Item (Report, Name, NT.Success);
      end if;
   exception
      when Error : others => NT.Report_Exception (Report, Name, Error);
   end Test_Reference_Counts;


   procedure Test_Reference_Tests (Report : in out NT.Reporter'Class) is
      Name : constant String := "Reference tests";
      Initial_Count : constant Integer := Instance_Count;
      Continue : Boolean := True;
   begin
      declare
         use type Refs.Reference;

         Ref : Refs.Reference;
         Base : constant Refs.Reference := Refs.Create (Factory'Access);
      begin
         if not Ref.Is_Empty then
            NT.Item (Report, Name, NT.Fail);
            NT.Info (Report, "Default reference is not empty");
            return;
         end if;

         if Base.Is_Empty then
            NT.Item (Report, Name, NT.Fail);
            NT.Info (Report, "Created reference is empty");
            return;
         end if;

         Ref.Replace (Factory'Access);
         if Ref.Is_Empty then
            NT.Item (Report, Name, NT.Fail);
            NT.Info (Report, "Replaced reference is empty");
            return;
         end if;

         if Ref = Base then
            NT.Item (Report, Name, NT.Fail);
            NT.Info (Report, "Unexpected equality between Ref and Base");
            return;
         end if;

         Ref := Base;
         if Ref /= Base then
            NT.Item (Report, Name, NT.Fail);
            NT.Info (Report, "Unexpected inequality between Ref and Base");
            return;
         end if;
      end;

      Check_Count (Report, Name, Initial_Count, Continue);

      if Continue then
         NT.Item (Report, Name, NT.Success);
      end if;
   exception
      when Error : others => NT.Report_Exception (Report, Name, Error);
   end Test_Reference_Tests;


   procedure Test_Task_Safety (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Task safety");
      Success : Boolean := True;

      protected Protected_Report is
         procedure Report_Exception (Ex : Ada.Exceptions.Exception_Occurrence);
      end Protected_Report;

      protected body Protected_Report is
         procedure Report_Exception
           (Ex : Ada.Exceptions.Exception_Occurrence) is
         begin
            Test.Report_Exception (Ex, NT.Fail);
         end Report_Exception;
      end Protected_Report;

      task type Checker is
         entry Start (Count : in Natural; Ref : in Refs.Immutable_Reference);
      end Checker;

      task body Checker is
         Starting_Value, Last : Natural;
         R : Refs.Immutable_Reference;
      begin
         accept Start (Count : in Natural; Ref : in Refs.Immutable_Reference)
         do
            Last := Count;
            R := Ref;
         end Start;
         Starting_Value := R.Query.Data.Instance_Number;
         for I in 1 .. Last loop
            declare
               Temp : constant Refs.Immutable_Reference := R;
            begin
               if Temp.Query.Data.Instance_Number /= Starting_Value then
                  Success := False;
               end if;
            end;
         end loop;
      exception
         when Error : others =>
            Protected_Report.Report_Exception (Error);
      end Checker;

      Start : constant Ada.Calendar.Time := Ada.Calendar.Clock;
   begin
      declare
         Base : constant Refs.Immutable_Reference
           := Refs.Create (Factory'Access);
      begin
         declare
            Checkers : array (1 .. 16) of Checker;
         begin
            for I in Checkers'Range loop
               Checkers (I).Start (10 ** 6, Base);
            end loop;
         end;

         if not Success then
            Test.Fail ("Success somehow got to False");
         end if;
      end;

      Test.Info ("Test run in "
        & Duration'Image (Ada.Calendar."-" (Ada.Calendar.Clock, Start)));
   exception
      when Error : others =>
         Test.Report_Exception (Error);
         Test.Info ("Test run in "
           & Duration'Image (Ada.Calendar."-" (Ada.Calendar.Clock, Start)));
   end Test_Task_Safety;



   ---------------------
   -- Test everything --
   ---------------------

   procedure All_Tests (Report : in out NT.Reporter'Class) is
   begin
      Test_Data_Access (Report);
      Test_Double_Finalize (Report);
      Test_Instance_Counts (Report);
      Test_Reference_Counts (Report);
      Test_Reference_Tests (Report);
   end All_Tests;

end Natools.Reference_Tests;
