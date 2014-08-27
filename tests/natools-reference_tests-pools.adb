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

package body Natools.Reference_Tests.Pools is

   use type Ref_Pools.Pool_Size;


   procedure Check_Counts
     (Test : in out NT.Test;
      Pool : in Ref_Pools.Pool;
      Active, Initialized, Total : in Ref_Pools.Pool_Size);

   procedure Check_Order
     (Test : in out NT.Test;
      Pool : in out Ref_Pools.Pool);


   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

   procedure Check_Counts
     (Test : in out NT.Test;
      Pool : in Ref_Pools.Pool;
      Active, Initialized, Total : in Ref_Pools.Pool_Size)
   is
      S : Ref_Pools.Pool_Size;
   begin
      S := Pool.Active_Size;
      if S /= Active then
         Test.Fail
           ("Pool.Active_Size is"
            & Ref_Pools.Pool_Size'Image (S)
            & ", expected "
            & Ref_Pools.Pool_Size'Image (Active));
      end if;

      S := Pool.Initialized_Size;
      if S /= Initialized then
         Test.Fail
           ("Pool.Initialized_Size is"
            & Ref_Pools.Pool_Size'Image (S)
            & ", expected "
            & Ref_Pools.Pool_Size'Image (Initialized));
      end if;

      S := Pool.Capacity;
      if S /= Total then
         Test.Fail
           ("Pool.Initialized_Size is"
            & Ref_Pools.Pool_Size'Image (S)
            & ", expected "
            & Ref_Pools.Pool_Size'Image (Total));
      end if;
   end Check_Counts;


   procedure Check_Order
     (Test : in out NT.Test;
      Pool : in out Ref_Pools.Pool)
   is
      procedure Process (Ref : in Refs.Reference);

      Rank, Last : Natural := 0;

      procedure Process (Ref : in Refs.Reference) is
      begin
         Rank := Rank + 1;

         if Ref.Is_Empty then
            Test.Fail ("Unexpected empty reference at rank"
              & Natural'Image (Rank));
            return;
         end if;

         declare
            Accessor : constant Refs.Accessor := Ref.Query;
         begin
            if Accessor.Data.Instance_Number = 0 then
               Test.Fail ("Unexpected null instance number at rank"
                 & Natural'Image (Rank));
            elsif Last = 0 then
               Last := Accessor.Data.Instance_Number;
            elsif Accessor.Data.Instance_Number /= Last + 1 then
               Test.Fail ("At rank"
                 & Natural'Image (Rank)
                 & ", reference to instance"
                 & Natural'Image (Accessor.Data.Instance_Number)
                 & " following reference to instance"
                 & Natural'Image (Last));
               Last := 0;
            else
               Last := Accessor.Data.Instance_Number;
            end if;
         end;
      end Process;
   begin
      Pool.Unchecked_Iterate (Process'Access);
   end Check_Order;



   ------------------------
   -- Peudo_Process Task --
   ------------------------

   task body Pseudo_Process is
      Time : Duration;
      Ref : Refs.Reference;
   begin
      select
         accept Start (Target : in Refs.Reference; Amount : in Duration) do
            Time := Amount;
            Ref := Target;
         end Start;
      or terminate;
      end select;

      delay Time;
      Ref.Reset;
   end Pseudo_Process;


   procedure Bounded_Start
     (Process : in out Pseudo_Process;
      Pool : in out Ref_Pools.Pool;
      Amount : in Duration;
      Test : in out NT.Test;
      Expected_Instance : in Natural)
   is
      Ref : Refs.Reference;
   begin
      Pool.Get (Factory'Access, Ref);

      if Ref.Query.Data.Instance_Number /= Expected_Instance then
         Test.Fail ("Got reference to instance"
           & Natural'Image (Ref.Query.Data.Instance_Number)
           & ", expected"
           & Natural'Image (Expected_Instance));
      end if;

      Process.Start (Ref, Amount);
   end Bounded_Start;


   procedure Unbounded_Start
     (Process : in out Pseudo_Process;
      Pool : in out Ref_Pools.Pool;
      Amount : in Duration;
      Test : in out NT.Test;
      Expected_Instance : in Natural)
   is
      Ref : Refs.Reference;
   begin
      Pool.Create (Factory'Access, Ref);

      if Ref.Query.Data.Instance_Number /= Expected_Instance then
         Test.Fail ("Got reference to instance"
           & Natural'Image (Ref.Query.Data.Instance_Number)
           & ", expected"
           & Natural'Image (Expected_Instance));
      end if;

      Process.Start (Ref, Amount);
   end Unbounded_Start;



   -------------------------
   -- Complete Test Suite --
   -------------------------

   procedure All_Tests (Report : in out NT.Reporter'Class) is
   begin
      Bounded_Pool (Report);
      Static_Pool (Report);
      Unbounded_Pool (Report);
   end All_Tests;



   -----------------------
   -- Inidividual Tests --
   -----------------------

   procedure Bounded_Pool (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Bounded pool typical usage");
   begin
      declare
         Test_Length : constant Duration := 0.5;
         Ref_Pool : Ref_Pools.Pool;
         Workers : array (1 .. 4) of Pseudo_Process;
      begin
         --  Timeline (in Test_Length/10):     <-------->
         --  Task using reference 1:           1111111111
         --  Task using reference 2:             2222 44
         --  Task using reference 3:               3333

         Check_Counts (Test, Ref_Pool, 0, 0, 0);
         Ref_Pool.Preallocate (3);
         Check_Counts (Test, Ref_Pool, 0, 0, 3);

         Bounded_Start (Workers (1), Ref_Pool, Test_Length, Test, 1);
         Check_Counts (Test, Ref_Pool, 1, 1, 3);

         delay Test_Length * 0.2;
         Bounded_Start (Workers (2), Ref_Pool, Test_Length * 0.4, Test, 2);
         Check_Counts (Test, Ref_Pool, 2, 2, 3);

         delay Test_Length * 0.2;
         Bounded_Start (Workers (3), Ref_Pool, Test_Length * 0.4, Test, 3);
         Check_Counts (Test, Ref_Pool, 3, 3, 3);

         delay Test_Length * 0.1;
         begin
            Bounded_Start (Workers (4), Ref_Pool, Test_Length * 0.2, Test, 0);
            Test.Fail ("Expected exception after filling bounded pool");
         exception
            when Constraint_Error =>
               null;
            when Error : others =>
               Test.Info ("At Get on full bounded pool,");
               Test.Report_Exception (Error, NT.Fail);
         end;

         delay Test_Length * 0.2;
         Check_Counts (Test, Ref_Pool, 2, 3, 3);
         Bounded_Start (Workers (4), Ref_Pool, Test_Length * 0.2, Test, 2);
         Check_Counts (Test, Ref_Pool, 3, 3, 3);

         Check_Order (Test, Ref_Pool);
      end;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Bounded_Pool;


   procedure Static_Pool (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Static pool typical usage");
   begin
      declare
         Size : constant Ref_Pools.Pool_Size := 10;
         Ref_Pool : Ref_Pools.Pool;
         Ref : array (Ref_Pools.Pool_Size range 1 .. Size) of Refs.Reference;
      begin
         Check_Counts (Test, Ref_Pool, 0, 0, 0);
         Ref_Pool.Preallocate (Size, Factory'Access);
         Check_Counts (Test, Ref_Pool, 0, Size, Size);

         for I in Ref'Range loop
            Ref_Pool.Get (Ref (I));
            Check_Counts (Test, Ref_Pool, I, Size, Size);
         end loop;

         Ref (2).Reset;
         Check_Counts (Test, Ref_Pool, Size - 1, Size, Size);

         Ref_Pool.Get (Ref (2));
         Check_Counts (Test, Ref_Pool, Size, Size, Size);

         declare
            Extra_Ref : Refs.Reference;
         begin
            Ref_Pool.Get (Extra_Ref);
            Test.Fail ("Expected exception at Get on full pool");
         exception
            when Constraint_Error =>
               null;
            when Error : others =>
               Test.Info ("At Get on full pool,");
               Test.Report_Exception (Error, NT.Fail);
         end;

         Check_Order (Test, Ref_Pool);
      end;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Static_Pool;


   procedure Unbounded_Pool (Report : in out NT.Reporter'Class) is
      Test : NT.Test := Report.Item ("Unbounded pool typical usage");
   begin
      declare
         Test_Length : constant Duration := 0.5;
         Ref_Pool : Ref_Pools.Pool;
         Workers : array (1 .. 5) of Pseudo_Process;
      begin
         Check_Counts (Test, Ref_Pool, 0, 0, 0);
         Ref_Pool.Preallocate (1);
         Check_Counts (Test, Ref_Pool, 0, 0, 1);

         --  Timeline (in Test_Length/10):     <-------->
         --  Task using reference 1:           11111 444
         --  Task using reference 2:             22222 55
         --  Task using reference 3:               33333

         Unbounded_Start (Workers (1), Ref_Pool, Test_Length * 0.5, Test, 1);
         Check_Counts (Test, Ref_Pool, 1, 1, 1);

         delay Test_Length * 0.2;
         Unbounded_Start (Workers (2), Ref_Pool, Test_Length * 0.5, Test, 2);
         Check_Counts (Test, Ref_Pool, 2, 2, 2);

         delay Test_Length * 0.2;
         Unbounded_Start (Workers (3), Ref_Pool, Test_Length * 0.5, Test, 3);
         Check_Counts (Test, Ref_Pool, 3, 3, 3);

         delay Test_Length * 0.2;
         Check_Counts (Test, Ref_Pool, 2, 3, 3);
         Unbounded_Start (Workers (4), Ref_Pool, Test_Length * 0.3, Test, 1);
         Check_Counts (Test, Ref_Pool, 3, 3, 3);

         delay Test_Length * 0.1;
         Check_Counts (Test, Ref_Pool, 2, 3, 3);
         Ref_Pool.Purge;
         Check_Counts (Test, Ref_Pool, 2, 2, 2);
         Unbounded_Start (Workers (5), Ref_Pool, Test_Length * 0.2, Test, 3);
      end;
   exception
      when Error : others => Test.Report_Exception (Error);
   end Unbounded_Pool;

end Natools.Reference_Tests.Pools;
