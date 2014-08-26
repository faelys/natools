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

package body Natools.References.Pools is

   ------------------------
   -- Helper Subprograms --
   ------------------------

   overriding procedure Finalize (Object : in out Pool_Backend) is
   begin
      Unchecked_Free (Object.Refs);
   end Finalize;


   procedure Find
     (Container : in Pool_Backend;
      First_Available : out Extended_Index;
      First_Empty : out Extended_Index) is
   begin
      First_Available := 0;
      First_Empty := 0;

      if Container.Refs = null then
         return;
      end if;

      for I in Container.Refs'Range loop
         if Container.Refs (I).Is_Empty then
            if First_Empty = 0 then
               First_Empty := I;
               exit when First_Available /= 0;
            end if;
         elsif Container.Refs (I).Is_Last then
            if First_Available = 0 then
               First_Available := I;
               exit when First_Empty /= 0;
            end if;
         end if;
      end loop;
   end Find;


   not overriding procedure Preallocate
     (Container : in out Pool_Backend;
      New_Item_Count : in Pool_Size;
      Constructor : access function return Held_Data := null) is
   begin
      if New_Item_Count = 0 then
         return;
      end if;

      if Container.Refs = null then
         Container.Refs := new Reference_Array (1 .. New_Item_Count);

         if Constructor /= null then
            for I in Container.Refs'Range loop
               Container.Refs (I) := Create (Constructor);
            end loop;
         end if;

      else
         declare
            New_Data : Reference_Array_Access
              := new Reference_Array
                 (1 .. Container.Refs'Length + New_Item_Count);
         begin
            New_Data (1 .. Container.Refs'Length) := Container.Refs.all;

            if Constructor /= null then
               for I in Container.Refs'Length + 1 .. New_Data'Last loop
                  New_Data (I) := Create (Constructor);
               end loop;
            end if;

            Unchecked_Free (Container.Refs);
            Container.Refs := New_Data;
         exception
            when others =>
               Unchecked_Free (New_Data);
               raise;
         end;
      end if;
   end Preallocate;



   ----------------------------------
   -- Public Protected Subprograms --
   ----------------------------------

   protected body Pool is

      procedure Get (Ref : out Reference) is
         First_Available, First_Empty : Extended_Index;
      begin
         Backend.Find (First_Available, First_Empty);

         if First_Available in Reference_Index then
            Ref := Backend.Refs (First_Available);
         else
            raise Constraint_Error
              with "No non-empty unused reference in pool";
         end if;
      end Get;


      procedure Get
        (Constructor : not null access function return Held_Data;
         Ref : out Reference)
      is
         First_Available, First_Empty : Extended_Index;
      begin
         Backend.Find (First_Available, First_Empty);

         if First_Available in Reference_Index then
            Ref := Backend.Refs (First_Available);
         elsif First_Empty in Reference_Index then
            Backend.Refs (First_Empty) := Create (Constructor);
            Ref := Backend.Refs (First_Empty);
         else
            raise Constraint_Error with "No unused reference in pool";
         end if;
      end Get;


      procedure Create
        (Constructor : not null access function return Held_Data;
         Ref : out Reference;
         Expand_Count : in Pool_Size := 1)
      is
         First_Available, First_Empty : Extended_Index;
      begin
         Backend.Find (First_Available, First_Empty);

         if First_Available in Reference_Index then
            Ref := Backend.Refs (First_Available);

         elsif First_Empty in Reference_Index then
            Backend.Refs (First_Empty) := Create (Constructor);
            Ref := Backend.Refs (First_Empty);

         else
            First_Available := Backend.Length + 1;
            Backend.Preallocate (Expand_Count, Constructor);
            Ref := Backend.Refs (First_Available);
         end if;
      end Create;


      procedure Preallocate
        (New_Item_Count : in Pool_Size;
         Constructor : access function return Held_Data := null) is
      begin
         Backend.Preallocate (New_Item_Count, Constructor);
      end Preallocate;


      procedure Release_Unused is
      begin
         if Backend.Refs = null then
            return;
         end if;

         for I in Backend.Refs'Range loop
            if not Backend.Refs (I).Is_Empty
              and then Backend.Refs (I).Is_Last
            then
               Backend.Refs (I).Reset;
            end if;
         end loop;
      end Release_Unused;


      procedure Trim is
         Index : Extended_Index := 0;
         New_Count : constant Pool_Size := Initialized_Size;
         New_Data : Reference_Array_Access := null;
      begin
         if New_Count = Backend.Length then
            return;
         end if;

         New_Data := new Reference_Array (1 .. New_Count);

         for I in Backend.Refs'Range loop
            if not Backend.Refs (I).Is_Empty then
               Index := Index + 1;
               New_Data (Index) := Backend.Refs (I);
            end if;
         end loop;

         pragma Assert (Index = New_Count);

         Unchecked_Free (Backend.Refs);
         Backend.Refs := New_Data;
      exception
         when others =>
            Unchecked_Free (New_Data);
            raise;
      end Trim;


      procedure Purge is
      begin
         Release_Unused;
         Trim;
      end Purge;


      function Capacity return Pool_Size is
      begin
         return Backend.Length;
      end Capacity;


      function Initialized_Size return Pool_Size is
         Result : Pool_Size := 0;
      begin
         if Backend.Refs /= null then
            for I in Backend.Refs'Range loop
               if not Backend.Refs (I).Is_Empty then
                  Result := Result + 1;
               end if;
            end loop;
         end if;

         return Result;
      end Initialized_Size;


      function Active_Size return Pool_Size is
         Result : Pool_Size := 0;
      begin
         if Backend.Refs /= null then
            for I in Backend.Refs'Range loop
               if not Backend.Refs (I).Is_Empty
                 and then not Backend.Refs (I).Is_Last
               then
                  Result := Result + 1;
               end if;
            end loop;
         end if;

         return Result;
      end Active_Size;


      procedure Unchecked_Iterate
        (Process : not null access procedure (Ref : in Reference)) is
      begin
         for I in Backend.Refs'Range loop
            Process.all (Backend.Refs (I));
         end loop;
      end Unchecked_Iterate;

   end Pool;

end Natools.References.Pools;
