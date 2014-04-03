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

with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;

package body Natools.Chunked_Strings is

   package Fixed renames Ada.Strings.Fixed;

   type Relation is (Equal, Greater, Lesser);


   -----------------------
   -- Local subprograms --
   -----------------------

   function Allocated_Size (Source : in Chunked_String) return Natural;
   pragma Inline (Allocated_Size);
      --  Return the number of Characters that can currently fit in Source


   function Chunks_For (Size            : in Natural;
                        Chunk_Size      : in Positive;
                        Allocation_Unit : in Positive)
      return Natural;
   pragma Inline (Chunks_For);
      --  Return the number of chunks to accommodate Size characters


   generic
      type Map_Type is private;
      with function Count (Source  : in String;
                           Pattern : in String;
                           Mapping : in Map_Type)
         return Natural;
   function Count_Gen (Source  : in Chunked_String;
                       Pattern : in String;
                       Mapping : in Map_Type)
      return Natural;
      --  Count the number of non-overlapping occurrences of the pattern


   function Compare
     (Left       : in Chunk_Array;
      Left_Size  : in Natural;
      Right      : in Chunk_Array;
      Right_Size : in Natural)
      return Relation;
   function Compare
     (Left       : in Chunk_Array_Access;
      Left_Size  : in Natural;
      Right      : in Chunk_Array_Access;
      Right_Size : in Natural)
      return Relation;
   function Compare
     (Left       : in Chunk_Array;
      Left_Size  : in Natural;
      Right      : in String)
      return Relation;
   function Compare
     (Left       : in Chunk_Array_Access;
      Left_Size  : in Natural;
      Right      : in String)
      return Relation;
      --  String comparisons


   procedure Fill (Data       : in out Chunk_Array;
                   From       : in     Positive;
                   Count      : in     Natural;
                   C          : in     Character;
                   Chunk_Size : in     Positive);
      --  Fill an area of the chunks with the given Character


   procedure Free (Data : in out Chunk_Array_Access);
      --  Free data associated to all chunks and to the chunk array


   generic
      type Map_Type is private;
      with function Index
        (Source  : String;
         Pattern : String;
         From :    Positive;
         Going :   Ada.Strings.Direction;
         Map :     Map_Type)
         return Natural;
   function Index_Gen
     (Source  : Chunked_String;
      Pattern : String;
      From :    Positive;
      Going :   Ada.Strings.Direction;
      Map :     Map_Type)
      return Natural;
      --  Search for a pattern in a source as described in the ARM


   procedure Move (Target          : in out Chunk_Array;
                   Target_Position : in     Positive;
                   Source          : in out Chunk_Array;
                   Source_Position : in     Positive;
                   Length          : in     Natural);
      --  Moves characters from one Chunk_Array to another, even when they
      --  do not have the same chunk size


   procedure Move (Target     : in out Chunk_Array;
                   Source     : in     String;
                   Position   : in     Positive;
                   Chunk_Size : in     Positive);
      --  Writes the string in the chunk array, which must be large enough


   procedure Move (Target :    out String;
                   Source : in     Chunk_Array;
                   From   : in     Positive);
      --  Fills a string using characters from the Chunk_Array


   procedure Move (Data            : in out Chunk_Array;
                   Target_Position : in     Positive;
                   Source_Position : in     Positive;
                   Length          : in     Positive;
                   Chunk_Size      : in     Positive);
      --  Move a slice of data inside a given chunk array


   procedure Resize_Chunk (Chunk : in out String_Access;
                           Size  : in     Positive);
      --  Resize a chunk to the target set


   procedure Resize_Chunks (Data            : in out Chunk_Array_Access;
                            Size            : in     Natural;
                            Chunk_Size      : in     Positive;
                            Allocation_Unit : in     Positive;
                            Can_Shrink      : in     Boolean := True);
      --  Resize Data to fit Size characters


   procedure Trim_Bounds (Source : in     Chunked_String;
                          Left   : in     Maps.Character_Set;
                          Right  : in     Maps.Character_Set;
                          Low    :    out Positive;
                          High   :    out Natural);
      --  Compute slice bounds of the trimmed result


   function Units_For (Size            : in Natural;
                       Chunk_Size      : in Positive;
                       Allocation_Unit : in Positive)
      return Natural;
   pragma Inline (Units_For);
      --  Return the number of allocation units in the last chunk



   ---------------------------------------
   -- Chunked_String memory subprograms --
   ---------------------------------------

   function Allocated_Size (Source : in Chunked_String) return Natural is
   begin
      if Source.Data = null or else Source.Data'Last < 1 then
         return 0;
      end if;

      return (Source.Data'Last - 1) * Source.Chunk_Size
        + Source.Data (Source.Data'Last)'Last;
   end Allocated_Size;



   function Chunks_For (Size            : in Natural;
                        Chunk_Size      : in Positive;
                        Allocation_Unit : in Positive)
      return Natural is
   begin
      pragma Unreferenced (Allocation_Unit);
      return (Size + Chunk_Size - 1) / Chunk_Size;
   end Chunks_For;



   procedure Free (Data : in out Chunk_Array_Access) is
      procedure Deallocate is
         new Ada.Unchecked_Deallocation (Chunk_Array, Chunk_Array_Access);
   begin
      if Data = null then
         return;
      end if;
      for J in Data'Range loop
         Free (Data (J));
      end loop;
      Deallocate (Data);
   end Free;



   procedure Resize_Chunk (Chunk : in out String_Access;
                           Size  : in     Positive)
   is
      New_Chunk : String_Access;
   begin
      if Size /= Chunk'Length then
         New_Chunk := new String (1 .. Size);
         if Size < Chunk'Length then
            New_Chunk.all := Chunk (Chunk'First .. Chunk'First + Size - 1);
         else
            New_Chunk.all (1 .. Chunk'Length) := Chunk.all;
         end if;
         Free (Chunk);
         Chunk := New_Chunk;
      end if;
   end Resize_Chunk;



   procedure Resize_Chunks (Data            : in out Chunk_Array_Access;
                            Size            : in     Natural;
                            Chunk_Size      : in     Positive;
                            Allocation_Unit : in     Positive;
                            Can_Shrink      : in     Boolean := True)
   is
      procedure Deallocate is
         new Ada.Unchecked_Deallocation (Chunk_Array, Chunk_Array_Access);

      Chunk_Nb : constant Natural
        := Chunks_For (Size, Chunk_Size, Allocation_Unit);
      Last_Chunk_Size : constant Natural
        := Units_For (Size, Chunk_Size, Allocation_Unit) * Allocation_Unit;
   begin
      if Size = 0 then
         if Can_Shrink then
            Free (Data);
         end if;
         return;
      end if;
      pragma Assert (Chunk_Nb > 0);

      if Data = null or else Data'Length < Chunk_Nb then
         declare
            First_New : Positive := 1;
            New_Data : constant Chunk_Array_Access
              := new Chunk_Array (1 .. Chunk_Nb);
         begin
            if Data /= null then
               Resize_Chunk (Data (Data'Last), Chunk_Size);
               New_Data (1 .. Data'Last) := Data.all;
               First_New := Data'Last + 1;
               Deallocate (Data);
            end if;
            Data := New_Data;
            for J in First_New .. Data'Last - 1 loop
               Data (J) := new String (1 .. Chunk_Size);
            end loop;
            Data (Data'Last) := new String (1 .. Last_Chunk_Size);
         end;
      elsif Data'Length > Chunk_Nb then
         if Can_Shrink then
            declare
               New_Data : constant Chunk_Array_Access
                 := new Chunk_Array (1 .. Chunk_Nb);
            begin
               Resize_Chunk (Data (Chunk_Nb), Last_Chunk_Size);
               for J in Chunk_Nb + 1 .. Data'Last loop
                  Free (Data (J));
               end loop;
               New_Data.all := Data (1 .. Chunk_Nb);
               Data := New_Data;
            end;
         end if;
      else -- Data'Length = Chunk_Nb
         if Last_Chunk_Size > Data (Data'Last).all'Last or Can_Shrink then
            Resize_Chunk (Data (Data'Last), Last_Chunk_Size);
         end if;
      end if;
   end Resize_Chunks;



   function Units_For (Size            : in Natural;
                       Chunk_Size      : in Positive;
                       Allocation_Unit : in Positive)
      return Natural is
   begin
      return (((Size + Chunk_Size - 1) mod Chunk_Size + 1)
               + Allocation_Unit - 1) / Allocation_Unit;
   end Units_For;



   ---------------------------
   -- Low-level subprograms --
   ---------------------------

   function Compare
     (Left       : in Chunk_Array;
      Left_Size  : in Natural;
      Right      : in Chunk_Array;
      Right_Size : in Natural)
      return Relation
   is
      L_Chunk  : Positive := Left'First;
      L_Pos    : Positive := Left (L_Chunk).all'First;
      L_Remain : Natural  := Left_Size;
      R_Chunk  : Positive := Right'First;
      R_Pos    : Positive := Right (R_Chunk).all'First;
      R_Remain : Natural  := Right_Size;
      Step     : Positive;
   begin
      loop
         Step := Positive'Min
                   (Natural'Min (Left  (L_Chunk).all'Last - L_Pos + 1,
                                 L_Remain),
                    Natural'Min (Right (R_Chunk).all'Last - R_Pos + 1,
                                 R_Remain));
         declare
            L_Part : String
              renames Left (L_Chunk).all (L_Pos .. L_Pos + Step - 1);
            R_Part : String
              renames Right (R_Chunk).all (R_Pos .. R_Pos + Step - 1);
         begin
            if L_Part < R_Part then
               return Lesser;
            elsif L_Part > R_Part then
               return Greater;
            end if;
         end;

         L_Remain := L_Remain - Step;
         R_Remain := R_Remain - Step;
         if L_Remain = 0 and R_Remain = 0 then
            return Equal;
         elsif L_Remain = 0 then
            return Lesser;
         elsif R_Remain = 0 then
            return Greater;
         end if;

         L_Pos := L_Pos + Step;
         R_Pos := R_Pos + Step;

         if L_Pos > Left (L_Chunk).all'Last then
            --  L_Chunk cannot be Left'Last because L_Remain > 0
            L_Chunk := L_Chunk + 1;
            L_Pos := Left (L_Chunk).all'First;
         end if;

         if R_Pos > Right (R_Chunk).all'Last then
            --  R_Chunk cannot be Right'Last because R_Remain > 0
            R_Chunk := R_Chunk + 1;
            R_Pos := Right (R_Chunk).all'First;
         end if;
      end loop;
   end Compare;



   function Compare
     (Left       : in Chunk_Array_Access;
      Left_Size  : in Natural;
      Right      : in Chunk_Array_Access;
      Right_Size : in Natural)
      return Relation is
   begin
      if Left = null or Left_Size = 0 then
         if Right = null or Right_Size = 0 then
            return Equal;
         else
            return Lesser;
         end if;
      else
         if Right = null or Right_Size = 0 then
            return Greater;
         else
            return Compare (Left.all, Left_Size, Right.all, Right_Size);
         end if;
      end if;
   end Compare;



   function Compare
     (Left       : in Chunk_Array;
      Left_Size  : in Natural;
      Right      : in String)
      return Relation
   is
      Chunk    : Positive := Left'First;
      L_Pos    : Positive := Left (Chunk).all'First;
      L_Remain : Natural  := Left_Size;
      R_Pos    : Positive := Right'First;
      Step     : Positive;
   begin
      loop
         Step
           := Positive'Min (Positive'Min (Left (Chunk).all'Last - L_Pos + 1,
                                          L_Remain),
                            Right'Last - R_Pos + 1);
         declare
            L_Part : String
              renames Left (Chunk).all (L_Pos .. L_Pos + Step - 1);
            R_Part : String
              renames Right (R_Pos .. R_Pos + Step - 1);
         begin
            if L_Part < R_Part then
               return Lesser;
            elsif L_Part > R_Part then
               return Greater;
            end if;
         end;

         L_Remain := L_Remain - Step;
         if L_Remain = 0 then
            if R_Pos + Step > Right'Last then
               return Equal;
            else
               return Lesser;
            end if;
         end if;

         L_Pos := L_Pos + Step;
         R_Pos := R_Pos + Step;

         if L_Pos > Left (Chunk).all'Last then
            --  _Chunk cannot be Left'Last because L_Remain > 0
            Chunk := Chunk + 1;
            L_Pos := Left (Chunk).all'First;
         end if;
         if R_Pos > Right'Last then
            return Greater;
         end if;
      end loop;
   end Compare;



   function Compare
     (Left       : in Chunk_Array_Access;
      Left_Size  : in Natural;
      Right      : in String)
      return Relation is
   begin
      if Left = null or Left_Size = 0 then
         if Right'Length = 0 then
            return Equal;
         else
            return Lesser;
         end if;
      else
         if Right'Length = 0 then
            return Greater;
         else
            return Compare (Left.all, Left_Size, Right);
         end if;
      end if;
   end Compare;



   procedure Fill (Data       : in out Chunk_Array;
                   From       : in     Positive;
                   Count      : in     Natural;
                   C          : in     Character;
                   Chunk_Size : in     Positive)
   is
      Chunk  : Positive := (From - 1) / Chunk_Size + 1;
      Offset : Positive := (From - 1) mod Chunk_Size + 1;
      Done   : Natural  := 0;
      Step   : Positive;
   begin
      while Done < Count loop
         Step := Positive'Min (Count - Done,
                               Data (Chunk).all'Last - Offset + 1);
         Data (Chunk).all (Offset .. Offset + Step - 1)
           := Ada.Strings.Fixed."*" (Step, C);
         Chunk := Chunk + 1;
         Offset := 1;
         Done := Done + Step;
      end loop;
   end Fill;



   function Is_Valid (Source : in Chunked_String) return Boolean is
   begin
      --  Null data is only acceptable when the string is empty.
      if Source.Data = null then
         return Source.Size = 0;
      end if;

      --  Data array must contain non-null chunks of even size
      declare
         D : Chunk_Array renames Source.Data.all;
      begin
         if D'First /= 1 then
            return False;
         end if;
         for J in D'Range loop
            if D (J) = null then
               return False;
            end if;

            if D (J).all'First /= 1 or
               (J < D'Last and D (J).all'Last /= Source.Chunk_Size)
            then
               return False;
            end if;
         end loop;
      end;

      --  Real size must be smaller than allocated size
      if Source.Size > Allocated_Size (Source) then
         return False;
      end if;

      return True;
   end Is_Valid;



   procedure Move (Target          : in out Chunk_Array;
                   Target_Position : in     Positive;
                   Source          : in out Chunk_Array;
                   Source_Position : in     Positive;
                   Length          : in     Natural)
   is
      Count   : Natural := 0;
      S_Chunk : Positive;
      S_Pos   : Positive;
      T_Chunk : Positive;
      T_Pos   : Positive;
   begin
      S_Chunk := Target'First;
      S_Pos := 1;
      while S_Pos + Source (S_Chunk).all'Length <= Source_Position loop
         S_Pos := S_Pos + Source (S_Chunk).all'Length;
         S_Chunk := S_Chunk + 1;
      end loop;
      S_Pos := Source_Position + 1 - S_Pos;

      T_Chunk := Target'First;
      T_Pos := 1;
      while T_Pos + Target (T_Chunk).all'Length <= Target_Position loop
         T_Pos := T_Pos + Target (T_Chunk).all'Length;
         T_Chunk := T_Chunk + 1;
      end loop;
      T_Pos := Target_Position + 1 - T_Pos;

      while Count < Length loop
         declare
            S_String : String renames Source (S_Chunk).all;
            T_String : String renames Target (T_Chunk).all;
            Step_C   : constant Positive := Length - Count;
            Step_S   : constant Positive := S_String'Last - S_Pos + 1;
            Step_T   : constant Positive := T_String'Last - T_Pos + 1;
            Step     : constant Positive
              := Positive'Min (Step_C, Positive'Min (Step_S, Step_T));
         begin
            T_String (T_Pos .. T_Pos + Step - 1)
              := S_String (S_Pos .. S_Pos + Step - 1);
            Count := Count + Step;
            exit when Count >= Length;
            S_Pos := S_Pos + Step;
            T_Pos := T_Pos + Step;
            if S_Pos > S_String'Last then
               S_Chunk := S_Chunk + 1;
               S_Pos := Source (S_Chunk).all'First;
            end if;
            if T_Pos > T_String'Last then
               T_Chunk := T_Chunk + 1;
               T_Pos := Target (T_Chunk).all'First;
            end if;
         end;
      end loop;
   end Move;



   procedure Move (Target     : in out Chunk_Array;
                   Source     : in     String;
                   Position   : in     Positive;
                   Chunk_Size : in     Positive)
   is
      Last_Position : constant Positive := Position + Source'Length - 1;
      First_Chunk   : constant Positive := (Position - 1) / Chunk_Size + 1;
      First_Offset  : constant Positive := (Position - 1) mod Chunk_Size + 1;
      Last_Chunk    : constant Positive
        := (Last_Position - 1) / Chunk_Size + 1;
      Last_Offset   : constant Positive
        := (Last_Position - 1) mod Chunk_Size + 1;
      Current : Positive;
   begin
      if First_Chunk = Last_Chunk then
         Target (First_Chunk).all (First_Offset .. Last_Offset) := Source;
      else
         Current := Source'First + Chunk_Size - First_Offset + 1;
         Target (First_Chunk).all (First_Offset .. Chunk_Size)
           := Source (Source'First .. Current - 1);
         for J in First_Chunk + 1 .. Last_Chunk - 1 loop
            Target (J).all := Source (Current .. Current + Chunk_Size - 1);
            Current := Current + Chunk_Size;
         end loop;
         Target (Last_Chunk).all (1 .. Last_Offset)
           := Source (Current .. Source'Last);
      end if;
   end Move;



   procedure Move (Target :    out String;
                   Source : in     Chunk_Array;
                   From   : in     Positive)
   is
      T_Pos : Positive := Target'First;
      S_Pos : Positive := 1;
      Chunk : Positive := 1;
      Step  : Positive;
   begin
      while S_Pos + Source (Chunk).all'Length <= From loop
         S_Pos := S_Pos + Source (Chunk).all'Length;
         Chunk := Chunk + 1;
      end loop;
      S_Pos := From - S_Pos + 1;

      Step := Source (Chunk).all'Last - S_Pos + 1;
      if Target'Length <= Step then
         Target := Source (Chunk).all (S_Pos .. S_Pos + Target'Length - 1);
         return;
      end if;

      Target (T_Pos .. T_Pos + Step - 1)
        := Source (Chunk).all (S_Pos .. Source (Chunk).all'Last);
      T_Pos := T_Pos + Step;
      Chunk := Chunk + 1;

      while T_Pos <= Target'Last loop
         Step := Positive'Min (Source (Chunk).all'Length,
                               Target'Last - T_Pos + 1);
         Target (T_Pos .. T_Pos + Step - 1)
           := Source (Chunk).all (1 .. Step);
         T_Pos := T_Pos + Step;
         Chunk := Chunk + 1;
      end loop;
   end Move;



   procedure Move (Data            : in out Chunk_Array;
                   Target_Position : in     Positive;
                   Source_Position : in     Positive;
                   Length          : in     Positive;
                   Chunk_Size      : in     Positive) is
   begin
      if Target_Position < Source_Position then
         declare
            S_Chunk : Positive := (Source_Position - 1)  /  Chunk_Size + 1;
            S_Pos   : Positive := (Source_Position - 1) mod Chunk_Size + 1;
            T_Chunk : Positive := (Target_Position - 1)  /  Chunk_Size + 1;
            T_Pos   : Positive := (Target_Position - 1) mod Chunk_Size + 1;
            Count : Natural := 0;
            Step : Positive;
         begin
            while Count < Length loop
               Step := Positive'Min
                         (Positive'Min (Data (S_Chunk).all'Last - S_Pos + 1,
                                        Data (T_Chunk).all'Last - T_Pos + 1),
                          Length - Count);
               Data (T_Chunk).all (T_Pos .. T_Pos + Step - 1)
                 := Data (S_Chunk).all (S_Pos .. S_Pos + Step - 1);
               Count := Count + Step;

               S_Pos := S_Pos + Step;
               if S_Pos > Chunk_Size then
                  S_Chunk := S_Chunk + 1;
                  S_Pos := 1;
               end if;

               T_Pos := T_Pos + Step;
               if T_Pos > Chunk_Size then
                  T_Chunk := T_Chunk + 1;
                  T_Pos := 1;
               end if;
            end loop;
         end;
      elsif Target_Position > Source_Position then
         declare
            S_End : constant Positive := Source_Position + Length - 1;
            T_End : constant Positive := Target_Position + Length - 1;
            S_Chunk : Positive := (S_End - 1)  /  Chunk_Size + 1;
            S_Pos   : Positive := (S_End - 1) mod Chunk_Size + 1;
            T_Chunk : Positive := (T_End - 1)  /  Chunk_Size + 1;
            T_Pos   : Positive := (T_End - 1) mod Chunk_Size + 1;
            Count : Natural := 0;
            Step : Positive;
         begin
            loop
               Step := Positive'Min (Positive'Min (S_Pos, T_Pos),
                                     Length - Count);
               Data (T_Chunk).all (T_Pos - Step + 1 .. T_Pos)
                 := Data (S_Chunk).all (S_Pos - Step + 1 .. S_Pos);
               Count := Count + Step;
               exit when Count = Length;
               pragma Assert (Count < Length);

               if S_Pos <= Step then
                  S_Chunk := S_Chunk - 1;
                  S_Pos := Chunk_Size;
               else
                  S_Pos := S_Pos - Step;
               end if;

               if T_Pos <= Step then
                  T_Chunk := T_Chunk - 1;
                  T_Pos := Chunk_Size;
               else
                  T_Pos := T_Pos - Step;
               end if;
            end loop;
         end;
      end if;
   end Move;



   --------------------------------------------------
   -- Public interface specific to Chunked_Strings --
   --------------------------------------------------


   function Build (Depth : Positive)
      return Natools.Accumulators.String_Accumulator'Class
   is
      pragma Unreferenced (Depth);
   begin
      return Null_Chunked_String;
   end Build;



   function Duplicate (Source : in Chunked_String) return Chunked_String is
      Data : Chunk_Array_Access := null;
   begin
      if Source.Data /= null then
         Data := new Chunk_Array (Source.Data'Range);
         for J in Source.Data'Range loop
            Data (J) := new String'(Source.Data (J).all);
         end loop;
      end if;

      return Chunked_String'(Ada.Finalization.Controlled with
                             Chunk_Size      => Source.Chunk_Size,
                             Allocation_Unit => Source.Allocation_Unit,
                             Size            => Source.Size,
                             Data            => Data);
   end Duplicate;



   procedure Hard_Reset (Str : in out Chunked_String) is
   begin
      Str.Size := 0;
      Free (Str.Data);
   end Hard_Reset;



   procedure Soft_Reset (Str : in out Chunked_String) is
   begin
      Str.Size := 0;
   end Soft_Reset;



   procedure To_String (Source : Chunked_String; Output : out String) is
      Position : Positive := Output'First;
      Step : Positive;
   begin
      if Source.Size > 0 then
         for J in Source.Data'Range loop
            Step := Positive'Min (Source.Data (J).all'Length,
                                  Source.Size - Position + 1);
            Output (Position .. Position + Step - 1)
              := Source.Data (J).all (1 .. Step);
            Position := Position + Step;
            exit when Position > Source.Size;
         end loop;
         pragma Assert (Position = Source.Size + 1);
      end if;
   end To_String;



   -------------------------------------------
   -- String_Accumulator specific interface --
   -------------------------------------------


   function Tail (Source : in Chunked_String; Size : in Natural)
      return String
   is
      Actual_Size : constant Natural := Natural'Min (Size, Source.Size);
   begin
      return Slice (Source, Source.Size - Actual_Size + 1, Source.Size);
   end Tail;



   procedure Unappend (From : in out Chunked_String; Text : in String) is
   begin
      if Text'Length <= From.Size
         and then String'(Tail (From, Text'Length)) = Text
      then
         From.Size := From.Size - Text'Length;
      end if;
   end Unappend;



   ------------------------
   -- Standard interface --
   ------------------------

   function Length (Source : in Chunked_String) return Natural is
   begin
      return Source.Size;
   end Length;



   procedure Deallocate is
      new Ada.Unchecked_Deallocation (String, String_Access);

   procedure Free (X : in out String_Access) is
   begin
      Deallocate (X);
   end Free;


   procedure Free_Extra_Memory (From : in out Chunked_String) is
   begin
      Resize_Chunks (From.Data, From.Size,
                     From.Chunk_Size, From.Allocation_Unit,
                     Can_Shrink => True);
   end Free_Extra_Memory;


   procedure Preallocate (Str : in out Chunked_String; Size : Natural) is
   begin
      Resize_Chunks (Str.Data, Size, Str.Chunk_Size, Str.Allocation_Unit,
                     Can_Shrink => False);
   end Preallocate;


   function To_Chunked_String
     (Source          : in String;
      Chunk_Size      : in Positive := Default_Chunk_Size;
      Allocation_Unit : in Positive := Default_Allocation_Unit)
      return Chunked_String
   is
      Data : Chunk_Array_Access := null;
   begin
      if Source'Length > 0 then
         Resize_Chunks (Data, Source'Length, Chunk_Size, Allocation_Unit);
         Move (Data.all, Source, 1, Chunk_Size);
      end if;
      return Chunked_String'(Ada.Finalization.Controlled with
                             Chunk_Size      => Chunk_Size,
                             Allocation_Unit => Allocation_Unit,
                             Size            => Source'Length,
                             Data            => Data);
   end To_Chunked_String;



   function To_Chunked_String
     (Length          : in Natural;
      Chunk_Size      : in Positive := Default_Chunk_Size;
      Allocation_Unit : in Positive := Default_Allocation_Unit)
      return Chunked_String
   is
      Data : Chunk_Array_Access := null;
   begin
      Resize_Chunks (Data, Length, Chunk_Size, Allocation_Unit);
      return Chunked_String'(Ada.Finalization.Controlled with
                             Chunk_Size      => Chunk_Size,
                             Allocation_Unit => Allocation_Unit,
                             Size            => Length,
                             Data            => Data);
   end To_Chunked_String;



   function To_String (Source : in Chunked_String) return String is
      Value : String (1 .. Source.Size);
   begin
      To_String (Source, Value);
      return Value;
   end To_String;



   procedure Set_Chunked_String
     (Target          :    out Chunked_String;
      Source          : in     String;
      Chunk_Size      : in     Positive := Default_Chunk_Size;
      Allocation_Unit : in     Positive := Default_Allocation_Unit) is
   begin
      Resize_Chunks (Target.Data, Source'Length,
                     Chunk_Size, Allocation_Unit,
                     Can_Shrink => True);
      Target.Chunk_Size := Chunk_Size;
      Target.Allocation_Unit := Allocation_Unit;
      Target.Size := Source'Length;
      if Target.Size > 0 then
         Move (Target.Data.all, Source, 1, Chunk_Size);
      end if;
   end Set_Chunked_String;



   procedure Append (Source   : in out Chunked_String;
                     New_Item : in Chunked_String)
   is
      New_Size : constant Natural := Source.Size + New_Item.Size;
   begin
      Resize_Chunks (Source.Data, New_Size,
                     Source.Chunk_Size, Source.Allocation_Unit,
                     Can_Shrink => False);
      Move (Source.Data.all, Source.Size + 1,
            New_Item.Data.all, 1,
            New_Item.Size);
      Source.Size := New_Size;
   end Append;



   procedure Append (Source   : in out Chunked_String;
                     New_Item : in     String)
   is
      New_Size : constant Natural := Source.Size + New_Item'Length;
   begin
      Resize_Chunks (Source.Data, New_Size,
                     Source.Chunk_Size, Source.Allocation_Unit,
                     Can_Shrink => False);
      Move (Source.Data.all, New_Item, Source.Size + 1, Source.Chunk_Size);
      Source.Size := New_Size;
   end Append;



   procedure Append (Source   : in out Chunked_String;
                     New_Item : in     Character)
   is
      S : constant String (1 .. 1) := (1 => New_Item);
   begin
      Append (Source, S);
   end Append;



   function "&" (Left, Right : in Chunked_String)
      return Chunked_String
   is
      Size : constant Natural := Left.Size + Right.Size;
      Data : Chunk_Array_Access := null;
   begin
      Resize_Chunks (Data, Size, Default_Chunk_Size, Default_Allocation_Unit);
      if Left.Size > 0 then
         Move (Data.all, 1, Left.Data.all, 1, Left.Size);
      end if;
      if Right.Size > 0 then
         Move (Data.all, 1 + Left.Size, Right.Data.all, 1, Right.Size);
      end if;
      return Chunked_String'(Ada.Finalization.Controlled with
                             Chunk_Size      => Default_Chunk_Size,
                             Allocation_Unit => Default_Allocation_Unit,
                             Size            => Size,
                             Data            => Data);
   end "&";



   function "&" (Left : in Chunked_String; Right : in String)
      return Chunked_String
   is
      Size : constant Natural := Left.Size + Right'Length;
      Data : Chunk_Array_Access := null;
   begin
      Resize_Chunks (Data, Size, Default_Chunk_Size, Default_Allocation_Unit);
      if Left.Size > 0 then
         Move (Data.all, 1, Left.Data.all, 1, Left.Size);
      end if;
      if Right'Length > 0 then
         Move (Data.all, Right, 1 + Left.Size, Default_Chunk_Size);
      end if;
      return Chunked_String'(Ada.Finalization.Controlled with
                             Chunk_Size      => Default_Chunk_Size,
                             Allocation_Unit => Default_Allocation_Unit,
                             Size            => Size,
                             Data            => Data);
   end "&";



   function "&" (Left : in String; Right : in Chunked_String)
      return Chunked_String
   is
      Size : constant Natural := Left'Length + Right.Size;
      Data : Chunk_Array_Access := null;
   begin
      Resize_Chunks (Data, Size, Default_Chunk_Size, Default_Allocation_Unit);
      if Left'Length > 0 then
         Move (Data.all, Left, 1, Default_Chunk_Size);
      end if;
      if Right.Size > 0 then
         Move (Data.all, 1 + Left'Length, Right.Data.all, 1, Right.Size);
      end if;
      return Chunked_String'(Ada.Finalization.Controlled with
                             Chunk_Size      => Default_Chunk_Size,
                             Allocation_Unit => Default_Allocation_Unit,
                             Size            => Size,
                             Data            => Data);
   end "&";



   function "&" (Left : in Chunked_String; Right : in Character)
      return Chunked_String
   is
      Size            : constant Natural := Left.Size + 1;
      Allocation_Unit : constant Positive := Default_Allocation_Unit;
      Chunk_Size      : constant Positive := Default_Chunk_Size;
      Data            : Chunk_Array_Access := null;
   begin
      Resize_Chunks (Data, Size, Chunk_Size, Allocation_Unit);
      if Left.Size > 0 then
         Move (Data.all, 1, Left.Data.all, 1, Left.Size);
      end if;
      declare
         Position : constant Positive := Left.Size + 1;
         Chunk    : constant Positive := (Position - 1) / Chunk_Size + 1;
         Offset   : constant Positive := (Position - 1) mod Chunk_Size + 1;
      begin
         Data (Chunk).all (Offset) := Right;
      end;
      return Chunked_String'(Ada.Finalization.Controlled with
                             Chunk_Size      => Chunk_Size,
                             Allocation_Unit => Allocation_Unit,
                             Size            => Size,
                             Data            => Data);
   end "&";



   function "&" (Left : in Character; Right : in Chunked_String)
      return Chunked_String
   is
      Size : constant Natural := 1 + Right.Size;
      Data : Chunk_Array_Access := null;
   begin
      Resize_Chunks (Data, Size, Default_Chunk_Size, Default_Allocation_Unit);
      Data (1).all (1) := Left;
      if Right.Size > 0 then
         Move (Data.all, 2, Right.Data.all, 1, Right.Size);
      end if;
      return Chunked_String'(Ada.Finalization.Controlled with
                             Chunk_Size      => Default_Chunk_Size,
                             Allocation_Unit => Default_Allocation_Unit,
                             Size            => Size,
                             Data            => Data);
   end "&";



   function Element (Source : in Chunked_String;
                     Index  : in Positive)
      return Character
   is
      Chunk  : constant Positive := (Index - 1) / Source.Chunk_Size + 1;
      Offset : constant Positive := (Index - 1) mod Source.Chunk_Size + 1;
   begin
      if Index > Source.Size then
         raise Ada.Strings.Index_Error;
      end if;
      return Source.Data (Chunk).all (Offset);
   end Element;



   procedure Replace_Element (Source : in out Chunked_String;
                              Index  : in Positive;
                              By     : in Character)
   is
      Chunk  : constant Positive := (Index - 1) / Source.Chunk_Size + 1;
      Offset : constant Positive := (Index - 1) mod Source.Chunk_Size + 1;
   begin
      if Index > Source.Size then
         raise Ada.Strings.Index_Error;
      end if;
      Source.Data (Chunk).all (Offset) := By;
   end Replace_Element;



   function Slice (Source : in Chunked_String;
                   Low    : in Positive;
                   High   : in Natural)
      return String
   is
      Returned : String (Low .. High);
   begin
      if Low > Source.Size + 1 or High > Source.Size then
         raise Ada.Strings.Index_Error;
      end if;
      if High >= Low then
         Move (Returned, Source.Data.all, Low);
      end if;
      return Returned;
   end Slice;



   function Chunked_Slice
     (Source          : in Chunked_String;
      Low             : in Positive;
      High            : in Natural;
      Chunk_Size      : in Positive := Default_Chunk_Size;
      Allocation_Unit : in Positive := Default_Allocation_Unit)
      return Chunked_String
   is
      Data : Chunk_Array_Access := null;
      Size : Natural := 0;
   begin
      if Low > Source.Size + 1 or High > Source.Size then
         raise Ada.Strings.Index_Error;
      end if;
      if Low <= High then
         Size := High - Low + 1;
         Resize_Chunks (Data, Size, Chunk_Size, Allocation_Unit);
         Move (Data.all, 1, Source.Data.all, Low, Size);
      end if;
      return Chunked_String'(Ada.Finalization.Controlled with
                             Chunk_Size      => Chunk_Size,
                             Allocation_Unit => Allocation_Unit,
                             Size            => Size,
                             Data            => Data);
   end Chunked_Slice;



   procedure Chunked_Slice
     (Source          : in     Chunked_String;
      Target          :    out Chunked_String;
      Low             : in     Positive;
      High            : in     Natural;
      Chunk_Size      : in     Positive := Default_Chunk_Size;
      Allocation_Unit : in     Positive := Default_Allocation_Unit) is
   begin
      if Low > Source.Size + 1 or High > Source.Size then
         raise Ada.Strings.Index_Error;
      end if;
      Target.Chunk_Size := Chunk_Size;
      Target.Allocation_Unit := Allocation_Unit;
      if Low <= High then
         Target.Size := High - Low + 1;
         Resize_Chunks (Target.Data, Target.Size,
                        Chunk_Size, Allocation_Unit,
                        Can_Shrink => True);
         Move (Target.Data.all, 1, Source.Data.all, Low, Target.Size);
      else
         Target.Size := 0;
         Target.Data := null;
      end if;
   end Chunked_Slice;



   function "=" (Left, Right : in Chunked_String) return Boolean is
   begin
      return Compare (Left.Data, Left.Size, Right.Data, Right.Size) = Equal;
   end "=";



   function "=" (Left : in Chunked_String; Right : in String)
      return Boolean is
   begin
      return Compare (Left.Data, Left.Size, Right) = Equal;
   end "=";



   function "=" (Left : in String; Right : in Chunked_String)
      return Boolean is
   begin
      return Compare (Right.Data, Right.Size, Left) = Equal;
   end "=";



   function "<" (Left, Right : in Chunked_String) return Boolean is
   begin
      return Compare (Left.Data, Left.Size, Right.Data, Right.Size) = Lesser;
   end "<";



   function "<" (Left : in Chunked_String; Right : in String)
      return Boolean is
   begin
      return Compare (Left.Data, Left.Size, Right) = Lesser;
   end "<";



   function "<" (Left : in String; Right : in Chunked_String)
      return Boolean is
   begin
      return Compare (Right.Data, Right.Size, Left) = Greater;
   end "<";



   function "<=" (Left, Right : in Chunked_String) return Boolean is
   begin
      return Compare (Left.Data, Left.Size, Right.Data, Right.Size) /= Greater;
   end "<=";



   function "<=" (Left : in Chunked_String; Right : in String)
      return Boolean is
   begin
      return Compare (Left.Data, Left.Size, Right) /= Greater;
   end "<=";



   function "<=" (Left : in String; Right : in Chunked_String)
      return Boolean is
   begin
      return Compare (Right.Data, Right.Size, Left) /= Lesser;
   end "<=";



   function ">" (Left, Right : in Chunked_String) return Boolean is
   begin
      return Compare (Left.Data, Left.Size, Right.Data, Right.Size) = Greater;
   end ">";



   function ">" (Left : in Chunked_String; Right : in String)
      return Boolean is
   begin
      return Compare (Left.Data, Left.Size, Right) = Greater;
   end ">";



   function ">" (Left : in String; Right : in Chunked_String)
      return Boolean is
   begin
      return Compare (Right.Data, Right.Size, Left) = Lesser;
   end ">";



   function ">=" (Left, Right : in Chunked_String) return Boolean is
   begin
      return Compare (Left.Data, Left.Size, Right.Data, Right.Size) /= Lesser;
   end ">=";



   function ">=" (Left : in Chunked_String; Right : in String)
      return Boolean is
   begin
      return Compare (Left.Data, Left.Size, Right) /= Lesser;
   end ">=";



   function ">=" (Left : in String; Right : in Chunked_String)
      return Boolean is
   begin
      return Compare (Right.Data, Right.Size, Left) /= Greater;
   end ">=";



   function Index_Gen
     (Source  : Chunked_String;
      Pattern : String;
      From :    Positive;
      Going :   Ada.Strings.Direction;
      Map :     Map_Type)
      return Natural is
   begin
      if Pattern = "" then
         raise Ada.Strings.Pattern_Error;
      end if;
      if Source.Size = 0 and From = 1 then
         return 0;
      end if;
      if From > Source.Size then
         raise Ada.Strings.Index_Error;
      end if;

      declare
         Chunk  : Positive := (From - 1) / Source.Chunk_Size + 1;
         Offset : Positive := (From - 1) mod Source.Chunk_Size + 1;
         Buffer : String (1 .. Source.Chunk_Size + Pattern'Length - 1);
         Result : Natural;
         Span   : Positive;
      begin
         case (Going) is
         when Ada.Strings.Forward =>
            while (Chunk - 1) * Source.Chunk_Size + Pattern'Length
                  <= Source.Size
            loop
               Span := Positive'Min
                         (Source.Chunk_Size + Pattern'Length - 1,
                          Source.Size - (Chunk - 1) * Source.Chunk_Size);
               Move (Buffer (1 .. Span),
                     Source.Data.all,
                     (Chunk - 1) * Source.Chunk_Size + 1);
               Result := Index (Buffer (1 .. Span),
                                Pattern, Offset, Going, Map);
               if Result /= 0 then
                  return (Chunk - 1) * Source.Chunk_Size + Result;
               end if;
               Chunk := Chunk + 1;
               Offset := 1;
            end loop;
            return 0;
         when Ada.Strings.Backward =>
            loop
               Span := Positive'Min
                         (Source.Chunk_Size + Pattern'Length - 1,
                          Source.Size - (Chunk - 1) * Source.Chunk_Size);
               Move (Buffer (1 .. Span),
                     Source.Data.all,
                     (Chunk - 1) * Source.Chunk_Size + 1);
               Result := Index (Buffer (1 .. Span),
                                Pattern, Offset, Going, Map);
               if Result /= 0 then
                  return (Chunk - 1) * Source.Chunk_Size + Result;
               end if;
               exit when Chunk = 1;
               Chunk := Chunk - 1;
               Offset := Positive'Min (Source.Chunk_Size + Pattern'Length - 1,
                                       Source.Chunk_Size + Offset);
            end loop;
            return 0;
         end case;
      end;
   end Index_Gen;



   function Index_Mapping is
      new Index_Gen (Maps.Character_Mapping, Ada.Strings.Fixed.Index);

   function Index (Source  : in Chunked_String;
                   Pattern : in String;
                   From    : in Positive;
                   Going   : in Ada.Strings.Direction := Ada.Strings.Forward;
                   Mapping : in Maps.Character_Mapping := Maps.Identity)
      return Natural
      renames Index_Mapping;



   function Index_Mapping_Function is
      new Index_Gen (Maps.Character_Mapping_Function, Ada.Strings.Fixed.Index);

   function Index (Source  : in Chunked_String;
                   Pattern : in String;
                   From    : in Positive;
                   Going   : in Ada.Strings.Direction := Ada.Strings.Forward;
                   Mapping : in Maps.Character_Mapping_Function)
      return Natural
      renames Index_Mapping_Function;



   function Index (Source  : in Chunked_String;
                   Pattern : in String;
                   Going   : in Ada.Strings.Direction := Ada.Strings.Forward;
                   Mapping : in Maps.Character_Mapping := Maps.Identity)
      return Natural is
   begin
      case (Going) is
      when Ada.Strings.Forward =>
         return Index (Source, Pattern, 1, Going, Mapping);
      when Ada.Strings.Backward =>
         return Index (Source, Pattern, Source.Size, Going, Mapping);
      end case;
   end Index;



   function Index (Source  : in Chunked_String;
                   Pattern : in String;
                   Going   : in Ada.Strings.Direction := Ada.Strings.Forward;
                   Mapping : in Maps.Character_Mapping_Function)
      return Natural is
   begin
      case (Going) is
      when Ada.Strings.Forward =>
         return Index (Source, Pattern, 1, Going, Mapping);
      when Ada.Strings.Backward =>
         return Index (Source, Pattern, Source.Size, Going, Mapping);
      end case;
   end Index;



   function Index (Source : in Chunked_String;
                   Set    : in Maps.Character_Set;
                   From   : in Positive;
                   Test   : in Ada.Strings.Membership := Ada.Strings.Inside;
                   Going  : in Ada.Strings.Direction := Ada.Strings.Forward)
      return Natural
   is
      Chunk  : Positive := (From - 1) / Source.Chunk_Size + 1;
      Offset : Positive := (From - 1) mod Source.Chunk_Size + 1;
      Result : Natural;
   begin
      if From > Source.Size then
         raise Ada.Strings.Index_Error;
      end if;

      case (Going) is
      when Ada.Strings.Forward =>
         loop
            Result := Ada.Strings.Fixed.Index
              (Source.Data (Chunk).all
                 (1 .. Positive'Min (Source.Size
                                       - (Chunk - 1) * Source.Chunk_Size,
                                     Source.Chunk_Size)),
               Set, Offset, Test, Going);
            if Result /= 0 then
               return (Chunk - 1) * Source.Chunk_Size + Result;
            end if;
            if Chunk = Source.Data'Last then
               return 0;
            end if;
            Chunk := Chunk + 1;
            Offset := 1;
         end loop;
      when Ada.Strings.Backward =>
         loop
            Result := Ada.Strings.Fixed.Index
              (Source.Data (Chunk).all
                 (1 .. Positive'Min (Source.Size
                                       - (Chunk - 1) * Source.Chunk_Size,
                                     Source.Chunk_Size)),
               Set, Offset, Test, Going);
            if Result /= 0 then
               return (Chunk - 1) * Source.Chunk_Size + Result;
            end if;
            if Chunk = Source.Data'First then
               return 0;
            end if;
            Chunk := Chunk - 1;
            Offset := Source.Chunk_Size;
         end loop;
      end case;
   end Index;



   function Index (Source : in Chunked_String;
                   Set    : in Maps.Character_Set;
                   Test   : in Ada.Strings.Membership := Ada.Strings.Inside;
                   Going  : in Ada.Strings.Direction := Ada.Strings.Forward)
      return Natural is
   begin
      case Going is
      when Ada.Strings.Forward =>
         return Index (Source, Set, 1, Test, Going);
      when Ada.Strings.Backward =>
         return Index (Source, Set, Source.Size, Test, Going);
      end case;
   end Index;



   function Index_Non_Blank (Source : in Chunked_String;
                             From   : in Positive;
                             Going  : in Ada.Strings.Direction
                                         := Ada.Strings.Forward)
      return Natural is
   begin
      return Index (Source,
                    Maps.To_Set (Ada.Strings.Space),
                    From,
                    Ada.Strings.Outside,
                    Going);
   end Index_Non_Blank;



   function Index_Non_Blank (Source : in Chunked_String;
                             Going  : in Ada.Strings.Direction
                                         := Ada.Strings.Forward)
      return Natural is
   begin
      return Index (Source,
                    Maps.To_Set (Ada.Strings.Space),
                    Ada.Strings.Outside,
                    Going);
   end Index_Non_Blank;



   function Count_Gen (Source  : in Chunked_String;
                       Pattern : in String;
                       Mapping : in Map_Type)
      return Natural
   is
      Buffer : String (1 .. Source.Chunk_Size + Pattern'Length - 1);
      Result : Natural := 0;
      Step   : Positive;
   begin
      if Pattern = "" then
         raise Ada.Strings.Pattern_Error;
      end if;
      if Source.Size < Pattern'Length then
         return 0;
      end if;

      for J in Source.Data'Range loop
         Step := Positive'Min (Source.Size - (J - 1) * Source.Chunk_Size,
                               Source.Chunk_Size + Pattern'Length - 1);
         Move (Buffer (1 .. Step),
               Source.Data.all,
               (J - 1) * Source.Chunk_Size + 1);
         Result := Result + Count (Buffer (1 .. Step),
                                   Pattern,
                                   Mapping);
      end loop;
      return Result;
   end Count_Gen;

   function Count_Mapping is
      new Count_Gen (Maps.Character_Mapping, Ada.Strings.Fixed.Count);

   function Count (Source  : in Chunked_String;
                   Pattern : in String;
                   Mapping : in Maps.Character_Mapping := Maps.Identity)
      return Natural
      renames Count_Mapping;

   function Count_Mapping_Function is
      new Count_Gen (Maps.Character_Mapping_Function, Ada.Strings.Fixed.Count);

   function Count (Source  : in Chunked_String;
                   Pattern : in String;
                   Mapping : in Maps.Character_Mapping_Function)
      return Natural
      renames Count_Mapping_Function;



   function Count (Source  : in Chunked_String;
                   Set     : in Maps.Character_Set)
      return Natural
   is
      Result : Natural := 0;
      Done : Natural := 0;
   begin
      if Source.Size > 0 then
         for C in Source.Data'Range loop
            declare
               Chunk : String renames Source.Data (C).all;
               Step : constant Natural
                 := Natural'Min (Source.Size - Done, Chunk'Length);
            begin
               Result := Result + Ada.Strings.Fixed.Count
                 (Chunk (Chunk'First .. Chunk'First + Step - 1), Set);
               Done := Done + Step;
            end;
         end loop;
      end if;
      return Result;
   end Count;



   procedure Find_Token (Source : in     Chunked_String;
                         Set    : in     Maps.Character_Set;
                         Test   : in     Ada.Strings.Membership;
                         First  :    out Positive;
                         Last   :    out Natural)
   is
      Invert : constant array (Ada.Strings.Membership)
        of Ada.Strings.Membership
        := (Ada.Strings.Inside  => Ada.Strings.Outside,
            Ada.Strings.Outside => Ada.Strings.Inside);

      N : Natural;
   begin
      N := Index (Source, Set, Test);

      if N = 0 then
         First := 1;
         Last := 0;
      else
         First := N;
         N := Index (Source, Set, First, Invert (Test));
         if N = 0 then
            Last := Source.Size;
         else
            Last := N - 1;
         end if;
      end if;
   end Find_Token;



   --  String translation subprograms

   function Translate (Source  : in Chunked_String;
                       Mapping : in Maps.Character_Mapping)
      return Chunked_String
   is
      Data : Chunk_Array_Access := null;
   begin
      if Source.Data /= null then
         Data := new Chunk_Array (Source.Data'Range);
         for J in Source.Data'Range loop
            Data (J) := new String (Source.Data (J).all'Range);
            Data (J).all := Fixed.Translate (Source.Data (J).all, Mapping);
         end loop;
      end if;
      return Chunked_String'(Ada.Finalization.Controlled with
                             Chunk_Size      => Source.Chunk_Size,
                             Allocation_Unit => Source.Allocation_Unit,
                             Size            => Source.Size,
                             Data            => Data);
   end Translate;



   procedure Translate (Source  : in out Chunked_String;
                        Mapping : in     Maps.Character_Mapping) is
   begin
      if Source.Data /= null then
         for J in Source.Data'Range loop
            Fixed.Translate (Source.Data (J).all, Mapping);
         end loop;
      end if;
   end Translate;



   function Translate (Source  : in Chunked_String;
                       Mapping : in Maps.Character_Mapping_Function)
      return Chunked_String
   is
      Data : Chunk_Array_Access := null;
   begin
      if Source.Data /= null then
         Data := new Chunk_Array (Source.Data'Range);
         for J in Source.Data'Range loop
            Data (J) := new String (Source.Data (J).all'Range);
            Data (J).all := Fixed.Translate (Source.Data (J).all, Mapping);
         end loop;
      end if;
      return Chunked_String'(Ada.Finalization.Controlled with
                             Chunk_Size      => Source.Chunk_Size,
                             Allocation_Unit => Source.Allocation_Unit,
                             Size            => Source.Size,
                             Data            => Data);
   end Translate;



   procedure Translate (Source  : in out Chunked_String;
                        Mapping : in     Maps.Character_Mapping_Function) is
   begin
      if Source.Data /= null then
         for J in Source.Data'Range loop
            Fixed.Translate (Source.Data (J).all, Mapping);
         end loop;
      end if;
   end Translate;



   --  String transformation subprograms

   function Replace_Slice (Source : in Chunked_String;
                           Low    : in Positive;
                           High   : in Natural;
                           By     : in String)
      return Chunked_String
   is
      Size : Natural := 0;
      Data : Chunk_Array_Access := null;
      Hi   : Natural := High;
   begin
      if Low > Source.Size + 1 then
         raise Ada.Strings.Index_Error;
      end if;

      if High < Low then
         Hi := Low - 1;
      end if;

      Size := (Low - 1) + By'Length + (Source.Size - Hi);
      Resize_Chunks (Data, Size, Source.Chunk_Size, Source.Allocation_Unit,
                     Can_Shrink => False);
      if Low > 1 then
         Move (Data.all, 1, Source.Data.all, 1, Low - 1);
      end if;
      if By'Length > 0 then
         Move (Data.all, By, Low, Source.Chunk_Size);
      end if;
      if Hi < Source.Size then
         Move (Data.all, Low + By'Length, Source.Data.all, Hi + 1,
               Source.Size - Hi);
      end if;

      return Chunked_String'(Ada.Finalization.Controlled with
                             Chunk_Size      => Source.Chunk_Size,
                             Allocation_Unit => Source.Allocation_Unit,
                             Size            => Size,
                             Data            => Data);
   end Replace_Slice;



   procedure Replace_Slice (Source : in out Chunked_String;
                            Low    : in     Positive;
                            High   : in     Natural;
                            By     : in     String)
   is
      Size : Natural := 0;
      Hi   : Natural := High;
   begin
      if Low > Source.Size + 1 then
         raise Ada.Strings.Index_Error;
      end if;

      if High < Low then
         Hi := Low - 1;
      end if;

      Size := (Low - 1) + By'Length + (Source.Size - Hi);
      Resize_Chunks (Source.Data, Size,
                     Source.Chunk_Size, Source.Allocation_Unit,
                     Can_Shrink => False);
      if Hi < Source.Size and Low + By'Length /= Hi + 1 then
         Move (Data            => Source.Data.all,
               Target_Position => Low + By'Length,
               Source_Position => Hi + 1,
               Length          => Source.Size - Hi,
               Chunk_Size      => Source.Chunk_Size);
      end if;
      if By'Length > 0 then
         Move (Source.Data.all, By, Low, Source.Chunk_Size);
      end if;
      Source.Size := Size;
   end Replace_Slice;



   function Insert (Source   : in Chunked_String;
                    Before   : in Positive;
                    New_Item : in String)
      return Chunked_String is
   begin
      return Replace_Slice (Source, Before, Before - 1, New_Item);
   end Insert;



   procedure Insert (Source   : in out Chunked_String;
                     Before   : in     Positive;
                     New_Item : in     String) is
   begin
      Replace_Slice (Source, Before, Before - 1, New_Item);
   end Insert;



   function Overwrite (Source   : in Chunked_String;
                       Position : in Positive;
                       New_Item : in String)
      return Chunked_String is
   begin
      return Replace_Slice (Source, Position, Source.Size, New_Item);
   end Overwrite;



   procedure Overwrite (Source   : in out Chunked_String;
                        Position : in     Positive;
                        New_Item : in     String) is
   begin
      Replace_Slice (Source,
                     Low  => Position,
                     High => Natural'Min (Source.Size,
                                          Position + New_Item'Length - 1),
                     By   => New_Item);
   end Overwrite;



   function Delete (Source  : in Chunked_String;
                    From    : in Positive;
                    Through : in Natural)
      return Chunked_String is
   begin
      if From <= Through then
         return Replace_Slice (Source, From, Through, "");
      else
         return Duplicate (Source);
      end if;
   end Delete;



   procedure Delete (Source  : in out Chunked_String;
                     From    : in     Positive;
                     Through : in     Natural) is
   begin
      if From <= Through then
         Replace_Slice (Source, From, Through, "");
      end if;
   end Delete;



   function Trim (Source : in Chunked_String;
                  Side   : in Ada.Strings.Trim_End)
      return Chunked_String is
   begin
      case Side is
         when Ada.Strings.Left =>
            return Trim (Source,
                         Maps.To_Set (Ada.Strings.Space),
                         Maps.Null_Set);
         when Ada.Strings.Right =>
            return Trim (Source,
                         Maps.Null_Set,
                         Maps.To_Set (Ada.Strings.Space));
         when Ada.Strings.Both =>
            return Trim (Source,
                         Maps.To_Set (Ada.Strings.Space),
                         Maps.To_Set (Ada.Strings.Space));
      end case;
   end Trim;



   procedure Trim (Source : in out Chunked_String;
                   Side   : in     Ada.Strings.Trim_End) is
   begin
      case Side is
         when Ada.Strings.Left =>
            Trim (Source,
                  Maps.To_Set (Ada.Strings.Space),
                  Maps.Null_Set);
         when Ada.Strings.Right =>
            Trim (Source,
                  Maps.Null_Set,
                  Maps.To_Set (Ada.Strings.Space));
         when Ada.Strings.Both =>
            Trim (Source,
                  Maps.To_Set (Ada.Strings.Space),
                  Maps.To_Set (Ada.Strings.Space));
      end case;
   end Trim;



   procedure Trim_Bounds (Source : in     Chunked_String;
                          Left   : in     Maps.Character_Set;
                          Right  : in     Maps.Character_Set;
                          Low    :    out Positive;
                          High   :    out Natural)
   is
      Chunk : Positive;
   begin
      Low  := 1;
      High := Source.Size;

      Chunk := 1;
      while Low <= High and then
         Maps.Is_In (Source.Data (Chunk).all
                                 (Low - (Chunk - 1) * Source.Chunk_Size),
                     Left)
      loop
         Low := Low + 1;
         if Low mod Source.Chunk_Size = 1 then
            Chunk := Chunk + 1;
         end if;
      end loop;

      if High > 0 then
         Chunk := (High - 1) / Source.Chunk_Size + 1;
         while Low <= High and then
            Maps.Is_In (Source.Data (Chunk).all
                                    (High - (Chunk - 1) * Source.Chunk_Size),
                        Right)
         loop
            High := High - 1;
            if High mod Source.Chunk_Size = 0 then
               Chunk := Chunk - 1;
            end if;
         end loop;
      end if;
   end Trim_Bounds;



   function Trim (Source : in Chunked_String;
                  Left   : in Maps.Character_Set;
                  Right  : in Maps.Character_Set)
      return Chunked_String
   is
      Low  : Positive;
      High : Natural;
   begin
      Trim_Bounds (Source, Left, Right, Low, High);
      return Chunked_Slice (Source, Low, High,
                            Source.Chunk_Size, Source.Allocation_Unit);
   end Trim;



   procedure Trim (Source : in out Chunked_String;
                   Left   : in     Maps.Character_Set;
                   Right  : in     Maps.Character_Set)
   is
      Low  : Positive;
      High : Natural;
   begin
      Trim_Bounds (Source, Left, Right, Low, High);
      if Low > 1 then
         Move (Data            => Source.Data.all,
               Target_Position => 1,
               Source_Position => Low,
               Length          => High - Low + 1,
               Chunk_Size      => Source.Chunk_Size);
      end if;
      Source.Size := High - Low + 1;
   end Trim;



   function Head (Source          : in Chunked_String;
                  Count           : in Natural;
                  Pad             : in Character := Ada.Strings.Space;
                  Chunk_Size      : in Natural := 0; -- use value from Source
                  Allocation_Unit : in Natural := 0) -- use value from Source
      return Chunked_String
   is
      Real_Chunk_Size : Positive := Default_Chunk_Size;
      Real_Unit : Positive := Default_Allocation_Unit;
      Data : Chunk_Array_Access := null;
   begin
      if Chunk_Size > 0 then
         Real_Chunk_Size := Chunk_Size;
      end if;
      if Allocation_Unit > 0 then
         Real_Unit := Allocation_Unit;
      end if;

      if Count > 0 then
         Resize_Chunks (Data, Count, Real_Chunk_Size, Real_Unit);
         if Count > Source.Size then
            Move (Data.all, 1, Source.Data.all, 1, Source.Size);
            Fill (Data.all, Source.Size + 1,
                  Count - Source.Size, Pad, Real_Chunk_Size);
         else
            Move (Data.all, 1, Source.Data.all, 1, Count);
         end if;
      end if;

      return Chunked_String'(Ada.Finalization.Controlled with
                             Chunk_Size      => Real_Chunk_Size,
                             Allocation_Unit => Real_Unit,
                             Size            => Count,
                             Data            => Data);
   end Head;



   procedure Head (Source : in out Chunked_String;
                   Count  : in     Natural;
                   Pad    : in     Character := Ada.Strings.Space) is
   begin
      if Count > Source.Size then
         Resize_Chunks (Source.Data, Count,
                        Source.Chunk_Size, Source.Allocation_Unit,
                        Can_Shrink => False);
         Fill (Source.Data.all, Source.Size + 1, Count - Source.Size, Pad,
               Source.Chunk_Size);
      end if;
      Source.Size := Count;
   end Head;



   function Tail (Source          : in Chunked_String;
                  Count           : in Natural;
                  Pad             : in Character := Ada.Strings.Space;
                  Chunk_Size      : in Natural := 0; -- use value from Source
                  Allocation_Unit : in Natural := 0) -- use value from Source
      return Chunked_String
   is
      Real_Chunk_Size : Positive := Default_Chunk_Size;
      Real_Unit : Positive := Default_Allocation_Unit;
      Data : Chunk_Array_Access := null;
   begin
      if Chunk_Size > 0 then
         Real_Chunk_Size := Chunk_Size;
      end if;
      if Allocation_Unit > 0 then
         Real_Unit := Allocation_Unit;
      end if;

      if Count > 0 then
         Resize_Chunks (Data, Count, Real_Chunk_Size, Real_Unit);
         if Count > Source.Size then
            Fill (Data.all, 1, Count - Source.Size, Pad, Real_Chunk_Size);
            Move (Data.all, Count - Source.Size + 1,
                  Source.Data.all, 1, Source.Size);
         else
            Move (Data.all, 1,
                  Source.Data.all, Source.Size - Count + 1, Count);
         end if;
      end if;

      return Chunked_String'(Ada.Finalization.Controlled with
                             Chunk_Size      => Real_Chunk_Size,
                             Allocation_Unit => Real_Unit,
                             Size            => Count,
                             Data            => Data);
   end Tail;



   procedure Tail (Source : in out Chunked_String;
                   Count  : in     Natural;
                   Pad    : in     Character := Ada.Strings.Space) is
   begin
      Resize_Chunks (Source.Data, Count,
                     Source.Chunk_Size, Source.Allocation_Unit,
                     Can_Shrink => False);
      if Count > Source.Size then
         if Source.Size > 0 then
            Move (Data            => Source.Data.all,
                  Target_Position => Count - Source.Size + 1,
                  Source_Position => 1,
                  Length          => Source.Size,
                  Chunk_Size      => Source.Chunk_Size);
         end if;
         Fill (Source.Data.all, 1, Count - Source.Size, Pad,
               Source.Chunk_Size);
      elsif Count > 0 then
         Move (Data            => Source.Data.all,
               Target_Position => 1,
               Source_Position => Source.Size - Count + 1,
               Length          => Count,
               Chunk_Size      => Source.Chunk_Size);
      end if;
      Source.Size := Count;
   end Tail;



   function "*" (Left  : in Natural;
                 Right : in Character)
      return Chunked_String
   is
      Chunk_Size : constant Positive := Default_Chunk_Size;
      Allocation_Unit : constant Positive := Default_Allocation_Unit;
      Size : constant Natural := Left;
      Chunk_Nb : constant Natural
        := Chunks_For (Size, Chunk_Size, Allocation_Unit);
      Last_Chunk_Size : constant Natural
        := Units_For (Size, Chunk_Size, Allocation_Unit) * Allocation_Unit;
      Data : Chunk_Array_Access := null;
   begin
      if Size > 0 then
         Data := new Chunk_Array (1 .. Chunk_Nb);
         for J in 1 .. Chunk_Nb - 1 loop
            Data (J) := new String'(Ada.Strings.Fixed."*" (Chunk_Size, Right));
         end loop;
         Data (Chunk_Nb) := new
           String'(Ada.Strings.Fixed."*" (Last_Chunk_Size, Right));
      end if;
      return Chunked_String'(Ada.Finalization.Controlled with
                             Chunk_Size      => Chunk_Size,
                             Allocation_Unit => Allocation_Unit,
                             Size            => Size,
                             Data            => Data);
   end "*";



   function "*" (Left  : in Natural;
                 Right : in String)
      return Chunked_String
   is
      Chunk_Size : constant Positive := Default_Chunk_Size;
      Allocation_Unit : constant Positive := Default_Allocation_Unit;
      Size : constant Natural := Left * Right'Length;
      Chunk_Nb : constant Natural
        := Chunks_For (Size, Chunk_Size, Allocation_Unit);
      Last_Chunk_Size : constant Natural
        := Units_For (Size, Chunk_Size, Allocation_Unit) * Allocation_Unit;
      Data : Chunk_Array_Access := null;
   begin
      if Size > 0 then
         if Chunk_Size mod Right'Length = 0 then
            Data := new Chunk_Array (1 .. Chunk_Nb);
            for J in 1 .. Chunk_Nb - 1 loop
               Data (J) := new String'(Ada.Strings.Fixed."*"
                                       (Chunk_Size / Right'Length, Right));
            end loop;
            Data (Chunk_Nb) := new String'(Ada.Strings.Fixed."*"
                                   (Last_Chunk_Size / Right'Length, Right));
         else
            Resize_Chunks (Data, Size, Chunk_Size, Allocation_Unit);
            for J in 1 .. Left loop
               Move (Data.all, Right, (J - 1) * Right'Length + 1, Chunk_Size);
            end loop;
         end if;
      end if;
      return Chunked_String'(Ada.Finalization.Controlled with
                             Chunk_Size      => Chunk_Size,
                             Allocation_Unit => Allocation_Unit,
                             Size            => Size,
                             Data            => Data);
   end "*";



   function "*" (Left  : in Natural;
                 Right : in Chunked_String)
      return Chunked_String
   is
      Chunk_Size : constant Positive := Default_Chunk_Size;
      Allocation_Unit : constant Positive := Default_Allocation_Unit;
      Size : constant Natural := Left * Right.Size;
      Data : Chunk_Array_Access := null;
   begin
      if Size > 0 then
         Resize_Chunks (Data, Size, Chunk_Size, Allocation_Unit);
         for J in 1 .. Left loop
            Move (Data.all, (J - 1) * Right.Size + 1,
                  Right.Data.all, 1, Right.Size);
         end loop;
      end if;
      return Chunked_String'(Ada.Finalization.Controlled with
                             Chunk_Size      => Chunk_Size,
                             Allocation_Unit => Allocation_Unit,
                             Size            => Size,
                             Data            => Data);
   end "*";



   --  Controlled object methods

   overriding procedure Initialize (Object : in out Chunked_String) is
   begin
      Object.Size := 0;
      Object.Data := null;
   end Initialize;



   overriding procedure Adjust (Object : in out Chunked_String) is
      New_Data : Chunk_Array_Access;
   begin
      if Object.Data /= null then
         New_Data := new Chunk_Array (Object.Data'Range);
         for J in Object.Data'Range loop
            New_Data (J) := new String'(Object.Data (J).all);
         end loop;
         Object.Data := New_Data;
      end if;
   end Adjust;



   overriding procedure Finalize (Object : in out Chunked_String) is
   begin
      Free (Object.Data);
   end Finalize;

end Natools.Chunked_Strings;
