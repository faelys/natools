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

with Ada.Characters.Latin_1;
with Ada.Unchecked_Deallocation;
with System.Machine_Code;

package body Natools.References is

   ---------------------------------
   -- Low-level memory management --
   ---------------------------------

   overriding procedure Adjust (Object : in out Immutable_Reference) is
   begin
      if Object.Count /= null then
         Increment (Object.Count);
      end if;
   end Adjust;


   overriding procedure Finalize (Object : in out Immutable_Reference) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Held_Data, Data_Access);
      procedure Free is
        new Ada.Unchecked_Deallocation (Counter, Counter_Access);

      Deallocate : Boolean;
   begin
      if Object.Count /= null then
         Decrement (Object.Count, Deallocate);

         if Deallocate then
            Free (Object.Count);
            Free (Object.Data);
         else
            Object.Count := null;
            Object.Data := null;
         end if;
      end if;
   end Finalize;



   -----------------------------------------
   -- Object construction and destruction --
   -----------------------------------------

   function Create
     (Constructor : not null access function return Held_Data)
      return Immutable_Reference is
   begin
      return (Ada.Finalization.Controlled with
         Data => new Held_Data'(Constructor.all),
         Count => new Counter'(1));
   end Create;


   procedure Replace
     (Ref : in out Immutable_Reference;
      Constructor : not null access function return Held_Data) is
   begin
      Finalize (Ref);
      Ref.Data := new Held_Data'(Constructor.all);
      Ref.Count := new Counter'(1);
   end Replace;


   function Create (Data : in Data_Access) return Immutable_Reference is
   begin
      if Data = null then
         return Null_Immutable_Reference;
      else
         return (Ada.Finalization.Controlled with
            Data => Data,
            Count => new Counter'(1));
      end if;
   end Create;


   procedure Replace
     (Ref : in out Immutable_Reference;
      Data : in Data_Access) is
   begin
      Finalize (Ref);

      if Data /= null then
         Ref.Data := Data;
         Ref.Count := new Counter'(1);
      end if;
   end Replace;


   procedure Reset (Ref : in out Immutable_Reference) is
   begin
      Finalize (Ref);
   end Reset;


   function Is_Empty (Ref : Immutable_Reference) return Boolean is
   begin
      return Ref.Count = null;
   end Is_Empty;


   function Is_Last (Ref : Immutable_Reference) return Boolean is
   begin
      return Ref.Count.all = 1;
   end Is_Last;


   function "=" (Left, Right : Immutable_Reference) return Boolean is
   begin
      return Left.Data = Right.Data;
   end "=";



   ----------------------
   -- Dereferenciation --
   ----------------------

   function Query (Ref : in Immutable_Reference) return Accessor is
   begin
      return Accessor'(Data => Ref.Data, Parent => Ref);
   end Query;


   function Update (Ref : in Reference) return Mutator is
   begin
      return Mutator'(Data => Ref.Data, Parent => Ref);
   end Update;


   procedure Query
     (Ref : in Immutable_Reference;
      Process : not null access procedure (Object : in Held_Data)) is
   begin
      Process.all (Ref.Data.all);
   end Query;


   procedure Update
     (Ref : in Reference;
      Process : not null access procedure (Object : in out Held_Data)) is
   begin
      Process.all (Ref.Data.all);
   end Update;



   ------------------------
   -- Counter Management --
   ------------------------

--   procedure Increment (Object : in Counter_Access) is
--   begin
--      Object.all := Object.all + 1;
--   end Increment;
--
--   procedure Decrement (Object : in Counter_Access; Zero : out Boolean) is
--   begin
--      Object.all := Object.all - 1;
--      Zero := Object.all = 0;
--   end Decrement;

   procedure Increment (Object : in Counter_Access) is
   begin
      System.Machine_Code.Asm
        (Template => "lock incl %0",
         Outputs => Counter'Asm_Output ("+m", Object.all),
         Volatile => True,
         Clobber => "cc, memory");
   end Increment;


   procedure Decrement (Object : in Counter_Access; Zero : out Boolean) is
      package Latin_1 renames Ada.Characters.Latin_1;
      use type Interfaces.Unsigned_8;
      Z_Flag : Interfaces.Unsigned_8;
   begin
      System.Machine_Code.Asm
        (Template => "lock decl %0" & Latin_1.LF & Latin_1.HT
                   & "setz %1",
         Outputs => (Counter'Asm_Output ("+m", Object.all),
                     Interfaces.Unsigned_8'Asm_Output ("=q", Z_Flag)),
         Volatile => True,
         Clobber => "cc, memory");
      Zero := Z_Flag = 1;
   end Decrement;

end Natools.References;

